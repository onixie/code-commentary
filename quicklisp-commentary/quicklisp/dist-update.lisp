;;;; dist-update.lisp

(in-package #:ql-dist)

(defgeneric available-update (dist)
  (:documentation "If an update is available for DIST, return the
  update as an uninstalled dist object. Otherwise, return NIL."))

(defgeneric update-release-differences (old-dist new-dist)
  (:documentation "Compare OLD-DIST to NEW-DIST and return three lists
  as multiple values: new releases \(present in NEW-DIST but not
  OLD-DIST), changed releases \(present in both dists but different in
  some way), and removed releases \(present in OLD-DIST but not
  NEW-DIST). The list of changed releases is a list of two-element
  lists, with each two-element list having first the old release
  object and then the new release object."))

(defgeneric show-update-report (old-dist new-dist)
  (:documentation "Display a description of the update from OLD-DIST
  to NEW-DIST."))

(defgeneric update-in-place (old-dist new-dist)
  (:documentation "Update OLD-DIST to NEW-DIST in place."))

(defmethod available-update ((dist dist))
  (let ((url (distinfo-subscription-url dist)) ;网络上的地址
        (target (qmerge "tmp/distinfo-update/distinfo.txt"))) ;下载后先放到本地的地址
    (when url
      (ensure-directories-exist target)
      (fetch url target :quietly t)
      (let ((new (make-dist-from-file target))) ;下载的新的dist创建一个对象
        (setf (base-directory new)
              (make-pathname :name nil
                             :type nil
                             :version nil
                             :defaults target))
        (when (and (string= (name dist) (name new))
                   (string/= (version dist) (version new))) ;比较新老的版本号
          new)))))

(defmethod update-release-differences ((old-dist dist)
                                       (new-dist dist))
  (let ((old-releases (provided-releases old-dist)) ;dist中包含的release
        (new-releases (provided-releases new-dist))
        (new '())
        (updated '())
        (removed '())
        (old-by-name (make-hash-table :test 'equalp)))
    (dolist (release old-releases)	;把所有旧dist的release都保存到hash表中
      (setf (gethash (name release) old-by-name)
            release))
    (dolist (new-release new-releases)
      (let* ((name (name new-release))
             (old-release (gethash name old-by-name))) ;用新的dist中的每个release去找旧的dist的release的hash表
        (remhash name old-by-name)		       ;不管有没有都移除，剩下的就是新的里没有的，而旧的里有的
        (cond ((not old-release)		       ;如果过去没有，那表明是全新的release
               (push new-release new))
              ((not (equal (archive-content-sha1 new-release) ;如果sha1不同，则表明是更新的release
                           (archive-content-sha1 old-release)))
               (push (list old-release new-release) updated)))))
    (maphash (lambda (name old-release)
               (declare (ignore name))
               (push old-release removed))
             old-by-name)		;剩下的这些应该是新的dist里没有的
    (values (nreverse  new)
            (nreverse  updated)
            (sort removed #'string< :key #'prefix))))

(defmethod show-update-report ((old-dist dist) (new-dist dist))
  (multiple-value-bind (new updated removed) ;多值返回和mvb真的是很好用！
      (update-release-differences old-dist new-dist)
    (format t "Changes from ~A ~A to ~A ~A:~%"
            (name old-dist)
            (version old-dist)
            (name new-dist)
            (version new-dist))
    (when new
      (format t "~&  New projects:~%")
      (format t "~{    ~A~%~}" (mapcar #'prefix new))) ;完全不用写循环，多方便呀！
    (when updated
      (format t "~%  Updated projects:~%") ;换行还是不还行，非常方便呀！
      (loop for (old-release new-release) in updated ;loop多强大，太方便了！
            do (format t "    ~A -> ~A~%"
                       (prefix old-release)
                       (prefix new-release))))
    (when removed
      (format t "~%  Removed projects:~%")
      (format t "~{    ~A~%~}" (mapcar #'prefix removed)))))

(defun clear-dist-systems (dist)
  (dolist (system (provided-systems dist))
    (asdf:clear-system (name system))))	;通过asdf卸载掉系统，以保证更新的系统可以被加载

(defmethod update-in-place :before ((old-dist dist) (new-dist dist)) ;通用函数的before, after已经超越了基本OO, 这是不是面向侧面编程？
  ;; Make sure ASDF will reload any systems at their new locations
  (clear-dist-systems old-dist))

(defmethod update-in-place :after ((old-dist dist) (new-dist dist))
  (clean new-dist))			;已经不需要的目录，归档文件都在这里删除

(defmethod update-in-place ((old-dist dist) (new-dist dist)) ;更新dist的真正的函数
  (flet ((remove-installed (type)			     ;删除安装的系统或releasae的meta信息文件
           (let ((wild (merge-pathnames (make-pathname :directory
                                                       (list :relative
                                                             "installed"
                                                             type)
                                                       :name :wild
                                                       :type "txt")
                                        (base-directory old-dist))))
             (dolist (file (directory wild)) ;这个directory非常具有迷惑性，其实他返回的是通配符匹配的所有文件的一个列表！
               (delete-file file))))) 
    (let ((reinstall-releases (installed-releases old-dist)))
      (remove-installed "systems")  	;meta信息被删除
      (remove-installed "releases")
      (delete-file-if-exists (relative-to old-dist "releases.txt")) ;release的index文件被删除
      (delete-file-if-exists (relative-to old-dist "systems.txt"))  ;system的index文件被删除
      ;; 这里没有去下载release和系统的index文件，是因为使用了slot-unbound方法，将其下载，初始化的动作绑定在了引用这两个slot的时候
      (replace-file (local-distinfo-file new-dist)
                    (local-distinfo-file old-dist)) ;替换成新的dist的文件
      (setf new-dist (find-dist (name new-dist)))   ;创建新的dist的对象
      (dolist (old-release reinstall-releases)	    ;对于已经安装的release, 重新安装那些在新的dist中存在的
        (let* ((name (name old-release))
               (new-release (find-release-in-dist name new-dist)))
          (if new-release
              (ensure-installed new-release)
              (warn "~S is not available in ~A" name new-dist)))))))

(defun install-dist (url &key (prompt t) replace)
  (block nil
    (setf url (url url))
    (let ((temp-file (qmerge "tmp/install-dist-distinfo.txt")))
      (ensure-directories-exist temp-file)
      (delete-file-if-exists temp-file)
      (fetch url temp-file)
      (let* ((new-dist (make-dist-from-file temp-file))
             (old-dist (find-dist (name new-dist))))
        (when old-dist
          (if replace			;如果replace为t，表明用户想要直接replace, 如果为nil，那么被动的也必须要replace
              (uninstall old-dist)
              (restart-case
                  (error "A dist named ~S is already installed." ;先error，然后就直接replace了
                         (name new-dist))
                (replace ()
                  :report "Replace installed dist with new dist"
                  (uninstall old-dist)))))
        (format t "Installing dist ~S version ~S.~%"
                (name new-dist)
                (version new-dist))
        (when (or (not prompt)
                  (press-enter-to-continue))
          (ensure-directories-exist (base-directory new-dist))
          (copy-file temp-file (relative-to new-dist "distinfo.txt")) ;将临时的新dist变成正式的
          (ensure-release-index-file new-dist) ;release的index文件也下载下来
          (ensure-system-index-file new-dist)  ;同上
          (enable new-dist)		       ;enable就是创建一个enable.txt文件？
          (when old-dist		       ;这回不管是新的旧的，全都清空了。并且不载入安装的系统
            (clear-dist-systems old-dist))
          (clear-dist-systems new-dist)
          new-dist)))))