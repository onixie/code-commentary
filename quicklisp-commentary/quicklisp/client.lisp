;;;; client.lisp

(in-package #:quicklisp-client)		;最终的用户接口

(defvar *quickload-verbose* nil)
(defvar *quickload-prompt* nil)
(defvar *quickload-explain* t)

(define-condition system-not-quickloadable (error)
  ((system
    :initarg :system
    :reader not-quickloadable-system)))

(defgeneric quickload (systems &key verbose prompt explain &allow-other-keys) ;通过quicklisp加载某些系统
  (:documentation
   "Load SYSTEMS the quicklisp way. SYSTEMS is a designator for a list
   of things to be loaded.")
  (:method (systems &key prompt verbose &allow-other-keys)
    (unless (consp systems)		;如果只有一个，那么也把它变成list的样子
      (setf systems (list systems)))
    (dolist (thing systems systems)
      (flet ((ql ()
               (autoload-system-and-dependencies thing :prompt prompt))) ;调用setup.lisp中的那个自动加载系统及其依赖的函数
        (if verbose
            (ql)
            (call-with-quiet-compilation #'ql)))))) ;这个call-with...是为什么上面对autoload-system-and-dependencies使用ql最为其wrapper的原因

(defmethod quickload :around (systems &key verbose prompt explain ;这个around为什么不直接写进上面的quickload里去？
                                      &allow-other-keys)
  (declare (ignorable systems verbose prompt explain)) ;这里可以用ignore嘛！
  (with-consistent-dists			       ;保证dists在系统加载过程中不会变
    (call-next-method)))

(defun system-list ()
  (provided-systems t))

(defun update-dist (dist &key (prompt t))
  (when (stringp dist)
    (setf dist (find-dist dist)))	;如果是字串，就找出对应的对象
  (let ((new (available-update dist)))	;从dist对象的注册的网络下载的url去取新的dist文件，
					;放到tmp下的distinfo-update中，
					;然后比较新的dist对象与老的对象的版本，以判断新的dist是否可用
    (cond (new
           (show-update-report dist new) ;根据新旧的dist内容，显示从旧版更新到新版，都增加了哪些release，更新了哪些，删除了哪些
           (when (or (not prompt) (press-enter-to-continue))
             (update-in-place dist new))) ;更新旧的系统为新的，并且更新所有的release, system文件，更新已经安装的release
          ((not (subscribedp dist))	  ;如果没有订阅这个dist，报错
           (format t "~&You are not subscribed to ~S."
                   (name dist)))
          (t				;其他情况下，不需要更新
           (format t "~&You already have the latest version of ~S: ~A.~%"
                   (name dist)
                   (version dist))))))

(defun update-all-dists (&key (prompt t))
  (let ((dists (remove-if-not 'subscribedp (all-dists)))) ;对所有订阅的dist进行更新
    (format t "~&~D dist~:P to check.~%" (length dists))
    (dolist (old dists)
      (with-simple-restart (skip "Skip update of dist ~S" (name old))
        (update-dist old :prompt prompt)))))

(defun help ()
  "For help with Quicklisp, see http://www.quicklisp.org/beta/")

(defun uninstall (system-name)		;删除一个安装的系统
  (let ((system (find-system system-name)))
    (when system
      (ql-dist:uninstall system))))

(defun uninstall-dist (name)		;删除一个安装的dist
  (let ((dist (find-dist name)))
    (when dist
      (ql-dist:uninstall dist))))

(defun write-asdf-manifest-file (output-file
                                   &key (if-exists :rename-and-delete))
  "Write a list of system file pathnames to OUTPUT-FILE, one per line,
in order of descending QL-DIST:PREFERENCE."
  (with-open-file (stream output-file
                          :direction :output
                          :if-exists if-exists)
    (with-consistent-dists
      (let ((systems (provided-systems t)))
        (dolist (system (sort systems #'>
                              :key #'preference))
          (let ((system-file (find-asdf-system-file (name system))))
            (when system-file
              (format stream "~A~%"
                      (native-namestring system-file))))))))
  (probe-file output-file))
