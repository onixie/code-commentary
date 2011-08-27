;;;; client-update.lisp

(in-package #:quicklisp-client)

(defun version-from-file (file)
  (with-open-file (stream file)
    (let ((version-string (read-line stream)))
      (when (every #'digit-char-p version-string)
        (values (parse-integer version-string))))))

(defun local-version ()
  (version-from-file (qmerge "quicklisp/version.txt")))

(defun upstream-version ()
  (let ((local-file (qmerge "tmp/client-update/version.txt")))
    (ensure-directories-exist local-file)
    (fetch "http://beta.quicklisp.org/quickstart/version.txt" ;这是服务端的quicklisp-client的版本信息文件
           local-file :quietly t)
    (prog1 (version-from-file local-file)
      (delete-file local-file))))

(defun update-available-p ()
  (< (local-version) (upstream-version)))

(defun upstream-archive-url (version)	;这个是根据版本获得的quicklisp-client(也就是当前这些文件)的文件包
  (format nil "http://beta.quicklisp.org/quickstart/quicklisp-~D.tgz"
          version))

(defvar *upstream-asdf-url*
  "http://beta.quicklisp.org/quickstart/asdf.lisp")

(defvar *upstream-setup-url*
  "http://beta.quicklisp.org/quickstart/setup.lisp")

(defun retirement-directory (base)	;将旧的quicklisp-client文件放到retired/的目录下，编号从0开始作为其目录名的结尾
  (let ((suffix 0))
    (loop
      (incf suffix)
      (let* ((try (format nil "~A-~D" base suffix))
             (dir (qmerge (make-pathname :directory
                                         (list :relative "retired" try)))))
        (unless (probe-directory dir)	;直到找到一个不存在的目录名作为其目录名
          (return dir))))))

(defun update-client (&key (prompt t))
  (let ((upstream-version (upstream-version))
        (local-version (local-version)))
    (when (<= upstream-version local-version) ;为什么不用update-availbale-p呢？或许这样更简单快速些
      (format t "Installed version ~D is as new as upstream version ~D. No update.~%"
              local-version upstream-version)
      (return-from update-client t))
    (format t "Updating from version ~D to version ~D.~%"
            local-version upstream-version)
    (when prompt
      (unless (press-enter-to-continue)
        (return-from update-client nil)))
    (let* ((work-dir (qmerge (make-pathname ;真正的更新从这里开始。。。
                              :directory
                              (list :relative
                                    "tmp"
                                    "client-update"
                                    (princ-to-string upstream-version))))) ;下载的quicklisp-client文件包首先放到这里
           (upstream-archive (merge-pathnames "quicklisp.tgz" work-dir))
           (upstream-tar (merge-pathnames "quicklisp.tar" work-dir))
           (upstream-unpacked (merge-pathnames "quicklisp/" work-dir))
           (retired (retirement-directory (format nil "quicklisp-~D"
                                                  local-version))) ;旧的quicklisp-client内容放到这里
           (current-dir (qmerge "quicklisp/")))
      (ensure-directories-exist (qmerge "retired/"))
      (ensure-directories-exist upstream-archive)
      (fetch (upstream-archive-url upstream-version) upstream-archive) ;把新的取下来
      (gunzip upstream-archive upstream-tar)
      (unpack-tarball upstream-tar :directory work-dir) ;解压，解包
      (rename-directory current-dir retired)		;把旧的移走
      (rename-directory upstream-unpacked current-dir)	;把新的移进来
      ;; A little crude; should version these, too
      (fetch *upstream-setup-url* (qmerge "setup.lisp")) ;都下载下来
      (fetch *upstream-asdf-url* (qmerge "asdf.lisp"))
      (format t "~&New quicklisp client installed. ~
                   It will take effect on restart.~%") ;正如这句话说的！
      t)))