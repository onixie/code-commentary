;;;; config.lisp

(in-package #:ql-config)

(defun config-value-file-pathname (path);配置文件路径
  (let ((bad-position (position #\Space path))) ;不应该包含空格,下面对空格有处理.
    (when bad-position
      (error "Space not allowed at position ~D in ~S"
             bad-position
             path)))
  (let* ((space-path (substitute #\Space #\/ path)) ;首先把路径中的 /替换为空格
         (split (split-spaces space-path))	    ;用空格分隔字串
         (directory-parts (butlast split))	    ;除了最后一部分外的就是 目录的部分
         (name (first (last split)))		    ;最后是文件 名称
         (base (qmerge "config/")))
     (merge-pathnames
      (make-pathname :name name		;文件名和相对路径
                     :type "txt"
                     :directory (list* :relative directory-parts)) ;这理list*的用法很好!
      base)))

(defun config-value (path)
  (let ((file (config-value-file-pathname path)))
    (with-open-file (stream file :if-does-not-exist nil)
      (when stream
        (values (read-line stream nil))))))

(defun (setf config-value) (new-value path)
  (let ((file (config-value-file-pathname path)))
    (ensure-directories-exist file)	;保证文件路径是存在的.
    (with-open-file (stream file :direction :output
                            :if-does-not-exist :create
                            :if-exists :rename-and-delete)
      (write-line new-value stream))))
