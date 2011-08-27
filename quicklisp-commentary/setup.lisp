(defpackage #:ql-setup
  (:use #:cl)
  (:export #:*quicklisp-home*
           #:qmerge
           #:qenough))

(in-package #:ql-setup)

(unless *load-truename*			;setup.lisp被加载的包的路径，如果没有加载过任何包，则为空
  (error "This file must be LOADed to set up quicklisp."))

(defvar *quicklisp-home*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*)) ;默认的情况应该在~/quicklisp/目录下

(defun qmerge (pathname)
  "Return PATHNAME merged with the base Quicklisp directory."
  (merge-pathnames pathname *quicklisp-home*)) ;与quicklisp.lisp中的qmerge一样。

(defun qenough (pathname)		;好像没什么实际用途？
  (enough-namestring pathname *quicklisp-home*))

;;; ASDF is a hard requirement of quicklisp. Make sure it's either
;;; already loaded or load it from quicklisp's bundled version.

(defvar *required-asdf-version* "2.011")

;;; Put ASDF's fasls in a separate directory

(defun implementation-signature ()
  "Return a string suitable for discriminating different
implementations, or similar implementations with possibly-incompatible
FASLs."
  (let ((*print-pretty* nil))
    (format nil "lisp-implementation-type: ~A~%~
                 lisp-implementation-version: ~A~%~
                 machine-type: ~A~%~
                 machine-version: ~A~%"
            (lisp-implementation-type)	;这几个CL标准函数要记住！
            (lisp-implementation-version)
            (machine-type)
            (machine-version))))

(defun dumb-string-hash (string)
  "Produce a six-character hash of STRING."
  (let ((hash #xD13CCD13))
    (loop for char across string
          for value = (char-code char)
          do
          (setf hash (logand #xFFFFFFFF
                             (logxor (ash hash 5) ;为何这里用5和-27
                                     (ash hash -27)
                                     value))))
    (subseq (format nil "~(~36,6,'0R~)" (mod hash 88888901)) ;~(用来转换大小写，~R以指定进制数显示
            0 6)))

(defun asdf-fasl-pathname ()
  "Return a pathname suitable for storing the ASDF FASL, separated
from ASDF FASLs from incompatible implementations. Also, save a file
in the directory with the implementation signature, if it doesn't
already exist."
  (let* ((implementation-signature (implementation-signature))
         (original-fasl (compile-file-pathname (qmerge "asdf.lisp"))) ;compile-file-pathname为编译后的fasl默认存放路径，这里应该就是~/quicklisp/目录
         (fasl
          (qmerge (make-pathname	;为了区分不同实现编译出的fasl的差异，将fasl放在~/quicklisp/cache/asdf-fasls/<hash of signature>/目录下
                   :defaults original-fasl
                   :directory
                   (list :relative
                         "cache"
                         "asdf-fasls"
                         (dumb-string-hash implementation-signature)))))
         (signature-file (merge-pathnames "signature.txt" fasl)))
    (ensure-directories-exist fasl)
    (unless (probe-file signature-file)	;如果没有signature.txt，则将signature的内容写入signature.txt文件
      (with-open-file (stream signature-file :direction :output)
        (write-string implementation-signature stream)))
    fasl))				;返回编译后的fasl文件全路径名

(defun ensure-asdf-loaded ()
  "Try several methods to make sure that a sufficiently-new ASDF is
loaded: first try (require 'asdf), then loading the ASDF FASL, then
compiling asdf.lisp to a FASL and then loading it."
  (let* ((source (qmerge "asdf.lisp"))
         (fasl (asdf-fasl-pathname)))
    (ensure-directories-exist fasl)
    (labels ((asdf-symbol (name)	;寻找asdf包中的指定符号
               (let ((asdf-package (find-package '#:asdf)))
                 (when asdf-package
                   (find-symbol (string name) asdf-package))))
             (version-satisfies (version) ;判断asdf版本与quicklisp要求的版本是否匹配
               (let ((vs-fun (asdf-symbol '#:version-satisfies))
                     (vfun (asdf-symbol '#:asdf-version)))
                 (when (and vs-fun vfun
                            (fboundp vs-fun) ;如果找到符号，并且绑定到函数过
                            (fboundp vfun))
                   (funcall vs-fun (funcall vfun) version)))))
      (block nil			;block可以理解成允许return-from和return的progn
        (macrolet ((try (&body asdf-loading-forms)
                     `(progn
                        (handler-bind ((warning #'muffle-warning)) ;这个用法要记住，屏蔽warning！
                          (ignore-errors
                            ,@asdf-loading-forms)) ;运行加载代码，如果有warning也什么都不说。。。
                        (when (version-satisfies *required-asdf-version*) ;成功加载asdf后判断版本是否为要求的版本
                          (return t))))) ;这个地方的return在宏展开后，表明退出block
          (try)				;以下几个都是为了加载asdf，首先看看是否已经加载过了，加载过的直接判断版本，然后退出
          (try (require 'asdf))		;否则用require加载，然后退出
          (try (load fasl :verbose nil)) ;否则load，然后退出
          (try (load (compile-file source :verbose nil :output-file fasl))) ;否则先编译，然后load，然后退出
          (error "Could not load ASDF ~S or newer" *required-asdf-version*))))))

(ensure-asdf-loaded)

;;;
;;; Quicklisp sometimes must upgrade ASDF. Ugrading ASDF will blow
;;; away existing ASDF methods, so e.g. FASL recompilation :around
;;; methods would be lost. This config file will make it possible to
;;; ensure ASDF can be configured before loading Quicklisp itself via
;;; ASDF. Thanks to Nikodemus Siivola for pointing out this issue.
;;;

(let ((asdf-init (probe-file (qmerge "asdf-config/init.lisp")))) ;加载asdf config
  (when asdf-init
    (with-simple-restart (skip "Skip loading ~S" asdf-init)
      (load asdf-init :verbose nil :print nil))))

(push (qmerge "quicklisp/") asdf:*central-registry*) ;将~/quicklisp/quicklisp/目录加入asdf默认寻找.asd文件路径

(let ((*compile-print* nil)		;屏蔽所有默认的输出
      (*compile-verbose* nil)
      (*load-verbose* nil)
      (*load-print* nil))
  (asdf:oos 'asdf:load-op "quicklisp" :verbose nil)) ;加载quicklisp，它就位于~/quicklisp/quicklisp/目录下

(quicklisp:setup)			;运行quicklisp的设置
