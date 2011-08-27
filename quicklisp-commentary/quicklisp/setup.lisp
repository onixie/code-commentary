(in-package #:quicklisp)

(defun show-wrapped-list (words &key (indent 4) (margin 60))
 (let ((*print-right-margin* margin)
       (*print-pretty* t)
       (*print-escape* nil)
       (prefix (make-string indent :initial-element #\Space)))
   (pprint-logical-block (nil words :per-line-prefix prefix) ;用于格式化输出
     (pprint-fill *standard-output* (sort (copy-seq words) #'string<) nil))
   (fresh-line)
   (finish-output)))

(defun recursively-install (name)
  (labels ((recurse (name)
             (let ((system (find-system name))) ;从所有enabled的dist中，寻找第一个名称为name的系统
               (unless system
                 (error "Unknown system ~S" name))
               (ensure-installed system) ;首先查看系统的asdf文件是否存在，如果不存在就安装该系统，
					;安装的时候，先查看release中的每个提供的系统是否都存在，
					;如果有不存在的，就重新安装整个release。
					;安装release包括下载release包,解压到software目录,记录每个系统到release的meta文件中
					;以及记录每个系统到系统的meta文件中
               (mapcar #'recurse (required-systems system)) ;对该系统依赖的其他系统进行安装
               name)))
    (with-consistent-dists		;非常特别的设计! 固定住*dist-enumeration-functions*中包含的函数,使其返回的值一直保持该调用时的内容,以保证其body中的内容引用的dists不发生变化
      (recurse name))))

(defclass load-strategy ()		;相当于在quickload的时候的系统名称
  ((name
    :initarg :name
    :accessor name)
   (asdf-systems			;需要加载的asdf系统,依赖
    :initarg :asdf-systems
    :accessor asdf-systems)
   (quicklisp-systems			;需要通过quicklisp下载的 系统, 依赖
    :initarg :quicklisp-systems
    :accessor quicklisp-systems)))

(defmethod print-object ((strategy load-strategy) stream) ;显示依赖的系统的个数,包括本地已经有的,和需要下载的
  (print-unreadable-object (strategy stream :type t)
    (format stream "~S (~D asdf, ~D quicklisp)"
            (name strategy)
            (length (asdf-systems strategy))
            (length (quicklisp-systems strategy)))))

(defgeneric quicklisp-releases (strategy) ;需要下载的那些系统的release
  (:method (strategy)
    (remove-duplicates (mapcar 'release (quicklisp-systems strategy)))))

(defgeneric quicklisp-release-table (strategy) ;放在hashtable中
  (:method ((strategy load-strategy))
    (let ((table (make-hash-table)))
      (dolist (system (quicklisp-systems strategy))
        (push system (gethash (release system) table nil)))
      table)))

(define-condition system-not-found (error)
  ((name
    :initarg :name
    :reader system-not-found-name))
  (:report (lambda (condition stream)
             (format stream "System ~S not found"
                     (system-not-found-name condition)))))

(defun compute-load-strategy (name)
  (setf name (string-downcase name))
  (let ((asdf-systems '())
        (quicklisp-systems '()))
    (labels ((recurse (name)
               (let ((asdf-system (asdf:find-system name nil)) ;通过asdf找到的系统,表明在本地
                     (quicklisp-system (find-system name)))    ;通过quicklisp找到的系统,表明是quicklisp支持的
                 (cond (asdf-system			       ;如果asdf先找到,那么就表明本地存在,直接调用即可
                        (push asdf-system asdf-systems))
                       (quicklisp-system ;否则表明需要下载
                        (push quicklisp-system quicklisp-systems)
                        (dolist (subname (required-systems quicklisp-system)) ;循环寻找那些依赖的系统, 为什么asdf的情况下不需要呢? 
                          (recurse subname)))
                       (t
                        (error 'system-not-found
                               :name name))))))
      (with-consistent-dists
        (recurse name)))		;通过此举,就能直到到底要加载那些系统,下载那些release了
    (make-instance 'load-strategy
                   :name name
                   :asdf-systems (remove-duplicates asdf-systems)
                   :quicklisp-systems (remove-duplicates quicklisp-systems))))

(defun show-load-strategy (strategy)	;显示要加载的本地系统,以及要下载的系统的release
  (format t "To load ~S:~%" (name strategy))
  (let ((asdf-systems (asdf-systems strategy))
        (releases (quicklisp-releases strategy)))
    (when asdf-systems
      (format t "  Load ~D ASDF system~:P:~%" (length asdf-systems))
      (show-wrapped-list (mapcar 'asdf:component-name asdf-systems)))
    (when releases
      (format t "  Install ~D Quicklisp release~:P:~%" (length releases))
      (show-wrapped-list (mapcar 'name releases)))))

(defvar *macroexpand-progress-in-progress* nil)

(defun macroexpand-progress-fun (old-hook &key (char #\.) ;返回一个新的宏展开的hook, 在宏展开的同时,显示进度
                                 (chars-per-line 50)	  ;每行显示50个字符最多
                                 (forms-per-char 250))	  ;展开250个宏显示一个字符
  (let ((output-so-far 0)				  ;当前一行已经显示的字符个数
        (seen-so-far 0))				  ;已经展开的宏的个数
    (labels ((finish-line ()				  ;用字符填充一行剩余的部分
               (when (plusp output-so-far)		  ;如果没有输出过,这一行就不用填充了
                 (dotimes (i (- chars-per-line output-so-far))
                   (write-char char))
                 (terpri)
                 (setf output-so-far 0)))
             (show-string (string)
               (let* ((length (length string))
                      (new-output (+ length output-so-far)))
                 (cond ((< chars-per-line new-output) ;如果字符串显示的长度超过该行的结尾,那么重新还一行在显示
                        (finish-line)
                        (write-string string)
                        (setf output-so-far length)) ;换行后剩余的字符
                       (t
                        (write-string string) ;如果能显示的下,那么不还行就直接显示
                        (setf output-so-far new-output))))
               (finish-output))
             (show-package (name)
               ;; Only show package markers when compiling. Showing
               ;; them when loading shows a bunch of ASDF system
               ;; package noise.
               (when *compile-file-pathname* ;输出编译过程中的信息
                 (finish-line)
                 (show-string (format nil "[package ~(~A~)]" name)))))
      (lambda (fun form env)		;新的宏展开的hook
        (when (and (consp form)
                   (eq (first form) 'cl:defpackage) ;显示包名! 如果是一个defpackage宏,并且第二个是包的名称,那么就显示这个信息. 
		   (ignore-errors (string (second form))))
	  (show-package (second form)));这招很灵,毕竟所有的系统都需要defpackage,所以通过它表明当前处理的是什么package
        (incf seen-so-far)		;增加当前展开的宏的个数
        (when (<= forms-per-char seen-so-far) ;如果展开的宏个数到达要求的时候,就显示一个字符,表明当前已经展开了这些宏
          (setf seen-so-far 0)
          (write-char char)
          (finish-output)
          (incf output-so-far)
          (when (<= chars-per-line output-so-far) ;超过一行了,也是需要换行的
            (setf output-so-far 0)
            (terpri)
            (finish-output)))
        (funcall old-hook fun form env))))) ;调用真正的hook函数

(defun call-with-macroexpand-progress (fun)
  (let ((*macroexpand-hook* (if *macroexpand-progress-in-progress* ;如果不在宏展开进度中, 那么就调用新的hook,显示展开的进度
                                *macroexpand-hook*
                                (macroexpand-progress-fun *macroexpand-hook*)))
        (*macroexpand-progress-in-progress* t)) ;表明已经在宏展开的进度显示中, 排他处理!
    (funcall fun)
    (terpri)))

(defun apply-load-strategy (strategy)
  (map nil 'ensure-installed (quicklisp-releases strategy)) ;保证需要的release都下载下来
  (call-with-macroexpand-progress			    ;然后通过asdf加载strategy, 通过autoload-system-and-dependencies会自动加载他需要的那些系统
   (lambda ()
     (format t "~&; Loading ~S~%" (name strategy))
     (asdf:oos 'asdf:load-op (name strategy) :verbose nil))))

(defun autoload-system-and-dependencies (name &key prompt) ;犀利的递归, restart处理, 局部跳转, 一应俱全!
  (setf name (string-downcase name))	;要加载的系统名称
  (with-simple-restart (abort "Give up on ~S" name) ;如果出错了,可以放弃
    (let ((strategy (compute-load-strategy name)))  ;计算所有需要加载的依赖系统,以及需要下载的那些
      (show-load-strategy strategy)		    ;显示这些信息
      (when (or (not prompt)			    
                (press-enter-to-continue))
        (tagbody
         retry
         (handler-bind
             ((asdf:missing-dependency	;如果出现没有加载依赖包的错误(需求)
               (lambda (c)		;那么处理该需求,加载那些需要的包.这个就是自动的,加载包及其依赖
                 (let ((parent (asdf::missing-required-by c))
                       (missing (asdf::missing-requires c)))
                   (when (typep parent 'asdf:system)
                     (autoload-system-and-dependencies missing ;对依赖包继续调用,以保证其上层的依赖也被这样自动处理
                                                       :prompt prompt)
                     (go retry))))))	;通过tagbody跳转,重新加载一次,在所有依赖都被加载后,重新加载将会成功
           (apply-load-strategy strategy))))) ;自动加载这一切的开始
    name))

(defvar *initial-dist-url*		;发布的位置,我很想建镜像!
  "http://beta.quicklisp.org/dist/quicklisp.txt")

(defun maybe-initial-setup ()		;如果该setup是quicklisp-quickstart调用的，那么就是第一次安装quicklisp
  ;; Is this running under the quicklisp bootstrap?
  (let ((bootstrap-package (find-package 'quicklisp-quickstart)))
    (when bootstrap-package
      (let* ((proxy (find-symbol (string '#:*proxy-url*) bootstrap-package)) ;将quicklisp-quickstart设定的代理重新设定到quicklisp上
             (proxy-value (and proxy (symbol-value proxy))))
        (when (and proxy-value (not *proxy-url*))
          (setf *proxy-url* proxy-value)
          (setf (config-value "proxy-url") proxy-value)))))
  (unless (ignore-errors (truename (qmerge "dists/"))) ;如果dist文件不存在，就下载一个dist文件，并启用它
    (let ((target (qmerge "dists/quicklisp/distinfo.txt")))
      (ensure-directories-exist target)
      (fetch *initial-dist-url* target)
      (enable (find-dist "quicklisp"))))) ;创建一个quicklisp的dist对象

(defun setup ()
  (unless (member 'system-definition-searcher
                  asdf:*system-definition-search-functions*)
    (setf asdf:*system-definition-search-functions*
          (append asdf:*system-definition-search-functions*
                  (list 'system-definition-searcher)))) ;将quicklisp自定义的.asd文件搜索函数注册到asdf中
  (let ((files (nconc (directory (qmerge "local-setup/*.lisp")) ;一些配置文件的加载，一般情况下应该没什么用
                      (directory (qmerge "local-setup/*.cl")))))
    (with-simple-restart (abort "Stop loading local setup files")
      (dolist (file (sort files #'string< :key #'pathname-name))
        (with-simple-restart (skip "Skip local setup file ~S" file)
          ;; Don't try to load Emacs lock files, other hidden files
          (unless (char= (char (pathname-name file) 0)
                         #\.)
            (load file))))))
  (maybe-initial-setup)			;下载dist文件，初始化必要的目录，创建一个dist对象
  (pushnew :quicklisp *features*)	;表明quicklisp已经启用
  t)
