;;;;
;;;; This is quicklisp.lisp, the quickstart file for Quicklisp. To use
;;;; it, start Lisp, then (load "quicklisp.lisp")
;;;;
;;;; Quicklisp is beta software and comes with no warranty of any kind.
;;;;
;;;; For more information about the Quicklisp beta, see:
;;;;
;;;;    http://www.quicklisp.org/beta/
;;;;
;;;; If you have any questions or comments about Quicklisp, please
;;;; contact:
;;;;
;;;;    Zach Beane <zach@quicklisp.org>
;;;;

(cl:in-package #:cl-user)
(cl:defpackage #:qlqs-user
  (:use #:cl))
(cl:in-package #:qlqs-user) ;定义quicklisp-quickstart用户包，不污染cl-user包中的符号。

(defpackage #:qlqs-impl     ;定义与各种CL实现相关代码的包，为了兼容性考虑
  (:use #:cl)
  (:export #:*implementation*) ;当前quicklisp-quickstart运行的CL实现
  (:export #:definterface      ;宏，用来定义与特定实现相关的generic function
           #:defimplementation);宏, 用来定义与特定实现相关的method function
  (:export #:lisp
           #:abcl
           #:allegro
           #:ccl
           #:clisp
           #:cmucl
           #:cormanlisp
           #:ecl
           #:gcl
           #:lispworks
           #:scl
           #:sbcl))

(defpackage #:qlqs-impl-util ;辅助工具包
  (:use #:cl #:qlqs-impl)
  (:export #:call-with-quiet-compilation)) ;屏蔽编译产生的垃圾信息

(defpackage #:qlqs-network   ;网络连接以及读写代码相关的包
  (:use #:cl #:qlqs-impl)
  (:export #:open-connection
           #:write-octets
           #:read-octets
           #:close-connection
           #:with-connection)) ;宏,网络通信的with实现

(defpackage #:qlqs-progress  ;字符风格进度条
  (:use #:cl)
  (:export #:make-progress-bar
           #:start-display
           #:update-progress
           #:finish-display))

(defpackage #:qlqs-http
  (:use #:cl #:qlqs-network #:qlqs-progress)
  (:export #:fetch
           #:*proxy-url*
           #:*maximum-redirects*
           #:*default-url-defaults*))

(defpackage #:qlqs-minitar
  (:use #:cl)
  (:export #:tarball-contents
           #:unpack-tarball))

(defpackage #:quicklisp-quickstart   ;将提供quicklisp-quickstart功能的主包,包含其他各个分散功能包
  (:use #:cl #:qlqs-impl #:qlqs-impl-util #:qlqs-http #:qlqs-minitar)
  (:export #:install
           #:*proxy-url*
           #:*asdf-url*
           #:*quicklisp-tar-url*
           #:*setup-url*
           #:*after-load-message*
           #:*after-initial-setup-message*))


;;;
;;; Defining implementation-specific packages and functionality
;;;

(in-package #:qlqs-impl)

(eval-when (:compile-toplevel :load-toplevel :execute)   ;被宏引用的函数,需要在编译期eval, 因此,此处需要eval-when :compile-toplevel
  (defun error-unimplemented (&rest args)                ;对于未支持的CL实现, 产生error
    (declare (ignore args))
    (error "Not implemented")))

(defmacro neuter-package (name)                          ;当使用不支持的CL实现时,对该CL实现对应的符号的函数调用等同于上面的error-unimplemented调用
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((definition (fdefinition 'error-unimplemented)))
       (do-external-symbols (symbol ,(string name))
         (unless (fboundp symbol)
           (setf (fdefinition symbol) definition))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun feature-expression-passes-p (expression)        ;检查参数是否代表某个*features*中的内容
    (cond ((keywordp expression)                         ;如果是单个元素时
           (member expression *features*))
          ((consp expression)                            ;如果是cons时
           (case (first expression)                      
             (or
              (some 'feature-expression-passes-p (rest expression)))   
             (and
              (every 'feature-expression-passes-p (rest expression)))))
          (t (error "Unrecognized feature expression -- ~S" expression)))))   ;其他情况认为不可能在*features*中出现.


(defmacro define-implementation-package (feature package-name &rest options)  ;该宏定义qlqs支持的CL实现, 生成一个代表特定CL实现的类, 以及导出其部分函数的包.
  (let* ((output-options '((:use)
                           (:export #:lisp)))
         (prep (cdr (assoc :prep options)))
         (class-option (cdr (assoc :class options)))
         (class (first class-option))
         (superclasses (rest class-option))
         (import-options '())
         (effectivep (feature-expression-passes-p feature)))     ;若当前查看定义的CL实现与当前运行的CL实现的对应关系
    (dolist (option options)
      (ecase (first option)
        ((:prep :class))                                         ;在let*中被处理
        ((:import-from                                           ;明确指定的引用
          :import)
         (push option import-options))
        ((:export                                                ;明确指定的其他包选项
          :shadow
          :intern
          :documentation)
         (push option output-options))
        ((:reexport-from)                                        ;引用特定包,并将部分函数导出.
         (push (cons :export (cddr option)) output-options)
         (push (cons :import-from (cdr option)) import-options))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(when effectivep                                        ;如果是当前运行的CL,则调用预先定义的处理函数,预先载入某些需要的包
               prep)     
       (defclass ,class ,superclasses ())                        ;定义代表特定CL实现的类
       (defpackage ,package-name ,@output-options                ;定义代表特定CL实现的包,到出与实现对应的部分函数.
                   ,@(when effectivep
                           import-options))
       ,@(when effectivep
               `((setf *implementation* (make-instance ',class))))   ;设置当前的CL
       ,@(unless effectivep
                 `((neuter-package ,package-name))))))               ;如果不支持,则设置CL名称为不支持的

(defmacro definterface (name lambda-list &body options)                             ;定义针对特定CL的接口,其中主要定义一个generic function以及一个该函数的wrapper
  (let* ((forbidden (intersection lambda-list lambda-list-keywords))                ;如果该宏的lambda-list中有标准的lambda-list中的那些关键字, 则不允许其定义
         (gf-options (remove :implementation options :key #'first))                 ;除了:implementaion 关键字的选项
         (implementations (set-difference options gf-options)))                     ;:implementation关键字的选项
    (when forbidden
      (error "~S not allowed in definterface lambda list" forbidden))
    (flet ((method-option (class body)                               ;在generic function中直接声明的method体部分, 针对class(即特定CL实现的类)声明不同的method
             `(:method ((*implementation* ,class) ,@lambda-list)
                ,@body)))
      (let ((generic-name (intern (format nil "%~A" name))))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defgeneric ,generic-name (lisp ,@lambda-list)            ;定义generic function
             ,@gf-options
             ,@(mapcar (lambda (implementation)
                         (destructuring-bind (class &rest body)      ;针对:implementation关键字, 生成method体部分
                             (rest implementation)
                           (method-option class body)))
                       implementations))
           (defun ,name ,lambda-list                                 ;定义一个wrapper, 调用当前运行的CL实现对应的generic function
             (,generic-name *implementation* ,@lambda-list)))))))

(defmacro defimplementation (name-and-options                        ;定义generic function对应的method function.
                             lambda-list &body body)
  (destructuring-bind (name &key (for t) qualifier)
      (if (consp name-and-options)                                   ;如果name-and-options只有name, 那么转换成列表形式
          name-and-options
          (list name-and-options))
    (unless for
      (error "You must specify an implementation name."))
    (let ((generic-name (find-symbol (format nil "%~A" name))))
      (unless (and generic-name                                      ;看看有没有对应的generic function
                   (fboundp generic-name))
        (error "~S does not name an implementation function" name))
      `(defmethod ,generic-name                                      ;定义一个针对特定CL实现的generic function的method function
           ,@(when qualifier (list qualifier))
         ,(list* `(*implementation* ,for) lambda-list) ,@body))))


;;; Bootstrap implementations

(defvar *implementation* nil)                                        ;默认的初始化
(defclass lisp () ())


;;; Allegro Common Lisp

(define-implementation-package :allegro #:qlqs-allegro               ;对allegro CL的支持
  (:documentation
   "Allegro Common Lisp - http://www.franz.com/products/allegrocl/")
  (:class allegro)                                                   ;代表该CL的类
  (:reexport-from #:socket                                           ;引入socket包,并导出make-socket
                  #:make-socket)
  (:reexport-from #:excl
                  #:read-vector))


;;; Armed Bear Common Lisp

(define-implementation-package :abcl #:qlqs-abcl                     
  (:documentation
   "Armed Bear Common Lisp - http://common-lisp.net/project/armedbear/")
  (:class abcl)
  (:reexport-from #:system
                  #:make-socket
                  #:get-socket-stream))

;;; Clozure CL

(define-implementation-package :ccl #:qlqs-ccl
  (:documentation
   "Clozure Common Lisp - http://www.clozure.com/clozurecl.html")
  (:class ccl)
  (:reexport-from #:ccl
                  #:make-socket))

;;; GNU CLISP

(define-implementation-package :clisp #:qlqs-clisp
  (:documentation "GNU CLISP - http://clisp.cons.org/")
  (:class clisp)
  (:reexport-from #:socket
                  #:socket-connect)
  (:reexport-from #:ext
                  #:read-byte-sequence))


;;; CMUCL

(define-implementation-package :cmu #:qlqs-cmucl
  (:documentation "CMU Common Lisp - http://www.cons.org/cmucl/")
  (:class cmucl)
  (:reexport-from #:ext
                  #:*gc-verbose*)
  (:reexport-from #:system
                  #:make-fd-stream)
  (:reexport-from #:extensions
                  #:connect-to-inet-socket))

(defvar qlqs-cmucl:*gc-verbose* nil)


;;; ECL

(define-implementation-package :ecl #:qlqs-ecl
  (:documentation "ECL - http://ecls.sourceforge.net/")
  (:class ecl)
  (:prep
   (require 'sockets))                                    ;对于ECL实现, 预先载入sockets包, 然后再reexport-from中定义引用和导出
  (:intern #:host-network-address)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:host-ent-address
                  #:socket-connect
                  #:socket-make-stream
                  #:inet-socket))


;;; LispWorks

(define-implementation-package :lispworks #:qlqs-lispworks
  (:documentation "LispWorks - http://www.lispworks.com/")
  (:class lispworks)
  (:prep
   (require "comm"))
  (:reexport-from #:comm
                  #:open-tcp-stream
                  #:get-host-entry))


;;; SBCL

(define-implementation-package :sbcl #:qlqs-sbcl
  (:class sbcl)
  (:documentation
   "Steel Bank Common Lisp - http://www.sbcl.org/")
  (:prep
   (require 'sb-bsd-sockets))
  (:intern #:host-network-address)
  (:reexport-from #:sb-ext                             ;引用sb-ext包,并导出compiler-note
                  #:compiler-note)
  (:reexport-from #:sb-bsd-sockets                     ;引用sb-bsd-socket包,并导出与网络连接对应的SBCL实现函数
                  #:get-host-by-name
                  #:inet-socket
                  #:host-ent-address
                  #:socket-connect
                  #:socket-make-stream))

;;;
;;; Utility function
;;;

(in-package #:qlqs-impl-util)

(definterface call-with-quiet-compilation (fun)        ;对于所有的CL实现, 屏蔽编译产生的信息时,设定下面一些全局变量为nil
  (:implementation t
		   (let ((*load-verbose* nil)
			 (*compile-verbose* nil)
			 (*load-print* nil)
			 (*compile-print* nil))
		     (handler-bind ((warning #'muffle-warning))
		       (funcall fun)))))

(defimplementation (call-with-quiet-compilation :for sbcl :qualifier :around)    ;针对SBCL还有特定的处理
    (fun)
  (declare (ignorable fun))
  (handler-bind ((qlqs-sbcl:compiler-note #'muffle-warning))
    (call-next-method)))

(defimplementation (call-with-quiet-compilation :for cmucl :qualifier :around)
    (fun)
  (declare (ignorable fun))
  (let ((qlqs-cmucl:*gc-verbose* nil))
    (call-next-method)))


;;;
;;; Low-level networking implementations
;;;

(in-package #:qlqs-network)

(definterface host-address (host)                  ;获取主机ip地址
  (:implementation t                           ;除了SBCL之外的其他CL
		   host)
  (:implementation sbcl
		   (qlqs-sbcl:host-ent-address (qlqs-sbcl:get-host-by-name host))))  ;SBCL特定实现

(definterface open-connection (host port)      ;打开网络连接
  (:implementation t
		   (declare (ignorable host port))
		   (error "Sorry, quicklisp in implementation ~S is not supported yet."
			  (lisp-implementation-type)))
  (:implementation allegro                     ;针对不同的CL,其方法不太一样
		   (qlqs-allegro:make-socket :remote-host host
					     :remote-port port))
  (:implementation abcl
		   (let ((socket (qlqs-abcl:make-socket host port)))
		     (qlqs-abcl:get-socket-stream socket :element-type '(unsigned-byte 8))))
  (:implementation ccl
		   (qlqs-ccl:make-socket :remote-host host
					 :remote-port port))
  (:implementation clisp
		   (qlqs-clisp:socket-connect port host :element-type '(unsigned-byte 8)))
  (:implementation cmucl
		   (let ((fd (qlqs-cmucl:connect-to-inet-socket host port)))
		     (qlqs-cmucl:make-fd-stream fd
						:element-type '(unsigned-byte 8)
						:binary-stream-p t
						:input t
						:output t)))
  (:implementation ecl
		   (let* ((endpoint (qlqs-ecl:host-ent-address
				     (qlqs-ecl:get-host-by-name host)))
			  (socket (make-instance 'qlqs-ecl:inet-socket
						 :protocol :tcp
						 :type :stream)))
		     (qlqs-ecl:socket-connect socket endpoint port)
		     (qlqs-ecl:socket-make-stream socket
						  :element-type '(unsigned-byte 8)
						  :input t
						  :output t
						  :buffering :full)))
  (:implementation lispworks
		   (qlqs-lispworks:open-tcp-stream host port
						   :direction :io
						   :read-timeout nil
						   :element-type '(unsigned-byte 8)
						   :timeout 5))
  (:implementation sbcl
		   (let* ((endpoint (qlqs-sbcl:host-ent-address
				     (qlqs-sbcl:get-host-by-name host)))
			  (socket (make-instance 'qlqs-sbcl:inet-socket
						 :protocol :tcp
						 :type :stream)))
		     (qlqs-sbcl:socket-connect socket endpoint port)
		     (qlqs-sbcl:socket-make-stream socket
						   :element-type '(unsigned-byte 8)
						   :input t
						   :output t
						   :buffering :full))))

(definterface read-octets (buffer connection)     ;读取网络连接中的字节序列
  (:implementation t
		   (read-sequence buffer connection))
  (:implementation allegro
		   (qlqs-allegro:read-vector buffer connection))
  (:implementation clisp
		   (qlqs-clisp:read-byte-sequence buffer connection
						  :no-hang nil
						  :interactive t)))

(definterface write-octets (buffer connection)   ;向网络连接中写数据
  (:implementation t
		   (write-sequence buffer connection)
		   (finish-output connection)))

(definterface close-connection (connection)      ;关闭连接
  (:implementation t
		   (ignore-errors (close connection))))

(definterface call-with-connection (host port fun)     ;对指定主机端口, 创建连接, 并执行fun
  (:implementation t
		   (let (connection)
		     (unwind-protect
			  (progn
			    (setf connection (open-connection host port))
			    (funcall fun connection))
		       (when connection
			 (close connection))))))  ;这里是不是错了? 应该调用close-connection不是吗?


(defmacro with-connection ((connection host port) &body body)     ;call-with-connection的with实现, 很优美的代码
  `(call-with-connection ,host ,port (lambda (,connection) ,@body)))


;;;
;;; A text progress bar
;;;

(in-package #:qlqs-progress)                               ;定义文本风格进度条的包

(defclass progress-bar ()                                  ;进度条基类
  ((start-time                                             ;开始时间
    :initarg :start-time
    :accessor start-time)
   (end-time                                               ;结束时间
    :initarg :end-time
    :accessor end-time)
   (progress-character                                     ;代表进度条显示的字符
    :initarg :progress-character
    :accessor progress-character)
   (character-count                                        ;进度条显示宽度, 总字符个数
    :initarg :character-count
    :accessor character-count
    :documentation "How many characters wide is the progress bar?")
   (characters-so-far                                      ;代表目前进度条已显示的字符个数
    :initarg :characters-so-far
    :accessor characters-so-far)
   (update-interval                                        ;进度条更新间隔, 多久更新一次进度条的显示
    :initarg :update-interval
    :accessor update-interval
    :documentation "Update the progress bar display after this many
    internal-time units.")
   (last-update-time                                       ;上次更新进度条的时间
    :initarg :last-update-time
    :accessor last-update-time
    :documentation "The display was last updated at this time.")
   (total                                                  ;进度的总单位数 (在这里表示的是总字节个数)
    :initarg :total
    :accessor total
    :documentation "The total number of units tracked by this progress bar.") ;下面的代码里显示的内容是字节,这里注释用unit好像没什么意思.
   (progress                                               ;进度条显示的已完成进度 , progress+pending为实际当前完成进度数 (即完成字节个数)
    :initarg :progress
    :accessor progress
    :documentation "How far in the progress are we?")
   (pending                                                ;下次更新进度条时, 需要增加的单位数 
    :initarg :pending
    :accessor pending
    :documentation "How many raw units should be tracked in the next
    display update?"))
  (:default-initargs
   :progress-character #\=
    :character-count 50
    :characters-so-far 0
    :update-interval (floor internal-time-units-per-second 4)                        ;四分之一秒更新一次
    :last-update-time 0
    :total 0
    :progress 0
    :pending 0))

(defgeneric start-display (progress-bar))                               ;进度开始
(defgeneric update-progress (progress-bar unit-count))                  ;跟新进度
(defgeneric update-display (progress-bar))                              ;更新进度条显示
(defgeneric finish-display (progress-bar))                              ;更新最后剩余的显示
(defgeneric elapsed-time (progress-bar))                                ;进度完成总花费时间
(defgeneric units-per-second (progress-bar))                            ;平均每秒完成进度单位数 (即字节)

(defmethod start-display (progress-bar)
  (setf (last-update-time progress-bar) (get-internal-real-time))       ;上次更新时间为当前时间
  (setf (start-time progress-bar) (get-internal-real-time))             ;开始时间为当前时间
  (fresh-line)
  (finish-output))

(defmethod update-display (progress-bar)                                
  (incf (progress progress-bar) (pending progress-bar))                  ;更新进度条显示的已完成进度为当前实际已完成进度
  (setf (pending progress-bar) 0)                                        ;进度条已完成数已经等于实际进度已完成数, 所以pending为0
  (setf (last-update-time progress-bar) (get-internal-real-time))        ;重置上次更新时间
  (let* ((showable (floor (character-count progress-bar)                 ;进度更新后,需要显示的总字符个数
                          (/ (total progress-bar) (progress progress-bar))))      
         (needed (- showable (characters-so-far progress-bar))))         ;相比上次更新显示, 此次更新显示需要增加的字符个数
    (setf (characters-so-far progress-bar) showable)                     ;重置当前以显示的字符个数
    (dotimes (i needed)
      (write-char (progress-character progress-bar)))
    (finish-output)))

(defmethod update-progress (progress-bar unit-count)                      
  (incf (pending progress-bar) unit-count)                               ;当前进度增加了unit-count, 下次更新进度条需要增加的单位数pending就为逐次累加的进度增加的内容, 
  (let ((now (get-internal-real-time)))                                  
    (when (< (update-interval progress-bar)                              ;当前时间与上次更新时间超过更新间隔的时候更新进度条
             (- now (last-update-time progress-bar)))
      (update-display progress-bar))))

(defmethod finish-display (progress-bar)
  (update-display progress-bar)                                          ;完成的时候, 首先更新显示
  (setf (end-time progress-bar) (get-internal-real-time))                ;完成时间
  (terpri)
  (format t "~:D bytes in ~$ seconds (~$KB/sec)"                         ;输出统计信息
          (total progress-bar)
          (elapsed-time progress-bar)
          (/  (units-per-second progress-bar) 1024))                     ;为什么不用kb/sec这个函数呢？
  (finish-output))

(defmethod elapsed-time (progress-bar)                                   ;总花费的秒数
  (/ (- (end-time progress-bar) (start-time progress-bar))
     internal-time-units-per-second))

(defmethod units-per-second (progress-bar)                               ;每秒下载的单位个数 (字节)
  (if (plusp (elapsed-time progress-bar))
      (/ (total progress-bar) (elapsed-time progress-bar))
      0))

(defun kb/sec (progress-bar)                                             ;换算成千字节的每秒个数
  (/ (units-per-second progress-bar) 1024))



(defparameter *uncertain-progress-chars* "?")                            ;进度条显示的 字符串

(defclass uncertain-size-progress-bar (progress-bar)                     ;未知总进度大小的进度条的显示, 这里total不作为总进度大小,而作为已完成的进度大小
  ((progress-char-index                                                  ;用字符串中的哪个作为字符 显示
    :initarg :progress-char-index
    :accessor progress-char-index)
   (units-per-char                                                       ;一个字符代表多少个进度单位
    :initarg :units-per-char
    :accessor units-per-char))
  (:default-initargs
   :total 0
    :progress-char-index 0
    :units-per-char (floor (expt 1024 2) 50)))                            ;一个字符 0.02KB

(defmethod update-progress :after ((progress-bar uncertain-size-progress-bar)   ;这里用:after和基类的update-progress函数合并
				   unit-count)
  (incf (total progress-bar) unit-count))   ;更新已完成的进度大小

(defmethod progress-character ((progress-bar uncertain-size-progress-bar))  ; 循环显示字符串里的字符, 作为进度条的显示
  (let ((index (progress-char-index progress-bar)))
    (prog1
        (char *uncertain-progress-chars* index)
      (setf (progress-char-index progress-bar)
            (mod (1+ index) (length *uncertain-progress-chars*))))))

(defmethod update-display ((progress-bar uncertain-size-progress-bar))   
  (setf (last-update-time progress-bar) (get-internal-real-time))       ;重置上次更新时间
  (multiple-value-bind (chars pend)                                     ;chars为按照units-per-char为单位, 计算的取整后的应该显示的字符个数, pend为还不够显示一个字符的那些进度单位个数 (字节个数), 这部分合并到下次更新中去.
      (floor (pending progress-bar) (units-per-char progress-bar))      
    (setf (pending progress-bar) pend)                                  ;对未更新的那部分剩余的进度单位个数, 合并下次更新
    (dotimes (i chars)
      (write-char (progress-character progress-bar))
      (incf (characters-so-far progress-bar))
      (when (<= (character-count progress-bar)                          ;当进度超过一个进度条的时候, 换行显示第二个进度条
                (characters-so-far progress-bar))
        (terpri)
        (setf (characters-so-far progress-bar) 0)
        (finish-output)))
    (finish-output)))

(defun make-progress-bar (total)                                         ;创建进度条实例
  (if (or (not total) (zerop total))
      (make-instance 'uncertain-size-progress-bar)
      (make-instance 'progress-bar :total total)))

;;;
;;; A simple HTTP client
;;;

(in-package #:qlqs-http)

;;; Octet data

(deftype octet ()			;定义无符号字节类型，代表Ascii码
  '(unsigned-byte 8))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet
              :initial-element 0))

(defun octet-vector (&rest octets)	;创建一个无符号字节的数组
  (make-array (length octets) :element-type 'octet
              :initial-contents octets))

;;; ASCII characters as integers

(defun acode (char)			;把acode转换成整数, acode为 字符, :cr, :lf
  (cond ((eql char :cr)
         13)
        ((eql char :lf)
         10)
        (t
         (let ((code (char-code char)))
           (if (<= 0 code 127)
               code
               (error "Character ~S is not in the ASCII character set"
                      char))))))

(defvar *whitespace*
  (list (acode #\Space) (acode #\Tab) (acode :cr) (acode :lf)))

(defun whitep (code)			;判断是否为空白字符
  (member code *whitespace*))

(defun ascii-vector (string)		;将字符串转换成 整数数组
  (let ((vector (make-octet-vector (length string))))
    (loop for char across string	;可否用map实现呢？
       for code = (char-code char)
       for i from 0
       if (< 127 code) do		;｀伟大｀的loop啊！
	 (error "Invalid character for ASCII -- ~A" char)
       else
       do (setf (aref vector i) code))
    vector))

(defun ascii-subseq (vector start end)	;将整数数组转换到字符串, end超过vector怎么办？
  "Return a subseq of octet-specialized VECTOR as a string."
  (let ((string (make-string (- end start))))
    (loop for i from 0
       for j from start below end
       do (setf (char string i) (code-char (aref vector j))))
    string))

(defun ascii-downcase (code)
  (if (<= 65 code 90)			;lisp <= 的魅力！
      (+ code 32)
      code))

(defun ascii-equal (a b)
  (eql (ascii-downcase a) (ascii-downcase b)))

;; A demo of macro expansion of acase
;; (qlqs-http::acase (qlqs-http::acode #\Space)
;; 	   (:cr (print "Carriage Return"))
;; 	   (:lf (print "Line Feed"))
;; 	   (#\Space (print "Space")))
;; 
;; (CASE (QLQS-HTTP::ACODE #\ )
;;   ((13) (PRINT "Carriage Return"))
;;   ((10) (PRINT "Line Feed"))
;;   ((32) (PRINT "Space")))

(defmacro acase (value &body cases)	;value为整数，把cases中的acode key转换成整数后（是整数就不转还），使用case进行判断
  (flet ((convert-case-keys (keys)
           (mapcar (lambda (key)
                     (etypecase key
                       (integer key)
                       (character (char-code key))
                       (symbol
                        (ecase key
                          (:cr 13)
                          (:lf 10)
                          ((t) t)))))
                   (if (consp keys) keys (list keys)))))
    `(case ,value
       ,@(mapcar (lambda (case)				 ;这个lambda返回的是 (key-for-case body-form)的对，mapcar之后再去掉括号，构成case的参数
                   (destructuring-bind (keys &rest body) ;很不错的方法
                       case
                     `(,(if (eql keys t)
                            t
                            (convert-case-keys keys))
                        ,@body)))
                 cases))))

;;; Pattern matching (for finding headers)

(defclass matcher ()
  ((pattern
    :initarg :pattern
    :reader pattern)
   (pos					;匹配过程中标记已匹配成功的字符个数
    :initform 0
    :accessor match-pos)
   (matchedp
    :initform nil
    :accessor matchedp)))

(defun reset-match (matcher)
  (setf (match-pos matcher) 0
        (matchedp matcher) nil))

(define-condition match-failure (error) ())

(defun match (matcher input &key (start 0) end error) ;当error为t时，matcher与input必须完全匹配
  (let ((i start)
        (end (or end (length input)))
        (match-end (length (pattern matcher))))
    (with-slots (pattern pos)
        matcher
      (loop			;时间复杂度O(length of input)！
	 (cond ((= pos match-end)		;若matcher的已匹配字符索引已经到matcher结尾，说明匹配成功
		(let ((match-start (- i pos)))
		  (setf pos 0)
		  (setf (matchedp matcher) t)
		  (return (values match-start (+ match-start match-end)))))
	       ((= i end)			;若matcher还没完全匹配完所有的内容的时候，input就结束了，说明匹配失败，返回nil
		(return nil))		;为什么这里不用error?
	       ((= (aref pattern pos)	;如果内容相同，同时增加matcher与input的字符索引
		   (aref input i))
		(incf i)
		(incf pos))
	       (t
		(if error
		    (error 'match-failure)
		    (if (zerop pos)	;若pos为0，说明到目前还没有任何内容字符被匹配，那么简单增加input里的字符索引就可以了
			(incf i)
			(setf pos 0))))))))) ;若pos不为0,说明虽然部分匹配了,但到当前字符不再匹配，所以重置pos，重新匹配整个matcher

(defun ascii-matcher (string)
  (make-instance 'matcher
                 :pattern (ascii-vector string)))

(defun octet-matcher (&rest octets)
  (make-instance 'matcher
                 :pattern (apply 'octet-vector octets)))

(defun acode-matcher (&rest codes)	;是不是参数的名字叫chars比较好？
  (make-instance 'matcher
                 :pattern (make-array (length codes)
                                      :element-type 'octet
                                      :initial-contents
                                      (mapcar 'acode codes))))


;;; "Connection Buffers" are a kind of callback-driven,
;;; pattern-matching chunky stream. Callbacks can be called for a
;;; certain number of octets or until one or more patterns are seen in
;;; the input. cbufs automatically refill themselves from a
;;; connection as needed.

(defvar *cbuf-buffer-size* 8192)

(define-condition end-of-data (error) ())

(defclass cbuf ()
  ((data
    :initarg :data
    :accessor data)
   (connection
    :initarg :connection
    :accessor connection)
   (start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   (eofp
    :initarg :eofp
    :accessor eofp))
  (:default-initargs
   :data (make-octet-vector *cbuf-buffer-size*)
    :connection nil
    :start 0
    :end 0
    :eofp nil)
  (:documentation "A CBUF is a connection buffer that keeps track of
  incoming data from a connection. Several functions make it easy to
  treat a CBUF as a kind of chunky, callback-driven stream."))

(define-condition cbuf-progress ()
  ((size
    :initarg :size
    :accessor cbuf-progress-size
    :initform 0)))

(defun call-processor (fun cbuf start end)
  (signal 'cbuf-progress :size (- end start))
  (funcall fun (data cbuf) start end))

(defun make-cbuf (connection)
  (make-instance 'cbuf :connection connection))

(defun make-stream-writer (stream)
  "Create a callback for writing data to STREAM."
  (lambda (data start end)
    (write-sequence data stream :start start :end end)))

(defgeneric size (cbuf)
  (:method ((cbuf cbuf))
    (- (end cbuf) (start cbuf))))

(defgeneric emptyp (cbuf)
  (:method ((cbuf cbuf))
    (zerop (size cbuf))))

(defgeneric refill (cbuf)
  (:method ((cbuf cbuf))
    (when (eofp cbuf)
      (error 'end-of-data))
    (setf (start cbuf) 0)
    (setf (end cbuf)
          (read-octets (data cbuf)
                       (connection cbuf))) ;读取stream中的内容，如果为空，则stream到达eof，设置cbuf的eofp为t
    (cond ((emptyp cbuf)
           (setf (eofp cbuf) t)
           (error 'end-of-data))	;产生的错误在事件调用函数中进行处理
          (t (size cbuf)))))

(defun process-all (fun cbuf)		;处理整个buffer中的8192字节的所有内容
  (unless (emptyp cbuf)
    (call-processor fun cbuf (start cbuf) (end cbuf))))

(defun multi-cmatch (matchers cbuf)	;找到所有matcher匹配的start与end
  (let (start end)
    (dolist (matcher matchers (values start end))
      (multiple-value-bind (s e)
          (match matcher (data cbuf)
                 :start (start cbuf)
                 :end (end cbuf))
        (when (and s (or (null start) (< s start)));返回匹配到的matcher中，start最小的匹配的start与end
          (setf start s
                end e))))))

(defun cmatch (matcher cbuf)
  (if (consp matcher)
      (multi-cmatch matcher cbuf)
      (match matcher (data cbuf) :start (start cbuf) :end (end cbuf))))

(defun call-until-end (fun cbuf)	;处理流中的所有内容
  (handler-case				;处理end-of-data错误
      (loop
	 (process-all fun cbuf)
	 (refill cbuf))
    (end-of-data ()
      (return-from call-until-end))))

(defun show-cbuf (context cbuf)		;这是什么东东？
  (format t "cbuf: ~A ~D - ~D~%" context (start cbuf) (end cbuf)))

(defun call-for-n-octets (n fun cbuf)	;处理从流开始的N个字节的内容
  (let ((remaining n))
    (loop
       (when (<= remaining (size cbuf))	;对于剩余的部分,如果不是一整个cbuf,则只处理剩余部分
	 (let ((end (+ (start cbuf) remaining)))
	   (call-processor fun cbuf (start cbuf) end)
	   (setf (start cbuf) end)
	   (return)))
       (process-all fun cbuf)		;对于可填满cbuf的整块内容，全部处理
       (decf remaining (size cbuf))	;从流中减去那些被处理的
       (refill cbuf))))

(defun call-until-matching (matcher fun cbuf) ;如果有匹配，则到匹配处停止处理，否则处理整个流中内容
  (loop
     (multiple-value-bind (start end)
	 (cmatch matcher cbuf)
       (when start
	 (call-processor fun cbuf (start cbuf) end)
	 (setf (start cbuf) end)
	 (return)))
     (process-all fun cbuf)
     (refill cbuf)))

(defun ignore-data (data start end)
  (declare (ignore data start end)))	;很严谨！

(defun skip-until-matching (matcher cbuf)
  (call-until-matching matcher 'ignore-data cbuf))


;;; Creating HTTP requests as octet buffers

(defclass octet-sink ()
  ((storage
    :initarg :storage
    :accessor storage))
  (:default-initargs
   :storage (make-array 1024 :element-type 'octet
                        :fill-pointer 0
                        :adjustable t))
  (:documentation "A simple stream-like target for collecting
  octets."))

(defun add-octet (octet sink)
  (vector-push-extend octet (storage sink)))

(defun add-octets (octets sink &key (start 0) end) ;end不判断大小不会有问题吗？
  (setf end (or end (length octets)))
  (loop for i from start below end
     do (add-octet (aref octets i) sink)))

(defun add-string (string sink)
  (loop for char across string
     for code = (char-code char)
     do (add-octet code sink)))

(defun add-strings (sink &rest strings)
  (mapc (lambda (string) (add-string string sink)) strings)) ;返回值是否比较不好呢？

(defun add-newline (sink)
  (add-octet 13 sink)
  (add-octet 10 sink))

(defun sink-buffer (sink)
  (subseq (storage sink) 0))

(defvar *proxy-url* nil)

(defun full-proxy-path (host port path)
  (format nil "~:[http~;https~]://~A~:[:~D~;~*~]~A" ;强大的format tilde控制符！~[选择~*跳过
	  (= port 443)
	  host
	  (or (= port 80)
	      (= port 443))
	  port
	  path))

(defun make-request-buffer (host port path &key (method "GET")) ;创建一个HTTP请求报文
  (setf method (string method))
  (when *proxy-url*
    (setf path (full-proxy-path host port path))) ;不明白为什么代理的时候，路径为全路径？
  (let ((sink (make-instance 'octet-sink)))
    (flet ((add-line (&rest strings)
             (apply #'add-strings sink strings)
             (add-newline sink)))
      (add-line method " " path " HTTP/1.1")
      (add-line "Host: " host (if (= port 80) ""
                                  (format nil ":~D" port)))
      (add-line "Connection: close")
      ;; FIXME: get this version string from somewhere else.
      (add-line "User-Agent: quicklisp-bootstrap/2010113000")
      (add-newline sink)
      (sink-buffer sink))))

(defun sink-until-matching (matcher cbuf) ;将cbuf中到匹配的内容都放入sink的buffer中
  (let ((sink (make-instance 'octet-sink)))
    (call-until-matching
     matcher
     (lambda (buffer start end)
       (add-octets buffer sink :start start :end end))
     cbuf)
    (sink-buffer sink)))


;;; HTTP headers

(defclass header ()			;响应头部的内容为名字：值的键值对
  ((data				;响应数据的字节数组
    :initarg :data
    :accessor data)
   (status
    :initarg :status
    :accessor status)
   (name-starts
    :initarg :name-starts
    :accessor name-starts)
   (name-ends
    :initarg :name-ends
    :accessor name-ends)
   (value-starts
    :initarg :value-starts
    :accessor value-starts)
   (value-ends
    :initarg :value-ends
    :accessor value-ends)))

(defmethod print-object ((header header) stream)
  (print-unreadable-object (header stream :type t)
    (prin1 (status header) stream)))

(defun matches-at (pattern target pos)
  (= (mismatch pattern target :start2 pos) (length pattern)))

(defun header-value-indexes (field-name header)
  (loop with data = (data header)	;这个loop可以做教学的典型例子！
     with pattern = (ascii-vector (string-downcase field-name))
     for start across (name-starts header)
     for i from 0
     when (matches-at pattern data start)  ;在头部数据的每个名字开始的地方，开始匹配pattern，如果匹配到了，则返回对应的value的开始和结束的位置
     return (values (aref (value-starts header) i)
		    (aref (value-ends header) i))))

(defun ascii-header-value (field-name header) ;返回名字对应的值的ascii字符串
  (multiple-value-bind (start end)
      (header-value-indexes field-name header) ;找到值的开始和结束位置
    (when start
      (ascii-subseq (data header) start end)))) ;获取该位置数组，转换为字符串

(defun all-field-names (header)		;返回所有的名字
  (map 'list
       (lambda (start end)
         (ascii-subseq (data header) start end))
       (name-starts header)
       (name-ends header)))

(defun headers-alist (header)		;返回名字与值的alist
  (mapcar (lambda (name)
            (cons name (ascii-header-value name header)))
          (all-field-names header)))

(defmethod describe-object :after ((header header) stream)
  (format stream "~&Decoded headers:~%  ~S~%" (headers-alist header)))

(defun content-length (header)
  (let ((field-value (ascii-header-value "content-length" header))) ;取得HTTP头部中，内容长度的值
    (when field-value
      (let ((value (ignore-errors (parse-integer field-value))))
        (or value
            (error "Content-Length header field value is not a number -- ~A"
                   field-value))))))

(defun chunkedp (header)
  (string= (ascii-header-value "transfer-encoding" header) "chunked"))

(defun location (header)
  (ascii-header-value "location" header))

(defun status-code (vector)		;HTTP响应报文开头的格式,比如HTTP/1.0 200 OK， 找到空格后的三个数为状态码
  (let* ((space (position (acode #\Space) vector))
         (c1 (- (aref vector (incf space)) 48)) ;从字符表示的1转换为数值表示的1
         (c2 (- (aref vector (incf space)) 48))
         (c3 (- (aref vector (incf space)) 48)))
    (+ (* c1 100)
       (* c2  10)
       (* c3   1))))

(defun force-downcase-field-names (header) ;取得的报文头部的那些名字，不管它的大小写
  (loop with data = (data header)
     for start across (name-starts header)
     for end across (name-ends header)
     do (loop for i from start below end
	   for code = (aref data i)
	   do (setf (aref data i) (ascii-downcase code)))))

(defun skip-white-forward (pos vector)
  (position-if-not 'whitep vector :start pos))

(defun skip-white-backward (pos vector)
  (let ((nonwhite (position-if-not 'whitep vector :end pos :from-end t)))
    (if nonwhite
        (1+ nonwhite)
        pos)))

(defun contract-field-value-indexes (header) ;报文头部的值信息，去掉前后的空白
  "Header field values exclude leading and trailing whitespace; adjust
the indexes in the header accordingly."
  (loop with starts = (value-starts header)
     with ends = (value-ends header)
     with data = (data header)
     for i from 0
     for start across starts
     for end across ends
     do
       (setf (aref starts i) (skip-white-forward start data))
       (setf (aref ends i) (skip-white-backward end data))))

(defun next-line-pos (vector)		;将位置移到下一行，太经典了！这不是状态机么
  (let ((pos 0))
    (labels ((finish (&optional (i pos)) 
               (return-from next-line-pos i))
             (after-cr (code)		;若已经遇到CR了，那么不管后面是什么，都表示着应该是换行了。
               (acase code
                 (:lf (finish pos))	;标准的情况，pos为:lf最后再incf一下就到下一行了
                 (t (finish (1- pos))))) ;不正常的情况
             (pending (code)		;处理一开始处于pending状态，读取字符，若字符不是CRLF，那么继续这个状态，略过非换行的那些字符
               (acase code
                 (:cr #'after-cr)	;若遇到CR进入，“遇到CR了”的这个状态
                 (:lf (finish pos))	;若直接遇到LF，则表示换行, 不需要CR的换行，这不是正常的情况吧。
                 (t #'pending))))
      (let ((state #'pending))
        (loop
	   (setf state (funcall state (aref vector pos))) ;状态迁移
	   (incf pos))))))

(defun make-hvector ()
  (make-array 16 :fill-pointer 0 :adjustable t))

					;每个头域由一个域名，冒号（:）和域值三部分组成。域名是大小写无关的，域值前可以添加任何数量的空格符，头域可以被扩展为多行，在每行开始处，使用至少一个空格或制表符。
(defun process-header (vector)		;读取数据，创建一个header实体，并将数据填入header实体
  "Create a HEADER instance from the octet data in VECTOR."
  (let* ((name-starts (make-hvector))
         (name-ends (make-hvector))
         (value-starts (make-hvector))
         (value-ends (make-hvector))
         (header (make-instance 'header
                                :data vector
                                :status 999
                                :name-starts name-starts
                                :name-ends name-ends
                                :value-starts value-starts
                                :value-ends value-ends))
         (mark nil)
         (pos (next-line-pos vector)))	;这个是header的起始位置
    (unless pos
      (error "Unable to process HTTP header"))
    (setf (status header) (status-code vector))
    (labels ((save (value vector)	;又是强大的状态机
               (vector-push-extend value vector))
             (mark ()			;记录值的结束的位置
               (setf mark pos))
             (clear-mark ()
               (setf mark nil))
             (finish ()
               (if mark
                   (save mark value-ends)
                   (save pos value-ends))
	       (force-downcase-field-names header)
	       (contract-field-value-indexes header)
	       (return-from process-header header))
             (in-new-line (code)	;一开始认为在新的一行
               (acase code
                 ((#\Tab #\Space) (setf mark nil) #'in-value) ;没有名字的值,不太可能有这种情况吧？
                 (t
                  (when mark		;如果设过mark那表示上一行的值的结尾的位置
                    (save mark value-ends))
                  (clear-mark)		;清除mark，当前位置为名字的开始位置，进入“在名字里”这个状态
                  (save pos name-starts)
                  (in-name code))))
             (after-cr (code)		
               (acase code
                 (:lf #'in-new-line)	
                 (t (in-new-line code)))) ;不太正常的情况
             (pending-value (code)	  ;这个状态没有用当前, 为了跳过值与:之间的空白吗?
               (acase code
                 ((#\Tab #\Space) #'pending-value)
                 (:cr #'after-cr)
                 (:lf #'in-new-line)
                 (t (save pos value-starts) #'in-value)))
             (in-name (code)
               (acase code
                 (#\:			;如果遇到：，表示名字部分结束，开始值的部分
                  (save pos name-ends)
                  (save (1+ pos) value-starts)
                  #'in-value)
                 ((:cr :lf)		;如果名字部分就遇到CRLF之一，表示该部分及以后都不是header了，就进入结束状态
                  (finish))
                 ((#\Tab #\Space)
                  (error "Unexpected whitespace in header field name"))
                 (t			;以上都不是,那么表示还在名字中.
                  (unless (<= 0 code 127)
                    (error "Unexpected non-ASCII header field name"))
                  #'in-name)))
             (in-value (code)		
               (acase code
                 (:lf (mark) #'in-new-line) ;如果遇到LF,记下值的结束位置, 接下来就是下一行这个状态了
                 (:cr (mark) #'after-cr)    ;如果遇到CR,那么进入下一个就是LF这个状态
                 (t #'in-value))))	    ;其他值都表示还在值中
      (let ((state #'in-new-line))
        (loop
	   (incf pos)
	   (when (<= (length vector) pos)
	     (error "No header found in response"))
	   (setf state (funcall state (aref vector pos)))))))) ;状态迁移


;;; HTTP URL parsing

(defclass url ()
  ((hostname
    :initarg :hostname
    :accessor hostname
    :initform nil)
   (port
    :initarg :port
    :accessor port
    :initform 80)
   (path
    :initarg :path
    :accessor path
    :initform "/")))

(defun parse-urlstring (urlstring)	;同样还是状态机的方式, 将字符串解析成url对像
  (setf urlstring (string-trim " " urlstring)) ;首先,去掉开头结尾的空白
  (let* ((pos (mismatch urlstring "http://" :test 'char-equal))
         (mark pos)
         (url (make-instance 'url)))
    (labels ((save ()			;解析出来 的 内容 在 前一次设置的mark到当前位置
               (subseq urlstring mark pos))
             (mark ()			;解析 的 开始位置
               (setf mark pos))
             (finish ()
               (return-from parse-urlstring url))
             (hostname-char-p (char)
               (position char "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_."
                         :test 'char-equal))
             (at-start (char)		;一开始如果就是/,则没有host和port,直接就是path
               (case char
                 (#\/
                  (setf (port url) nil)
                  (mark)
                  #'in-path)
                 (t			;否则一开始是host
                  #'in-host)))
             (in-host (char)
               (case char
                 ((#\/ :end)		;若遇到/或者:end字串不够的时候人为掉用的结尾,那么说明接下来是path, 先保存host
                  (setf (hostname url) (save))
                  (mark)		;设置path的开始谓位置
                  #'in-path)
                 (#\:			;若遇到:说明后面是port
                  (setf (hostname url) (save))
                  (mark)
                  #'in-port)
                 (t
                  (unless (hostname-char-p char)
                    (error "~S is not a valid URL" urlstring))
                  #'in-host)))
             (in-port (char)		;若遇到/或者结尾,那么说明接下来是path, 先保存port
               (case char
                 ((#\/ :end)
                  (setf (port url)
                        (parse-integer urlstring
                                       :start (1+ mark)
                                       :end pos))
                  (mark)
                  #'in-path)
                 (t
                  (unless (digit-char-p char)
                    (error "Bad port in URL ~S" urlstring))
                  #'in-port)))
             (in-path (char)            ;若遇到#或者结尾,那么说明分析完成
               (case char
                 ((#\# :end)
                  (setf (path url) (save))
                  (finish)))
               #'in-path))
      (let ((state #'at-start))
        (loop
	   (when (<= (length urlstring) pos)
	     (funcall state :end)		;当pos超过字串长度时,说明处理结束
	     (finish))
	   (setf state (funcall state (aref urlstring pos)))
	   (incf pos))))))

(defun url (thing)			;虽然有点不太严谨,但内部使用应该没问题
  (if (stringp thing)
      (parse-urlstring thing)
      thing))

(defgeneric request-buffer (method url)	;根据HTTP请求报文的方法,生成一个向URL的HTTP请求报文
  (:method (method url)
    (setf url (url url))
    (make-request-buffer (hostname url) (port url) (path url)
                         :method method)))

(defun urlstring (url)
  (format nil "~@[http://~A~]~@[:~D~]~A" ;~@[表示若参数为真,则不读取它,若参数为假,则不显示它它括起来的部分
          (hostname url)
          (and (/= 80 (port url)) (port url))
          (path url)))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (prin1 (urlstring url) stream)))

(defun merge-urls (url1 url2)
  (setf url1 (url url1))
  (setf url2 (url url2))
  (make-instance 'url
                 :hostname (or (hostname url1)		;merge-pathnames估计也是这样的处理方式
                               (hostname url2))
                 :port (or (port url1)
                           (port url2))
                 :path (or (path url1)
                           (path url2))))


;;; Requesting an URL and saving it to a file

(defparameter *maximum-redirects* 10)
(defvar *default-url-defaults* (url "http://src.quicklisp.org/"))

(defun read-http-header (cbuf)
  (let ((header-data (sink-until-matching (list (acode-matcher :lf :lf)
                                                (acode-matcher :cr :cr)
                                                (acode-matcher :cr :lf :cr :lf))
					  cbuf)))
    (process-header header-data)))

					;在有时服务器生成HTTP回应是无法确定消息大小的，这时用Content-Length就无法事先写入长度，而需要实时生成消息长度，这时服务器一般采用Chunked编码。在进行Chunked编码传输时，在回复消息的头部有transfer-coding并定为Chunked，表示将用Chunked编码传输内容。
					;编码使用若干个Chunk组成，由一个标明长度为0的chunk结束，每个Chunk有两部分组成，第一部分是该Chunk的长度和长度单位（一般不写），第二部分就是指定长度的内容，每个部分用CRLF隔开。在最后一个长度为0的Chunk中的内容是称为footer的内容，是一些没有写的头部内容。 
(defun read-chunk-header (cbuf)
  (let* ((header-data (sink-until-matching (acode-matcher :cr :lf) cbuf)) ;读取到chunk头部+CRLF的部分
         (end (or (position (acode :cr) header-data)
                  (position (acode #\;) header-data))))
    (values (parse-integer (ascii-subseq header-data 0 end) :radix 16)))) ;从0到end, 不包括chunk头部的CRLF

(defun save-chunk-response (stream cbuf)
  "For a chunked response, read all chunks and write them to STREAM."
  (let ((fun (make-stream-writer stream))
        (matcher (acode-matcher :cr :lf)))
    (loop
       (let ((chunk-size (read-chunk-header cbuf))) ;读取一个chunk的头部
	 (when (zerop chunk-size)			  ;若头部为0,表示为chunk结束
	   (return))
	 (call-for-n-octets chunk-size fun cbuf) ;读取chunk内容
	 (skip-until-matching matcher cbuf)))))  ;跳过chunk的分界CRLF

(defun save-response (file header cbuf)	;真对不同的编码传输方示,
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :element-type 'octet)
    (let ((content-length (content-length header)))
      (cond (content-length		;大小确定的时候
             (call-for-n-octets content-length
                                (make-stream-writer stream)
                                cbuf))
            ((chunkedp header)		;chunked的时候
             (save-chunk-response stream cbuf))
            (t				;否则,读到最后
             (call-until-end (make-stream-writer stream) cbuf))))))

(defun call-with-progress-bar (size fun)
  (let ((progress-bar (make-progress-bar size)))
    (start-display progress-bar)
    (flet ((update (condition)
             (update-progress progress-bar
                              (cbuf-progress-size condition))))
      (handler-bind ((cbuf-progress #'update)) ;lisp强大的地方，通过handler-bind将处理逻辑与显示分开，call-processor函数中引起cbuf-progress信号，更新显示！
        (funcall fun)))
    (finish-display progress-bar)))

;; HTTP状态代码 状态信息 含义 
;; ------------------------------------------------------------------------------------------
;; 100 Continue 初始的请求已经接受，客户应当继续发送请求的其余部分。（HTTP 1.1新） 
;; 101 Switching Protocols 服务器将遵从客户的请求转换到另外一种协议（HTTP 1.1新） 
;; 200 OK 一切正常，对GET和POST请求的应答文档跟在后面。
;; 201 Created 服务器已经创建了文档，Location头给出了它的URL。 
;; 202 Accepted 已经接受请求，但处理尚未完成。 
;; 203 Non-Authoritative Information 文档已经正常地返回，但一些应答头可能不正确，因为使用的是文档的拷贝（HTTP 1.1新）。 
;; 204 No Content 没有新文档，浏览器应该继续显示原来的文档。如果用户定期地刷新页面，而Servlet可以确定用户文档足够新，这个状态代码是很有用的。 
;; 205 Reset Content 没有新的内容，但浏览器应该重置它所显示的内容。用来强制浏览器清除表单输入内容（HTTP 1.1新）。 
;; 206 Partial Content 客户发送了一个带有Range头的GET请求，服务器完成了它（HTTP 1.1新）。 
;; 300 Multiple Choices 客户请求的文档可以在多个位置找到，这些位置已经在返回的文档内列出。如果服务器要提出优先选择，则应该在Location应答头指明。 
;; 301 Moved Permanently 客户请求的文档在其他地方，新的URL在Location头中给出，浏览器应该自动地访问新的URL。 
;; 302 Found 类似于301，但新的URL应该被视为临时性的替代，而不是永久性的。注意，在HTTP1.0中对应的状态信息是“Moved Temporatily”。 
;;     出现该状态代码时，浏览器能够自动访问新的URL，因此它是一个很有用的状态代码。 
;;     注意这个状态代码有时候可以和301替换使用。例如，如果浏览器错误地请求http://host/~user（缺少了后面的斜杠），有的服务器返回301，有的则返回302。 
;;     严格地说，我们只能假定只有当原来的请求是GET时浏览器才会自动重定向。请参见307。 
;; 303 See Other 类似于301/302，不同之处在于，如果原来的请求是POST，Location头指定的重定向目标文档应该通过GET提取（HTTP 1.1新）。 
;; 304 Not Modified 客户端有缓冲的文档并发出了一个条件性的请求（一般是提供If-Modified-Since头表示客户只想比指定日期更新的文档）。服务器告诉客户，原来缓冲的文档还可以继续使用。 
;; 305 Use Proxy 客户请求的文档应该通过Location头所指明的代理服务器提取（HTTP 1.1新）。 
;; 307 Temporary Redirect 和302（Found）相同。许多浏览器会错误地响应302应答进行重定向，即使原来的请求是POST，即使它实际上只能在POST请求的应答是303时才能重定向。
;;     由于这个原因，HTTP 1.1新增了307，以便更加清除地区分几个状态代码：当出现303应答时，浏览器可以跟随重定向的GET和POST请求；如果是307应答，则浏览器只能跟随对GET请求的重定向。（HTTP 1.1新） 
;; 400 Bad Request 请求出现语法错误。 
;; 401 Unauthorized 客户试图未经授权访问受密码保护的页面。应答中会包含一个WWW-Authenticate头，浏览器据此显示用户名字/密码对话框，然后在填写合适的Authorization头后再次发出请求。 
;; 403 Forbidden 资源不可用。服务器理解客户的请求，但拒绝处理它。通常由于服务器上文件或目录的权限设置导致。 
;; 404 Not Found 无法找到指定位置的资源。这也是一个常用的应答。 
;; 405 Method Not Allowed 请求方法（GET、POST、HEAD、DELETE、PUT、TRACE等）对指定的资源不适用。（HTTP 1.1新） 
;; 406 Not Acceptable 指定的资源已经找到，但它的MIME类型和客户在Accpet头中所指定的不兼容（HTTP 1.1新）。 
;; 407 Proxy Authentication Required 类似于401，表示客户必须先经过代理服务器的授权。（HTTP 1.1新） 
;; 408 Request Timeout 在服务器许可的等待时间内，客户一直没有发出任何请求。客户可以在以后重复同一请求。（HTTP 1.1新） 
;; 409 Conflict 通常和PUT请求有关。由于请求和资源的当前状态相冲突，因此请求不能成功。（HTTP 1.1新） 
;; 410 Gone 所请求的文档已经不再可用，而且服务器不知道应该重定向到哪一个地址。它和404的不同在于，返回407表示文档永久地离开了指定的位置，而404表示由于未知的原因文档不可用。（HTTP 1.1新） 
;; 411 Length Required 服务器不能处理请求，除非客户发送一个Content-Length头。（HTTP 1.1新） 
;; 412 Precondition Failed 请求头中指定的一些前提条件失败（HTTP 1.1新）。 
;; 413 Request Entity Too Large 目标文档的大小超过服务器当前愿意处理的大小。如果服务器认为自己能够稍后再处理该请求，则应该提供一个Retry-After头（HTTP 1.1新）。 
;; 414 Request URI Too Long URI太长（HTTP 1.1新）。 
;; 416 Requested Range Not Satisfiable 服务器不能满足客户在请求中指定的Range头。（HTTP 1.1新） 
;; 500 Internal Server Error 服务器遇到了意料不到的情况，不能完成客户的请求。 
;; 501 Not Implemented 服务器不支持实现请求所需要的功能。例如，客户发出了一个服务器不支持的PUT请求。 
;; 502 Bad Gateway 服务器作为网关或者代理时，为了完成请求访问下一个服务器，但该服务器返回了非法的应答。 
;; 503 Service Unavailable 服务器由于维护或者负载过重未能应答。例如，Servlet可能在数据库连接池已满的情况下返回503。服务器返回503时可以提供一个Retry-After头。 
;; 504 Gateway Timeout 由作为代理或网关的服务器使用，表示不能及时地从远程服务器获得应答。（HTTP 1.1新） 
;; 505 HTTP Version Not Supported 服务器不支持请求中所指明的HTTP版本。（HTTP 1.1新）
(defun fetch (url file &key (follow-redirects t) quietly
              (maximum-redirects *maximum-redirects*))
  "Request URL and write the body of the response to FILE."
  (setf url (merge-urls url *default-url-defaults*))
  (setf file (merge-pathnames file))	;将指定目录与当前目录merge，有效的处理指定的目录为相对路径的情况
  (let ((redirect-count 0)
        (original-url url)
        (connect-url (or (url *proxy-url*) url)) ;如果有代理，则先连接代理
        (stream (if quietly
                    (make-broadcast-stream)
                    *trace-output*)))	;详细信息输出到*trace-output*
    (loop				;循环，直到获取到文件，或者超过代理转接上限
       (when (<= maximum-redirects redirect-count)
	 (error "Too many redirects for ~A" original-url))
       (with-connection (connection (hostname connect-url) (port connect-url))
	 (let ((cbuf (make-instance 'cbuf :connection connection))
	       (request (request-buffer "GET" url))) ;在打开的连接里，发送HTTP GET请求报文
	   (write-octets request connection)
	   (let ((header (read-http-header cbuf))) ;读取响应报文
	     (loop while (= (status header) 100)
		do (setf header (read-http-header cbuf))) ;之前将全部的请求报文都发了，可能因为网络原因，服务端还没有收到全部内容。如果收到后，将会返回非100的状态。所以这里，一直读，直到有非100的状态返回。
	     (cond ((= (status header) 200)		   ;如果是200，则说明没有代理，并且返回的报文里就是请求的内容。
		    (let ((size (content-length header)))
		      (format stream "~&; Fetching ~A~%" url)
		      (if (and (numberp size)
			       (plusp size))
			  (format stream "; ~$KB~%" (/ size 1024))
			  (format stream "; Unknown size~%"))
		      (if quietly
			  (save-response file header cbuf) ;将请求的内容保存之
			  (call-with-progress-bar (content-length header)
						  (lambda ()
						    (save-response file header cbuf))))))
		   ((not (<= 300 (status header) 399)) ;如果不再300～399之间，可能是个错误
		    (error "Unexpected status for ~A: ~A"
			   url (status header))))
	     (if (and follow-redirects (<= 300 (status header) 399)) ;如果在300~399之间，则说明有代理转向
		 (let ((new-urlstring (ascii-header-value "location" header))) ;代理的时候，返回的location中为真正的内容的url地址，返回到一开始loop的地方，向真正的url再次发出请求。
		   (when (not new-urlstring)
		     (error "Redirect code ~D received, but no Location: header"
			    (status header)))
		   (incf redirect-count)
		   (setf url (merge-urls new-urlstring
					 url))
		   (format stream "~&; Redirecting to ~A~%" url))
		 (return (values header (and file (probe-file file))))))))))) ;返回HTTP头部对象，以及文件 (失败的情况下，应该为nil)


;;; A primitive tar unpacker

;; tar只是一个归档文件，并不进行压缩。
;; 　　struct tar_header
;; 　　{
;; 　　 char name[100];    //0
;; 　　 char mode[8];      //100
;; 　　 char uid[8];       //108
;; 　　 char gid[8];       //116
;; 　　 char size[12];     //124
;; 　　 char mtime[12];    //136
;; 　　 char chksum[8];    //148
;; 　　 char typeflag;     //156
;; 　　 char linkname[100];//157
;; 　　 char magic[6];     //257
;; 　　 char version[2];   //263
;; 　　 char uname[32];    //265
;; 　　 char gname[32];    //297
;; 　　 char devmajor[8];  //329
;; 　　 char devminor[8];  //337
;; 　　 char prefix[155];  //345
;; 　　 char padding[12];  //500
;; 　　};                  //512
;; 　　
;; 　　以上是tar中保存文件信息的数据结构，其后跟着的就是文件的内容
;;    size为文件大小的八进制字节表示，例如文件大小为90个字节，那么这里就是八进制的90，即为132。
;; 　　其中，文件大小，修改时间，checksum都是存储的对应的八进制字符串，字符串最后一个字符为空格字符
;; 　　checksum的计算方法为除去checksum字段其他所有的512-8共504个字节的ascii码相加的值再加上256(checksum当作八个空格，即8*0x20）
;; 　　文件内容以512字节为一个block进行分割，最后一个block不足部分以0补齐
;; 　　两个文件的tar包首先存放第一个文件的tar头结构，然后存储文件内容，接着存储第二个文件的tar头结构，然后存储文件内容
;; 　　所有文件都存储完了以后，最后存放一个全零的tar结构
;; 　　所有的tar文件大小应该都是512的倍数，一个空文件打包后为512*3字节，包括一个tar结构头，一个全零的block存储文件内容，一个全零的tar结构

(in-package #:qlqs-minitar)   ;迷你tar 解包器

(defun make-block-buffer ()   ;创建一个512字节的全零缓冲块
  (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))

(defun skip-n-blocks (n stream)   ;跳过流中n个块
  (let ((block (make-block-buffer)))
    (dotimes (i n)
      (read-sequence block stream))))

(defun ascii-subseq (vector start end)   ;将数组中,从start到end的内容转换成字符串
  (let ((string (make-string (- end start))))
    (loop for i from 0
       for j from start below end
       do (setf (char string i) (code-char (aref vector j))))
    string))

(defun block-asciiz-string (block start length) ;将一个块中从start开始的字符串读出
  (let* ((end (+ start length))
         (eos (or (position 0 block :start start :end end)
		  end)))
    (ascii-subseq block start eos)))

(defun prefix (header)                         ;读取tar头中prefix字段, 它是解包后的目标目录
  (when (plusp (aref header 345))
    (block-asciiz-string header 345 155)))

(defun name (header)                           ;读取tar头中name字段, 它是解包后文件或目录的名称
  (block-asciiz-string header 0 100))

(defun payload-size (header)		;读取tar头中的size字段, 转换为数字, 该内容为其后的文件的真实大小
  (values (parse-integer (block-asciiz-string header 124 12) :radix 8)))

(defun nth-block (n file)               ;读取文件中第n个512字节块的内容, 该函数好像没有地方在用?
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((block (make-block-buffer)))
      (skip-n-blocks (1- n) stream)
      (read-sequence block stream)
      block)))

(defun payload-type (code)              ;确定tar头代表目录还是文件
  (case code
    (0 :file)
    (48 :file)
    (53 :directory)
    (t :unsupported)))

(defun full-path (header) 		;tar头代表的文件或目录的全路径信息
  (let ((prefix (prefix header))
        (name (name header)))
    (if prefix
        (format nil "~A/~A" prefix name)
        name)))

(defun save-file (file size stream)	;将tar文件流中的文件部分转储到实际文件中
  (multiple-value-bind (full-blocks partial)  ;文件已512字节为单位的块个数, 以及剩余部分的字节数
      (truncate size 512)
    (ensure-directories-exist file)
    (with-open-file (outstream file
			       :direction :output
			       :if-exists :supersede
			       :element-type '(unsigned-byte 8))
      (let ((block (make-block-buffer)))
        (dotimes (i full-blocks)	      ;转储完整的512字节的那些块
          (read-sequence block stream)
          (write-sequence block outstream))
        (when (plusp partial)
          (read-sequence block stream)
          (write-sequence block outstream :end partial))))))   ;转储将剩余部分的内容

(defun unpack-tarball (tarfile &key (directory *default-pathname-defaults*)) ;解tar包
  (let ((block (make-block-buffer)))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
	 (let ((size (read-sequence block stream))) ;读取第一个tar头
	   (when (zerop size)			  ;若未读出
	     (return))
	   (unless (= size 512)		;若不是512字节
	     (error "Bad size on tarfile"))
	   (when (every #'zerop block)	;若读出的内容全零
	     (return))
	   (let* ((payload-code (aref block 156))
		  (payload-type (payload-type payload-code)) ;判断tar头代表的内容类型, 文件还是目录
		  (tar-path (full-path block))		   
		  (full-path (merge-pathnames tar-path directory)) ;完整的解包路径为tar头中指定的部分, 如果是相对路径,则用默认的路径merge之
		  (payload-size (payload-size block)))		 ;解包出的文件大小
	     (case payload-type
	       (:file
		(save-file full-path payload-size stream)) ;若为文件时,转储之
	       (:directory
		(ensure-directories-exist full-path)) ;若为目录时, 创建之
	       (t
		(warn "Unknown tar block payload code -- ~D" payload-code)
		(skip-n-blocks (ceiling (payload-size block) 512) stream))))))))) ;其他情况, 略过之 (可不可能有这种情况? 有了这样处理肯定正确?)

(defun contents (tarfile)		;列出tar包中的目录文件名称路径, 存到result中
  (let ((block (make-block-buffer))
        (result '()))
    (with-open-file (stream tarfile :element-type '(unsigned-byte 8))
      (loop
	 (let ((size (read-sequence block stream)))
	   (when (zerop size)
	     (return (nreverse result)))
	   (unless (= size 512)
	     (error "Bad size on tarfile"))
	   (when (every #'zerop block)
	     (return (nreverse result)))
	   (let* ((payload-type (payload-type (aref block 156)))
		  (tar-path (full-path block))
		  (payload-size (payload-size block)))
	     (skip-n-blocks (ceiling payload-size 512) stream)
	     (case payload-type
	       (:file
		(push tar-path result))
	       (:directory
		(push tar-path result)))))))))


;;;
;;; The actual bootstrapping work
;;;

(in-package #:quicklisp-quickstart)

(defvar *home*
  (merge-pathnames (make-pathname :directory '(:relative "quicklisp"))
                   (user-homedir-pathname))) ;quicklisp的目录为用户根目录下的quicklisp目录

(defun qmerge (pathname)
  (merge-pathnames pathname *home*))	;保证内容都在用户根目录下的quicklisp目录下

(defun renaming-fetch (url file)	
  (let ((tmpfile (qmerge "tmp/fetch.dat"))) ;先下载文件到fetch.dat
    (fetch url tmpfile)
    (rename-file tmpfile file)))	;rename-file不光重命名文件，还可以更改文件所属目录，相当于mv的功能

(defvar *asdf-url* "http://beta.quicklisp.org/quickstart/asdf.lisp")
(defvar *quicklisp-tar-url* "http://beta.quicklisp.org/quickstart/quicklisp.tar")
(defvar *setup-url* "http://beta.quicklisp.org/quickstart/setup.lisp")
(defvar *after-load-message*
  (format nil "~&~%  ==== quicklisp quickstart loaded ====~%~%    ~
               To continue, evaluate: (quicklisp-quickstart:install)~%~%"))

(defvar *after-initial-setup-message*
  (with-output-to-string (*standard-output*)
    (format t "~&~%  ==== quicklisp installed ====~%~%")
    (format t "    To load a system, use: (ql:quickload \"system-name\")~%~%")
    (format t "    To find systems, use: (ql:system-apropos \"term\")~%~%")
    (format t "    To load Quicklisp every time you start Lisp, use: (ql:add-to-init-file)~%~%")
    (format t "    For more information, see http://www.quicklisp.org/beta/~%~%")))

(defun initial-install ()		;初始化安装quicklisp
  (ensure-directories-exist (qmerge "tmp/")) ;下载时暂存用的目录
  (ensure-directories-exist (qmerge "quicklisp/")) 
  (renaming-fetch *asdf-url* (qmerge "asdf.lisp")) ;下载到~/quicklisp/下
  (let ((tmptar (qmerge "tmp/quicklisp.tar")))
    (renaming-fetch *quicklisp-tar-url* tmptar)
    (unpack-tarball tmptar :directory (qmerge "./"))) ;将tar包解压到~/quicklisp/quicklisp/下
  (renaming-fetch *setup-url* (qmerge "setup.lisp"))
  (load (qmerge "setup.lisp"))		;一切准备好后，启动setup.lisp，载入quicklisp包
  (write-string *after-initial-setup-message*)
  (finish-output))

(defun install (&key ((:path *home*) *home*)
                ((:proxy *proxy-url*) *proxy-url*))
  (setf *home* (merge-pathnames *home*)) ;不使用默认路径的时候，要将默认路径重置为用户指定的目录
  (let ((setup-file (qmerge "setup.lisp")))
    (when (probe-file setup-file)
      (multiple-value-bind (result proceed)
          (with-simple-restart (load-setup "Load ~S" setup-file) ;使用restart和用户交互！让用户选择是否继续执行。
            (error "Quicklisp has already been installed. Load ~S instead."
                   setup-file))
        (declare (ignore result))
        (when proceed
          (return-from install (load setup-file)))))) ;选择载入setup.lisp的时候，就继续载入
  (if (find-package '#:ql)			      ;
      (progn
        (write-line "!!! Quicklisp has already been set up. !!!") ;一般不会进入这里，除非上面的setup.lisp因为什么原因不存在了。
        (write-string *after-initial-setup-message*)
        t)
      (call-with-quiet-compilation #'initial-install))) ;没有安装过的情况下，进行初始化安装过程。

;;; Try to canonicalize to an absolute pathname; helps on Lisps where
;;; *default-pathname-defaults* isn't an absolute pathname at startup
;;; (e.g. CCL, CMUCL)
(setf *default-pathname-defaults* (truename *default-pathname-defaults*))

(write-string *after-load-message*)

;;; End of quicklisp.lisp