;;;; utils.lisp

(in-package #:ql-util)

(defun write-line-to-file (string file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede)
    (write-line string stream)))

(defvar *do-not-prompt* nil
  "When *DO-NOT-PROMPT* is true, PRESS-ENTER-TO-CONTINUE returns true
  without user interaction.")

(defmacro without-prompting (&body body)
  "Evaluate BODY in an environment where PRESS-ENTER-TO-CONTINUE
 always returns true without prompting for the user to press enter."
  `(let ((*do-not-prompt* t))
     ,@body))

(defun press-enter-to-continue ()
  (when *do-not-prompt*
    (return-from press-enter-to-continue t))
  (format *query-io* "~&Press Enter to continue.~%")
  (let ((result (read-line *query-io*)))
    (zerop (length result))))		;如果回车,则长度为空

(defun replace-file (from to)
  "Like RENAME-FILE, but deletes TO if it exists, first."
  (when (probe-file to)
    (delete-file to))
  (rename-file from to))

(defun copy-file (from to &key (if-exists :rename-and-delete))
  "Copy the file FROM to TO."
  (let* ((buffer-size 8192)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (with-open-file (from-stream from :element-type '(unsigned-byte 8))
      (with-open-file (to-stream to :element-type '(unsigned-byte 8)
                                 :direction :output
                                 :if-exists if-exists)
        (let ((length (file-length from-stream)))
          (multiple-value-bind (full leftover)
              (floor length buffer-size) ;这招在minitar里有用过,计算完整的buffer的个数,和最后那个不到buffer size的内容长度
            (dotimes (i full)		 ;首先,可填充完整的buffer的内容处理
              (read-sequence buffer from-stream)
              (write-sequence buffer to-stream))
            (read-sequence buffer from-stream)
            (write-sequence buffer to-stream :end leftover))))) ;最后,仅写入剩于的长度
    (probe-file to)))

(defun ensure-file-exists (pathname)
  (open pathname :direction :probe :if-does-not-exist :create)) ;注意这里direction的参数是probe

(defun delete-file-if-exists (pathname)
  (when (probe-file pathname)
    (delete-file pathname)))

(defun split-spaces (line)
  (let ((words '())			;包存解析后的单词
        (mark 0)
        (pos 0))
    (labels ((finish ()			
               (setf pos (length line))	;结尾
               (save)
               (return-from split-spaces (nreverse words))) ;反回所有的单词的 列表
             (save ()
               (when (< mark pos)	;如果mark小于结尾, 则说明还有一个单词
                 (push (subseq line mark pos) words))) ;将剩下的 单词 push到包存的 列表中
             (mark ()
               (setf mark pos))
             (in-word (char)
               (case char
                 (#\Space		;如果碰到space,就 说明当前单词结束,包存当前单词
                    (save)
                    #'in-space)
                 (t			;否则还是当前单词中
                    #'in-word)))
             (in-space (char)		
               (case char
                 (#\Space		;还在空格中
                    #'in-space)
                 (t
                    (mark)		;单词的开始位置包存
                    #'in-word))))
      (let ((state #'in-word))		;一开始是在单词中, 因位主要处理的是路径
        (dotimes (i (length line) (finish)) ;循环结束的时候,调用finish反回
          (setf pos i)
          (setf state (funcall state (char line i)))))))) ;状态机

(defun first-line (file)
  (with-open-file (stream file)
    (values (read-line stream))))

(defun (setf first-line) (line file)	;这个有点不对,rename-and-delete会把文件删了,就不只是set first line了
  (with-open-file (stream file :direction :output
                          :if-exists :rename-and-delete)
    (write-line line stream)))
