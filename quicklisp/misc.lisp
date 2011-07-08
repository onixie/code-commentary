;;;; misc.lisp

(in-package #:quicklisp-client)

;;;
;;; This stuff will probably end up somewhere else.
;;;

(defun use-only-quicklisp-systems ()
  (asdf:initialize-source-registry
   '(:source-registry :ignore-inherited-configuration)) ;对于sbcl来说，仅保留系统SBCL_HOME中的
  (asdf:map-systems 'asdf:clear-system)	;对每个加载过的系统，调用clear-system从asdf系统加载列表中去掉
  t)