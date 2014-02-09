;;;; lambda-calculus.lisp --- Untyped lambda calculus based on pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

;;;; Partially based on idea from
;;;;
;;;; [1] Benjamin C. Pierce (2002): Types and Programming languages

(cl:defpackage #:pattern-specializer.examples.lambda-calculus
  (:use
   #:cl
   #:pattern-specializer))

(cl:in-package #:pattern-specializer.examples.lambda-calculus)

;;; Syntax

(defstruct term)

(defstruct (value (:include term) (:constructor make-value (value)))
  (value nil)) ; TODO val?

(defstruct (var (:include term) (:constructor make-var (name)))
  (name nil :type symbol))

(defstruct (abst (:include value) (:constructor make-abst (var body)))
  (var nil :type var)
  (body nil :type term))

(defstruct (app (:include term) (:constructor make-app (func arg)))
  (func nil :type term)
  (arg nil :type term))

;;; Parse

(defgeneric parse (from)
  (:generic-function-class pattern-generic-function))

(defmethod parse ((form (pattern (optima:guard value (integerp value))))) ; TODO just (form integer)
  (make-value value))

(defmethod parse ((form (pattern (optima:guard name (symbolp name))))) ; TODO just (form symbol)
  (make-var form))

(defmethod parse ((form (pattern (list 'λ (optima:guard name (symbolp name)) body))))
  (make-abst (parse name) (parse body)))

(defmethod parse ((form (pattern (list func arg))))
  (make-app (parse func) (parse arg)))

;;; Substitution

(defgeneric substitute1 (term var val))

(defmethod substitute1 ((term value) (var var) (val value))
  term)

;; [1 Page 69]
(defmethod substitute1 ((term var) (var var) (val value))
  (if (equalp term var) val term))

;; [1 Page 69]
(defmethod substitute1 ((term abst) (var var) (val value))
  ;; TODO capture
  (make-abst (abst-var term) (substitute1 (abst-body term) var val)))

;; [1 Page 69]
(defmethod substitute1 ((term app) (var var) (val value))
  (make-app (substitute1 (app-func term) var val)
            (substitute1 (app-arg term) var val)))

;;; Evaluation

(defgeneric eval1 (term)
  (:generic-function-class pattern-generic-function))

(defmethod eval1 ((term (pattern (value)))) ; TODO does not need pattern
  term)

;; Reduce function to value
;;
;;      t_1 -> t_1'
;; ---------------------
;;  t_1 t_2 -> t_1' t_2
;;
;; [1 Page 72; Figure 5.3]
(defmethod eval1 ((term (pattern (app func arg))))
  (eval1 (make-app (eval1 func) arg)))

;; Reduce argument to value
;;
;;      t_2 -> t_2'
;; ---------------------
;;  v_1 t_2 -> v_1 t_2'
;;
;; [1 Page 72; Figure 5.3]
(defmethod eval1 ((term (pattern (app (func (and func (value))) arg))))
  (eval1 (make-app func (eval1 arg))))

;; Application
;;
;; (λx.t_12) v_2 -> [x -> v_2] t_12
;;
;; [1 Page 72; Figure 5.3]
(defmethod eval1 ((term (pattern (app (func (abst var body)) (arg (and arg (value)))))))
  (let ((arg-value (eval1 arg)))
    (eval1 (substitute1 body var arg-value))))

;;; Test

(eval1 (make-value 1))

(eval1 (make-abst (make-var 'x) (make-value 1)))

(eval1 (make-app (make-abst (make-var 'x) (make-var 'x)) (make-value 1)))
;; => S#(VALUE)

(eval1 (parse '(((λ z (λ y z)) 5) 6)))
