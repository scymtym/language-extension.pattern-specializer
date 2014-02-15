;;;; pattern-specializer.lisp --- Implementation of pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

;;; `pattern-specializer' class

(defclass pattern-specializer (specializable:extended-specializer)
  ((pattern        :initarg  :pattern
                   :reader   specializer-pattern))
  (:default-initargs
   :pattern (required-argument :pattern)))

(defmethod print-object ((object pattern-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specializer-pattern object) stream)))

(defun specializer-parsed-pattern (specializer)
  (optima::parse-pattern (specializer-pattern specializer)))

(defun specializer-pattern-variables (specializer)
  (optima::pattern-variables (specializer-parsed-pattern specializer)))

(specializable:define-extended-specializer pattern (generic-function pattern)
  (declare (ignore generic-function))
  (make-instance 'pattern-specializer :pattern pattern))

;; Parsing is handled by `define-extended-specializer' above

(defmethod unparse-specializer-using-class
    ((gf pattern-generic-function) (specializer pattern-specializer))
  `(pattern ,(specializer-pattern specializer)))

(defmethod make-specializer-form-using-class or
    ((proto-generic-function pattern-generic-function) ; TODO should work for all specializable generic functions
     (proto-method pattern-method)
     (specializer-name cons)
     (environment t))
  (when (typep specializer-name '(cons (eql pattern)))
    `(sb-pcl:parse-specializer-using-class ; TODO packages
      (sb-pcl:class-prototype (find-class ',(type-of proto-generic-function)))
      ',specializer-name)))

(defmethod sb-pcl::same-specializer-p ((specializer1 pattern-specializer)
                                       (specializer2 pattern-specializer))
  (equalp (specializer-pattern specializer1)
          (specializer-pattern specializer2)))

(defmethod specializable:specializer< ((generic-function specializable:specializable-generic-function)
                                       (specializer1 pattern-specializer)
                                       (specializer2 pattern-specializer)
                                       (generalizer t))
  (when (pattern-more-specific-p (specializer-parsed-pattern specializer1)
                                 (specializer-parsed-pattern specializer2))
    '<))

(defmethod specializable:specializer-accepts-p ((specializer pattern-specializer) object)
  ;; TODO store in specializer later
  (let* ((accept-form (with-gensyms (object)
                        `(lambda (,object)
                           (optima:match ,object
                             (,(specializer-pattern specializer)
                              (declare (ignore ,@(specializer-pattern-variables specializer)))
                              t)))))
         (accept-function (compile nil accept-form)))
    (funcall accept-function object )))

(defmethod specializable:specializer-accepts-generalizer-p ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (generalizer pattern-generalizer))
  (values (find specializer (pattern-generalizer-specializers generalizer)) t))

(defgeneric specializer-accepts-generalizer-p-using-pattern
    (generic-function specializer pattern generalizer)
  (:documentation
   "Like SPECIALIZER-ACCEPTS-GENERALIZER-P but with the ability to
    dispatch on PATTERN."))

(defmethod specializable:specializer-accepts-generalizer-p ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (generalizer class))
  (specializer-accepts-generalizer-p-using-pattern
   gf specializer (specializer-parsed-pattern specializer) generalizer))


(defmethod specializer-accepts-generalizer-p-using-pattern ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (pattern optima::variable-pattern)
                                                            (generalizer class))
  (values t t))

(defmethod specializer-accepts-generalizer-p-using-pattern ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (pattern optima::and-pattern)
                                                            (generalizer class))
  (let ((definitivep t))
    (values
     (block nil
       (mapc (lambda (subpattern)
               (multiple-value-bind (result definitivep)
                   (specializer-accepts-generalizer-p-using-pattern
                    gf specializer subpattern generalizer)
                 (unless result
                   (setf definitivep t) ; TODO correct?
                   (return nil))
                 (unless definitivep
                   (setf definitivep nil))))
             (optima::complex-pattern-subpatterns pattern)))
     definitivep)))

(defmethod specializer-accepts-generalizer-p-using-pattern ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (pattern optima::or-pattern)
                                                            (generalizer class))
  (error "not implemented"))

(defmethod specializer-accepts-generalizer-p-using-pattern ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (pattern optima::cons-pattern)
                                                            (generalizer class))
  (multiple-value-bind (result definitivep) (subtypep generalizer 'cons)
    (values result (when (and result definitivep)
                     (subpatterns-unrestricted-p pattern)))))

(defmethod specializer-accepts-generalizer-p-using-pattern ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (pattern optima::class-pattern)
                                                            (generalizer class))
  (multiple-value-bind (result definitivep)
      (specializable:specializer-accepts-generalizer-p
       gf (find-class (optima::class-pattern-class-name pattern)) generalizer)
    (values result (when (and result definitivep)
                     (subpatterns-unrestricted-p pattern)))))

(defmethod specializer-accepts-generalizer-p-using-pattern ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (pattern optima::guard-pattern)
                                                            (generalizer class))
  (values t nil))

;; TODO why did i need this again?
(defmethod class-name ((class (eql (find-class 'pattern-specializer))))
  'pattern-specializer)
;; at least this one is for slime
(defmethod class-name ((class pattern-specializer))
  'pattern-specializer)

;;; pattern-generalizer

(defstruct (pattern-generalizer
            (:constructor make-pattern-generalizer (#+no bitmap specializers key variables &optional next))
            (:copier nil))
  (specializers nil :type list :read-only t)
  (key nil :type t :read-only t)
  (variables nil :type list :read-only t)
  (next nil))

(defmethod specializable:generalizer-equal-hash-key
    ((generic-function specializable:specializable-generic-function)
     (generalizer pattern-generalizer))
  (pattern-generalizer-key generalizer))

(defmethod specializable::generalizer-args
    ((generic-function specializable:specializable-generic-function)
     (generalizer pattern-generalizer))
  (pattern-generalizer-variables generalizer))

;;; pattern-method

;; Forward definition. Actual definition is below.
(defclass pattern-generic-function (specializable:specializable-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defclass pattern-method (standard-method)
  ())

(defmethod method-pattern-specializers ((gf pattern-generic-function)
                                        (method pattern-method))
  (remove-if-not (of-type 'pattern-specializer)
                 (mapcar (curry #'parse-specializer-using-class gf) ; TODO necessary?
                         (method-specializers method))))

(defmethod make-method-lambda-using-specializers
    ((gf pattern-generic-function) (method pattern-method) qualifiers specializers
     lambda-expression environment)

  ;; This transforms LAMBDA-EXPRESSION of the form
  ;;
  ;;   (lambda (arg1 arg2 …) BODY)
  ;;
  ;; into
  ;;
  ;;   (lambda (arg1 arg2 …
  ;;            &key
  ;;            ((:PATTERN-VAR1 PATTERN-VAR1)) ((:PATTERN-VAR2 PATTERN-VAR2)) …
  ;;            &allow-other-keys)
  ;;     BODY)
  ;;
  ;; where BODY contains uses of PATTERN-VAR1, PATTERN-VAR2, …
  (destructuring-bind (operator lambda-list &body body) lambda-expression
    (declare (ignore operator))
    (multiple-value-bind (required optional rest keyword allow-other-keys-p)
        (parse-ordinary-lambda-list lambda-list :normalize nil)
      (flet ((make-keyword-parameter (variable)
               (list `((,(make-keyword variable) ,variable)))))
        (let* ((variables (mappend #'specializer-pattern-variables ; TODO this stuff is repeated in make-method-matching-form
                                   (remove-if-not (of-type 'pattern-specializer)
                                                  (mapcar (curry #'parse-specializer-using-class gf)
                                                          specializers))))
               (new-lambda-list `(,@required
                                  ,@(when optional
                                      `(&optional ,@optional))
                                  ,@(when rest
                                      `(&rest ,rest))
                                  ,@(when (or keyword variables)
                                      `(&key ,@keyword
                                             ,@(mapcan #'make-keyword-parameter variables)))
                                  ,@(when allow-other-keys-p
                                      '(&allow-other-keys))))
               (new-lambda-expression `(lambda ,new-lambda-list ,@body)))

          (format t "make-method-lambda-using-specializers~%  ~A~%  ~A~%  ~A~%  ~A~%=>"
                  gf method specializers lambda-expression)
          (print new-lambda-list)
          (print new-lambda-expression)

          (call-next-method
           gf method qualifiers specializers new-lambda-expression environment))))))

;;; pattern-generic-function

(defclass pattern-generic-function (specializable:specializable-generic-function)
  ((generalizer-makers :type     list
                       :accessor generic-function-generalizer-makers))
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :method-class (find-class 'pattern-method)))

(defmethod reinitialize-instance :after ((instance pattern-generic-function)
                                         &key)
  (log:info instance)
  (slot-makunbound instance 'generalizer-makers))

(defmethod generic-function-generalizer-makers ((generic-function pattern-generic-function))
  (if (slot-boundp generic-function 'generalizer-makers)
      (slot-value generic-function 'generalizer-makers)
      (setf (slot-value generic-function 'generalizer-makers)
            (make-generalizer-makers generic-function))))

(defmethod specializable:generalizers-of-using-class ((generic-function pattern-generic-function)
                                                      (args list))
  (let* ((generalizer-makers (generic-function-generalizer-makers
                             generic-function))
        (next-generalizers (make-list (length generalizer-makers)) #+no (call-next-method))) ; TODO avoid when possible
    #+fast-path (mapcar #'funcall args generalizer-makers)
    (mapcar (lambda (arg maker next)
              (let ((generalizer (funcall maker arg)))
                (setf (pattern-generalizer-next generalizer) next)
                generalizer))
            args generalizer-makers next-generalizers)))
