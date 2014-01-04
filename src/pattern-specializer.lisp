;;;; pattern-specializer.lisp --- Implementation of pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

;;;

(defgeneric pattern-more-specific-p (pattern1 pattern2))

(defmethod pattern-more-specific-p :around ((pattern1 optima::pattern)
                                            (pattern2 optima::pattern))
  (unless (eq pattern1 pattern2)
    (call-next-method)))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::pattern))
  nil)

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::variable-pattern))
  t)

(defmethod pattern-more-specific-p ((pattern1 optima::variable-pattern)
                                    (pattern2 optima::variable-pattern))
  nil)

; TODO do this in a generic way via optima::complex-pattern-subpatterns
(defmethod pattern-more-specific-p ((pattern1 optima::cons-pattern)
                                    (pattern2 optima::cons-pattern))
  (let ((car1 (optima::cons-pattern-car-pattern pattern1))
        (cdr1 (optima::cons-pattern-cdr-pattern pattern1))
        (car2 (optima::cons-pattern-car-pattern pattern2))
        (cdr2 (optima::cons-pattern-cdr-pattern pattern2)))
    (or (pattern-more-specific-p car1 car2)
        (and (not (pattern-more-specific-p car2 car1))
             (pattern-more-specific-p cdr1 cdr2)))))

;;; `pattern-specializer' class

(defclass pattern-specializer (specializer)
  ((pattern        :initarg  :pattern
                   :reader   specializer-pattern)
   (direct-methods :type     list
                   :initform '()
                   :reader   specializer-direct-methods
                   :accessor specializer-%direct-methods))
  (:default-initargs
   :pattern (required-argument :pattern)))

(defun specializer-parsed-pattern (specializer)
  (optima::parse-pattern (specializer-pattern specializer)))

(defun specializer-pattern-variables (specializer)
  (optima::pattern-variables (specializer-parsed-pattern specializer)))

;; TODO why did i need this again?
(defmethod class-name ((class (eql (find-class 'pattern-specializer))))
  'pattern-specializer)

(defmethod add-direct-method ((specializer pattern-specializer)
                              (method      t))
  (pushnew method (specializer-%direct-methods specializer)))

(defmethod remove-direct-method ((specializer pattern-specializer)
                                 (method      t))
  (removef (specializer-%direct-methods specializer) method :count 1))

(defmethod print-object ((object pattern-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specializer-pattern object) stream)))

;;;

(defvar *pattern-specializer-table*
  (make-hash-table :test 'equal :weakness :key-and-value))

(defun ensure-pattern-specializer (pattern)
  (ensure-gethash pattern *pattern-specializer-table*
                  (make-instance 'pattern-specializer :pattern pattern)))

;;; pattern-method

;; Forward definition. Actual definition is below.
(defclass pattern-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defclass pattern-method (standard-method)
  ())

(defmethod method-pattern-specializers ((gf pattern-generic-function)
                                        (method pattern-method))
  (remove-if-not (of-type 'pattern-specializer)
                 (mapcar (curry #'parse-specializer-using-class gf)
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
  ;; TODO obviously, this has to parse the original lambda-list
  ;; properly in the future.
  (destructuring-bind (lambda lambda-list &body body) lambda-expression
    (declare (ignore lambda))
    (flet ((make-keyword-parameter (variable)
             (list `((,(make-keyword variable) ,variable)))))
      (let* ((variables (mappend #'specializer-pattern-variables ; TODO this stuff is repeated in make-method-matching-form
                                 (remove-if-not (of-type 'pattern-specializer)
                                                (mapcar (curry #'parse-specializer-using-class gf)
                                                        specializers))))
             (new-lambda-list (append
                               lambda-list
                               (list* '&key (mapcan #'make-keyword-parameter variables))
                               (list '&allow-other-keys)))
             (new-lambda-expression `(lambda ,new-lambda-list ,@body)))

        (format t "make-method-lambda-using-specializers~%  ~A~%  ~A~%  ~A~%  ~A~%=>"
               gf method specializers lambda-expression)
        (print new-lambda-list)
        (print new-lambda-expression)

        (multiple-value-call #'values
          (call-next-method
           gf method qualifiers specializers new-lambda-expression environment)
          new-lambda-list)))))

(defgeneric method-more-specific-p (gf method1 method2))

(defmethod method-more-specific-p ((gf pattern-generic-function)
                                   (method1 pattern-method)
                                   (method2 pattern-method))
  (let* ((specializers1 (method-pattern-specializers gf method1))
         (specializers2 (method-pattern-specializers gf method2))
         (more-index (mismatch specializers1 specializers2
                               :test (complement #'pattern-more-specific-p)
                               :key #'specializer-parsed-pattern))
         (less-index (mismatch specializers1 specializers2
                               :test #'pattern-more-specific-p
                               :key #'specializer-parsed-pattern)))
    (or (and more-index (not less-index))
        (and more-index (< more-index less-index)))))

(defun in-same-cluster-p (gf method1 method2)
  (or (equal (mapcar #'specializer-pattern
                     (method-pattern-specializers gf method1))
             (mapcar #'specializer-pattern
                     (method-pattern-specializers gf method2)))
      (method-more-specific-p gf method1 method2)
      (method-more-specific-p gf method2 method1)))

(defun cluster-methods (gf methods)
  (let ((clusters '()))
    (dolist (method1 methods)
      (dolist (cluster clusters (push (list (list method1)) clusters))
        (when (every (lambda (entry) (in-same-cluster-p gf method1 (first entry)))
                     cluster)
          (dolist (entry cluster
                         (nconcf cluster (list (list method1))))
            (when (equal (mapcar #'specializer-pattern ; TODO repeated in in-same-cluster-p
                                 (method-pattern-specializers gf method1))
                         (mapcar #'specializer-pattern
                                 (method-pattern-specializers gf (first entry))))
              (nconcf entry (list method1))
              (return)))
          (return))))
    (mapcar (lambda (cluster)
              (stable-sort cluster (lambda (entry1 entry2)
                                     (method-more-specific-p gf (first entry1) (first entry2)))))
            clusters)))

;;; pattern-generic-function

(defclass pattern-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :method-class (find-class 'pattern-method)))

(defmethod parse-specializer-using-class
    ((gf pattern-generic-function) (specializer-name t))
  (if (typep specializer-name '(cons (eql pattern)))
      (let ((pattern (second specializer-name)))
        (ensure-pattern-specializer pattern))
      (call-next-method)))

(defmethod parse-specializer-using-class
    ((gf pattern-generic-function) (specializer-name pattern-specializer))
  specializer-name)

(defmethod unparse-specializer-using-class
    ((gf pattern-generic-function) (specializer pattern-specializer))
  `(pattern ,(specializer-pattern specializer)))

(defmethod make-method-specializers-form
    ((gf pattern-generic-function) method snames env)
  (flet ((make-parse-form (name)
           `(parse-specializer-using-class
             (fdefinition ',(generic-function-name gf)) ',name)))
    `(list ,@(mapcar #'make-parse-form snames))))

(defun make-matching-lambda-form (gf methods)
  (labels ((specializer-pattern1 (specializer)
             (typecase specializer
               (pattern-specializer (specializer-pattern specializer))
               (t                   '*)))
           (method-variables (method)
             (mappend #'specializer-pattern-variables
                      (method-pattern-specializers gf method)))
           (cluster-clause (most-specific-method other-methods)
             (let ((specializers (method-specializers most-specific-method)))
               `((list ,@(mapcar #'specializer-pattern1 specializers))
                 (list '(,most-specific-method ,@other-methods)
                       (list ,@(method-variables most-specific-method))))))
           (cluster-clauses (cluster)
             (loop :for ((head-first . head-rest) . rest) :on cluster
                   :collect (cluster-clause
                             head-first (reduce #'append rest
                                                :initial-value head-rest)))))
    (let ((clusters (cluster-methods gf methods)))
      `(lambda (&rest args)
         (format t "dispatch: ~A~%" args)
         (list
          ,@(loop :for cluster :in clusters
                  :collect `(optima:match args
                              ,@(cluster-clauses cluster))))))))

(defun make-method-interpreting-function (gf)
  (format t "~&method-interpreting-function: ~A~%" gf)
  (let* ((methods (generic-function-methods gf))
         (f (compile nil (print (make-matching-lambda-form gf methods)))))
    (named-lambda method-pattern-matching-function (&rest args) ; TODO just return the (compile …) above after debugging
      (apply f args))))

(defmethod compute-discriminating-function
    ((gf pattern-generic-function))
  (lambda (&rest args)
    (format t "~&discriminating function: ~A~%" args)
    (labels ((make-effective-method-form (spec)
               `(lambda (&rest args)
                  (locally
                      (declare (sb-ext:disable-package-locks make-method call-method))
                    (macrolet ((make-method (spec)
                                 (let ((make-effective-method-function ,#'make-effective-method-function))
                                   (make-instance 'standard-method
                                                  :specializers nil ; TODO
                                                  :qualifiers nil ; TODO
                                                  :function (let ((f (funcall make-effective-method-function spec)))
                                                              (lambda (a n)
                                                                (apply f a))))))
                               (call-method (method next-methods)
                                 ;; TODO we could do method-specific parsing here
                                 `(progn
                                    (format t "~& trying to call~%  ~A~%  ~A~%  ~A~%"
                                            ,method args (list ,@next-methods))
                                    (funcall (method-function ,method) args (list ,@next-methods)))))
                      ,spec))))
             (make-effective-method-function (spec)
               (compile nil (make-effective-method-form spec))))
      (let* ((function2     (make-method-interpreting-function gf))
             (function4     (lambda (&rest args)
               (let* ((methods '())
                      (variables '())
                      (function3 (progn
                                   (loop :for (methods1 variables1) :in (apply function2 args)
                                         :do (appendf methods methods1)
                                             (appendf variables variables1))

                                   (format t "~&  methods~%  ~A~&  variables~&  ~A~%" methods variables)
                                   (multiple-value-bind (effective-method options)
                                       (compute-effective-method
                                        gf (sb-mop::generic-function-method-combination gf) methods)
                                     (format t "~&  effective method:~&  ")
                                     (print effective-method)
                                     (format t "~&  options:~&  ")
                                     (print options)
                                     (make-effective-method-function effective-method)))))
                 (apply function3 (append args (loop :for value :in variables
                                                     :for name :in (when methods
                                                                     (mappend
                                                                      #'specializer-pattern-variables
                                                                      (method-pattern-specializers gf (first methods))))
                                                     :collect (make-keyword name)
                                                     :collect value)))))))
        (set-funcallable-instance-function gf function4)
        (apply function4 args)))))
