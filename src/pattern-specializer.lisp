;;;; pattern-specializer.lisp --- Implementation of pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

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
  ;; TODO obviously, this has to parse the original lambda-list
  ;; properly in the future.
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

(defgeneric method-more-specific-p (gf method1 method2))

(defmethod method-more-specific-p ((gf      pattern-generic-function)
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

(defmethod make-specializer-form-using-class or
    ((proto-generic-function pattern-generic-function)
     (proto-method pattern-method)
     (specializer-name cons)
     (environment t))
  (when (typep specializer-name '(cons (eql pattern)))
    `(sb-pcl:parse-specializer-using-class ; TODO packages
      (sb-pcl:class-prototype (find-class ',(type-of proto-generic-function)))
      ',specializer-name)))

(defun make-matching-lambda-form (gf methods)
  (let ((arity (when-let ((first-method (first methods)))
                 (length (method-specializers first-method))))
        (clusters (cluster-methods gf methods)))
   (labels ((specializer-pattern1 (specializer)
              (typecase specializer
                (pattern-specializer (specializer-pattern specializer))
                (t                   '*)))
            (method-variables (method)
              (mappend #'specializer-pattern-variables
                       (method-pattern-specializers gf method)))
            (cluster-clause (most-specific-method other-methods)
              (let ((specializers (method-specializers most-specific-method)))
                `(,(case arity
                     (1 (specializer-pattern1 (first specializers)))
                     (t (mapcar #'specializer-pattern1 specializers)))
                  (values
                   '(,most-specific-method ,@other-methods)
                   (list ,@(method-variables most-specific-method))))))
            (cluster-clauses (cluster)
              (loop :for ((head-first . head-rest) . rest) :on cluster
                    :collect (cluster-clause
                              head-first (reduce #'append rest
                                                 :initial-value head-rest)))))
     `(lambda ,(case arity
                 (1 '(arg))
                 (t '(&rest args)))
        ,(case arity
           (1 '(format t "dispatch: ~A~%" arg))
           (t '(format t "dispatch: ~A~%" args)))
        (,@(case arity
             (1 `(optima:match arg))
             (t `(optima:multiple-value-match (values-list args))))
         ,@(loop :for cluster :in clusters
                 :appending (cluster-clauses cluster)))))))

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
                                 ;; TODO can we extract the method-function like ,(method-function method)?
                                 `(progn
                                    (format t "~& trying to call~%  ~A~%  ~A~%  ~A~%"
                                            ,method args (list ,@next-methods))
                                    (funcall (method-function ,method) args (list ,@next-methods)))))
                      ,spec))))
             (make-effective-method-function (spec)
               (compile nil (make-effective-method-form spec))))
      (let* ((function2     (make-method-interpreting-function gf))
             (function4     (lambda (&rest args)
               (multiple-value-bind (methods variables) (apply function2 args)

                 (loop :for spec :in (method-pattern-specializers gf (first methods))
                       :for gen :in (mapcar #'class-of args)
                       :do (print (list spec gen (multiple-value-list (specializer-accepts-generalizer-p
                                                                       gf spec gen)))))

                (let ((function3 (progn
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
                                                      :collect value))))))))
        (set-funcallable-instance-function gf function4) ; TODO seems to be wrong
        (apply function4 args)))))
