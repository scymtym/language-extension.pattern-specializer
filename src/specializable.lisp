;;;; pattern-specializer.lisp --- Implementation of pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

(defun specializer-same-pattern-p (specializer1 specializer2)
  (equal (specializer-pattern specializer1)
         (specializer-pattern specializer2)))

(defun in-same-cluster-p (specializer1 specializer2)
  (let ((pattern1 (specializer-parsed-pattern specializer1))
        (pattern2 (specializer-parsed-pattern specializer2)))
    (or (specializer-same-pattern-p specializer1 specializer2)
        (pattern-more-specific-p pattern1 pattern2)
        (pattern-more-specific-p pattern2 pattern1))))

(defun cluster-specializers (specializers)
  (let ((clusters '()))
    (dolist (specializer specializers)
      (dolist (cluster clusters (push (list (list specializer)) clusters))
        (when (every (lambda (entry) (in-same-cluster-p specializer (first entry))) cluster)
          (dolist (entry cluster (nconcf cluster (list (list specializer))))
            (when (specializer-same-pattern-p specializer (first entry))
              (nconcf entry (list specializer))
              (return)))
          (return))))
    (mapcar (lambda (cluster)
              (stable-sort cluster (lambda (entry1 entry2)
                                     (pattern-more-specific-p entry1 entry2))
                           :key (compose #'specializer-parsed-pattern #'first)))
            clusters)))

(defun make-specializer-bitmap (specializers)
  (let ((bitmap (make-array 8 :element-type 'bit)))
    (dolist (specializer specializers)
      (aref bitmap (specializer-index specializer)))
    bitmap))

(defun make-generalizer-maker (gf methods)
  (let ((arity (when-let ((first-method (first methods)))
                 (length (method-specializers first-method)))) ; TODO improve
        (clusters (cluster-methods gf methods)))
    (labels ((specializer-pattern1 (specializer)
               (typecase specializer
                 (pattern-specializer (specializer-pattern specializer))
                 (t                   '*)))
             (cluster-clause (specializers)
               (let ((bitmap (make-specializer-bitmap specializers))
                     (variables (specializer-pattern-variables (first specializers))))
                 `(,(case arity
                      (1 (specializer-pattern1 (first specializers)))
                      (t (mapcar #'specializer-pattern1 specializers)))
                   (make-pattern-generalizer bitmap (list ,@variables)))))
             (cluster-clauses (cluster)
               (loop :for ((head-first . head-rest) . rest) :on cluster
                     :collect (cluster-clause ()))))
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

(defun make-generalizer-maker-form (specializers)
  (let ((clusters (cluster-specializers specializers)))
    (labels ((cluster-element-clause (element rest)
               (let* ((specializer (first element))
                      (variables (specializer-pattern-variables specializer)))
                 `(,(specializer-pattern specializer)
                   (make-pattern-generalizer
                    '(,@(mappend #'identity (list* element rest)))
                    ',(specializer-pattern specializer)
                    (list ,@(loop :for variable in variables
                                  :collect (make-keyword variable)
                                  :collect variable))))))
             (cluster-clauses (cluster)
               (loop :for (element . rest) :on cluster
                     :collect (cluster-element-clause element rest))))
      `(lambda (arg)
         (optima:match arg
           ,@(loop :for cluster :in clusters
                   :appending (cluster-clauses cluster))
           (t ,(make-pattern-generalizer '() nil '())))))))

(defun make-generalizer-maker (specializers)
  (compile nil (make-generalizer-maker-form specializers)))

(defun make-generalizer-makers (generic-function)
  (let* ((methods (generic-function-methods generic-function))
         (arity (when-let ((first-method (first methods)))
                  (length (method-specializers first-method)))) ; TODO improve
         )
    (loop :for i :below arity
          :collect (make-generalizer-maker
                    (mapcar (lambda (method) (nth i (method-specializers method))) methods)))))

(let ((gf (fdefinition 'pattern-specializer.examples.lambda-calculus::eval1))
      (arg pattern-specializer.examples.lambda-calculus::(make-app (make-abst (make-var :foo) (make-value 1)) (make-value 2))))
 (print
  (time
   (funcall (first (generic-function-generalizer-makers gf)) arg))))
