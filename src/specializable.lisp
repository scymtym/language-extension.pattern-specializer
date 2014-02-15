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
