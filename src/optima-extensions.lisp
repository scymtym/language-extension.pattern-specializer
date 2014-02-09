;;;; optima-extensions.lisp --- Necessary extensions of the optima library.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

(defgeneric pattern-more-specific-p (pattern1 pattern2)
  (:documentation
   "Return true if PATTERN1 is strictly more specific than
    PATTERN2.

    General principles:

    * Variable patterns are less specific than all other patterns

    * For most complex patterns, subpatterns are compared
      lexicographically. Exceptions:

      * For `class-pattern' s, subclass relations have higher
        precedence. The above rule applies only when the classes are
        identical.

      * `and-pattern's are comparable to all patterns by checking
        whether some of their subpatterns are more specific than the
        pattern in question.

      * `or-pattern's are similar."))

(defmethod pattern-more-specific-p :around ((pattern1 optima::pattern)
                                            (pattern2 optima::pattern))
  (unless (eq pattern1 pattern2)
    (call-next-method)))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::pattern))
  nil)

;; `variable-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::variable-pattern))
  t)

(defmethod pattern-more-specific-p ((pattern1 optima::variable-pattern)
                                    (pattern2 optima::variable-pattern))
  nil)

;; `and-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima::and-pattern)
                                    (pattern2 optima::pattern))
  (some (lambda (subpattern)
          (pattern-more-specific-p subpattern pattern2))
        (optima::complex-pattern-subpatterns pattern1)))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::and-pattern))
  (some (lambda (subpattern)
          (pattern-more-specific-p pattern1 subpattern))
        (optima::complex-pattern-subpatterns pattern2)))

;; `or-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima::or-pattern)
                                    (pattern2 optima::pattern))
  (every (lambda (subpattern)
           (pattern-more-specific-p subpattern pattern2))
         (optima::complex-pattern-subpatterns pattern1)))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::or-pattern))
  (every (lambda (subpattern)
           (pattern-more-specific-p pattern1 subpattern))
         (optima::complex-pattern-subpatterns pattern2)))

;; `cons-pattern'

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

;; `class-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima::class-pattern)
                                    (pattern2 optima::class-pattern))
  (let ((class1 (optima::class-pattern-class-name pattern1))
        (class2 (optima::class-pattern-class-name pattern2)))
    (multiple-value-bind (result1 certain1-p) (subtypep class1 class2)
      (multiple-value-bind (result2 certain2-p) (subtypep class2 class1)
        (assert (and certain1-p certain2-p))
        (cond
          ((and result1 result2)
           ;; TODO this will be call-next-method => method for complex-pattern-sub-patterns
           (loop :for subpattern1 :in (optima::complex-pattern-subpatterns pattern1) ; TODO permutations
                 :for subpattern2 :in (optima::complex-pattern-subpatterns pattern2)
                 :do (cond
                       ((pattern-more-specific-p subpattern1 subpattern2)
                        (return t))
                       ((pattern-more-specific-p subpattern2 subpattern1)
                        (return nil)))))
          (result1
           t)
          (t
           nil))))))

;; `structure-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima::structure-pattern)
                                    (pattern2 optima::structure-pattern))
  (error "not implemented"))
