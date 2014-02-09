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
