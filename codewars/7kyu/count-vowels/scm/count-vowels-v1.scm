;;
;; tags: codewars algorithm vowel count
;;

(import (only srfi-13 string-fold string-index string-contains-ci))

;;;;
;; Count the number of vowels in s.
;;
;(define (count-vowels str)
;  (string-fold (lambda (c acc)
;                 (if (or (string-index "aeiou" c)
;                         (string-index "AEIOU" c))
;                     (+ acc 1)
;                     acc))
;               0
;               str))

(define (count-vowels s)
  (string-fold (lambda (c acc)
                 (if (string-contains-ci "aeiou" (string c))
                     (+ acc 1)
                     acc))
               0
               s))
