;; MIT License
;; Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the Software), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED AS'$'IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; [assert_upper_bounded x] is satisfiable only if 0 <= x < 13
;;
;; 13 is 1101 in binary
;; Implemented as:
;;   b0 * (1 - b0) = 0
;;   b1 * (1 - b1) = 0
;;   b2 * (1 - b2) = 0
;;   b3 * (1 - b3) = 0
;;   x = b0 + 2 * b1 + 4 * b2 + 8 * b3
;;   not (b3 and (b2 and (b1 or (b0 and true))))
;;            1       1       0      1

(declare-const x Real)
(declare-const b0 Real)
(declare-const b1 Real)
(declare-const b2 Real)
(declare-const b3 Real)

;; Specification
(define-fun specification () Bool (and (<= 0 x) (< x 13)))

;; Constraints
(define-fun constraints () Bool
  (and (= 0 (* b0 (- 1 b0)))
  (and (= 0 (* b1 (- 1 b1)))
  (and (= 0 (* b2 (- 1 b2)))
  (and (= 0 (* b3 (- 1 b3)))
  (and (= x (+ b0 (+ (* 2 b1) (+ (* 4 b2) (* 8 b3)))))
       (not (and (= 1 b3) (and (= 1 b2) (or (= 1 b1) (= 1 b0)))))
  ))))))

;; Check soundness
(assert (not (=> constraints specification)))

(check-sat)
;; unsat
;; (get-model)
