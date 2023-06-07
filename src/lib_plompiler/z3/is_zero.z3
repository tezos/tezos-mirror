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

;; is_zero x = if x = 0 then 1 else 0
;;
;; is_zero x is defined as b such that:
;;   b * (1 - b) = 0
;;   b + x * r = 1 for some non-zero r

(declare-const x Real)
(declare-const b Real)
(declare-const r Real)

;; Specification
(define-fun specification () Bool (and (=> (= x 0) (= b 1))
                                       (=> (not (= x 0)) (= b 0))
                                  ))

;; Constraints
(define-fun constraints () Bool (and (= 0 (* b (- 1 b)))
                                     (= 1 (+ b (* x r)))
                                     (not (= 0 r))
                                ))

;; Check soundness
(assert (not (=> constraints specification)))

(check-sat)
;; unsat
;; (get-model)
