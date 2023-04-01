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

;; ifthenelse b x y =
;;   if b = 1 then x
;;   else y
;;
;; (Our implementation works under the precondition that b is a bool.)

(declare-const b Real)
(declare-const x Real)
(declare-const y Real)
(declare-const z Real)

;; Specification

(define-fun pre-condition () Bool (or (= x 0) (= x 1)))

(define-fun specification () Bool (and (=> (= 1 b) (= z x))
                                       (=> (not (= 1 b)) (= z y))
                                  ))

(define-fun post-condition () Bool (or (= x z) (= y z)))

;; Constraints
(define-fun constraints () Bool (= z (+ (* x b) (* y (- 1 b)))))

;; Check soundness
(assert (not (=> constraints specification)))

;; Check correctness
(assert (not (=> (and pre-condition specification) post-condition)))

(check-sat)
;; unsat
;; (get-model)
