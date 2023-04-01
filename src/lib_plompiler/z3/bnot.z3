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

;; bnot x = if x = 0 then 1 else 0

(declare-const x Real)
(declare-const b Real)

;; Specification
(define-fun pre-condition () Bool (or (= x 0) (= x 1)))

(define-fun specification () Bool (and (=> (= 0 x) (= 1 b))
                                       (=> (= 1 x) (= 0 b))
                                  ))

(define-fun post-condition () Bool (or (= b 0) (= b 1)))

;; Constraints
(define-fun constraints () Bool (= b (- 1 x)))

;; Check soundness
(assert (not (=> constraints specification)))

;; Check correctness
(assert (not (=> (and pre-condition specification) post-condition)))

(check-sat)
;; unsat
;; (get-model)
