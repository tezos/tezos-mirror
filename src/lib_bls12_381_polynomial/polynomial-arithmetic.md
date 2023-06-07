- Polynomial representation
  ```
  a(X) = a_{n-1} * X^{n-1} + ... + a_1 * X + a_0
  ```
  - The **coefficient** representation
    - a **dense** representation stores zero coefficients explicitly and exponents implicitly
      - e.g., `a = [a_0; a_1; ...; a_{n-1}]`
    - a **sparse** representation doesn't store zero coefficients at all, but stores exponents explicitly
      - e.g., `a = [(a_0, 0); (a_1, 1); ...; (a_{n-1}, n-1)]`  where `a_i <> zero for i = 0..(n-1)`
  - The **evaluation** representation `[a(one); a(g); a(g^2); ..; a(g^{n-1})]`, where
    - `g` is a primitive `n`-th root of unity, i.e., `g^n = one` and `g^i <> one` for `i=1..(n-1)`
    - domain = `[one; g; g^2; g^3; ..; g^{n-1}]`, `n > 0`
  - Converting between the coefficient and evaluation representations of a polynomial `a`
    - size of domain must be a power of two
    - `evaluation_fft`: `[a_0; a_1; ...; a_{n-1}]` -> `[a(one); a(g); a(g^2); ..; a(g^{n-1})]`
    - `interpolation_fft`: `[a(one); a(g); a(g^2); ..; a(g^{n-1})]` -> `[a_0; a_1; ...; a_{n-1}]`
    - Some reading
      - https://zcash.github.io/halo2/background/polynomials.html
      - https://vitalik.ca/general/2019/05/12/fft.html
      - https://dsprenkels.com/ntt.html
      - https://maths-people.anu.edu.au/~brent/pd/mca-cup-0.5.9.pdf (Section 2.3)
- The degree of the polynomial is the highest exponent occurring in the polynomial
  - e.g., if `a_{n-1} <> 0` then the degree of `a` is `(n-1)`
  - **special case**: the degree of the zero polynomial is either left undefined, or is defined to be negative (usually, `-1`)
- Polynomial equality, `n >= m`
  ```
  a_{n-1} ............... a_1 a_0 | a
              b_{m-1} ... b_1 b_0 | b
  ```
  ```
  a == b <==> (a_i = b_i for i = 0..(m-1) /\ a_i = zero for i = m..(n-1))
  ```
- Polynomial addition, `n >= m`
  ```
  a_{n-1} ............... a_1 a_0 | a
              b_{m-1} ... b_1 b_0 | b
  ------------------------------- |---
  c_{n-1} ............... c_1 c_0 | c
  ```
  ```
  c_i = a_i + b_i, for i = 0..(m-1)
  c_i = a_i, for i = m..(n-1)
  ```
- Polynomial subtraction, `n >= m`
  ```
  a_{n-1} ............... a_1 a_0 | a
              b_{m-1} ... b_1 b_0 | b
  ------------------------------- |---
  c_{n-1} ............... c_1 c_0 | c
  ```
  ```
  c_i = a_i - b_i, for i = 0..(m-1)
  c_i = a_i, for i = m..(n-1)
  ```
- Polynomial subtraction, `n < m`
  ```
              a_{n-1} ... a_1 a_0 | a
  b_{m-1} ............... b_1 b_0 | b
  ------------------------------- |---
  c_{m-1} ............... c_1 c_0 | c
  ```
  ```
  c_i = a_i - b_i, for i = 0..(n-1)
  c_i =     - b_i, for i = n..(m-1)
  ```
- Polynomial multiplication; **Requires for `mul`:** initialize `c` with zeros
  ```
                                     a_{n-1}     ........................ a_1     a_0 | a
                                                          b_{m-1} ....... b_1     b_0 | b
                                     ------------------------------------------------ |---
                                 a_{n-1}*b_0      ................... a_1*b_0 a_0*b_0 | a*b_0
                     a_{n-1}*b_1      ....................... a_1*b_1 a_0*b_1         | a*b_1*X
                                     .....
  a_{n-1}*b_{m-1} ...................................a_0*b_{m-1}                      | a*b_{m-1}*X^{m-1}
  ----------------------------------------------------------------------------------- |---
  c_{n+m-2}            .........     c_{n-1}     .............     c2     c_1     c_0 | c
  ```
  ```
  c_{i+j} += a_i * b_j for i = 0..(n-1) and j = 0..(m-1)
  ```
- Multiplying a polynomial `a` by a scalar `b`
  ```
  a_{n-1} ............... a_1 a_0 | a
                              b_0 | b
  ------------------------------- |---
  c_{n-1} ............... c_1 c_0 | c
  ```
  ```
  c_i = a_i * b_0 for i = 0..(n-1)
  ```
- Checking whether a polynomial is the zero polynomial
  ```
  a_{n-1} ............... a_1 a_0 | a
  ```
  ```
  a = poly_zero <==> a_i = zero for i = 0..(n-1)
  ```
- Polynomial evaluation
  ```
  a(X) = a_{n-1} * X^{n-1} + .. + a_1 * X + a_0
  ```
  can be computed as follows
  ```
  a(X) = ((..(a_{n-1} * X + a_{n-2}) * X + ...) * X + a_1) * X + a_0
  ```
- Dividing a polynomial `a` by `(X^n + c)`. **Note** that by definition R(X) is either equal to zero or degree R(X) < n.
  ```
  (X^n + c) * (b_{l-n-1} * X^{l-n-1} + .. + b_1 * X + b_0) + R(X) = a_{l-1} * X^{l-1} + .. + a_1 * X + a_0
  ```
  **Case 1:** `l-n-1 >= n`, i.e., `l-1 >= 2n`
  ```
  lhs: b_{l-n-1} * X^{l-1} + .. + b_1 * X^{n+1} + b_0 * X^n + c*b_{l-n-1} * X^{l-n-1} + .. + c*b_1 * X + c*b_0 + R(X)
       X^{l-1}   .... X^{l-n} | X^{l-n-1} ...     X^n | X^{n-1} ...     X^1   X^0
      ---------------------------------------------------------------------------
       a_{l-1}    ... a_{l-n} | a_{l-n-1} ...     a_n | a_{n-1} ...     a_1   a_0
      ---------------------------------------------------------------------------
   `+` b_{l-n-1} ... b_{l-2n} | b_{l-2n-1}        b_0 |  0 ....           0     0
   `+` 0              .... 0  | c*b_{l-n-1} ... c*b_n | c*b_{n-1} ... c*b_1 c*b_0
   `+` 0              .... 0  | 0           ...     0 | r_{n-1}  ...    r_1   r_0
      ---------------------------------------------------------------------------
  ```
  - quotient
  ```
  a_i = b_{i-n} for i in [l-n; l-1] <==> b_i = a_{i+n} for i in [l-2n; l-n-1]
  a_i = b_{i-n} + c*b_i for i in [n; l-n-1] ==> b_{i-n} = a_i - c*b_i <==> b_i = a_{i+n} - c*b_{i+n} for i in [l-2n-1; 0]
  ```
  - remainder R(X)
  ```
  a_i = r_i + c*b_i for i in [0; n-1] ==> r_i = a_i - c*b_i for i in [0; n-1]
  ```
  **Case 2:** `l-n-1 < n`, i.e., `l-1 < 2n` (but `n <= l-1`)
  ```
  lhs: b_{l-n-1} * X^{l-1} + .. + b_1 * X^{n+1} + b_0 * X^n + c*b_{l-n-1} * X^{l-n-1} + .. + c*b_1 * X + c*b_0 + R(X)
       X^{l-1}  .....  X^n | X^{n-1} ...   X^{l-n} | X^{l-n-1}  ...       X^1   X^0
      ------------------------------------------------------------------------------
       a_{l-1}  .....  a_n | a_{n-1} ...   a_{l-n} | a_{l-n-1}  ...       a_1   a_0
      ------------------------------------------------------------------------------
   `+` b_{l-n-1}  ...  b_0 | 0       ...         0 | 0             ...      0     0
   `+` 0       ....      0 | 0       ...         0 | c*b_{l-n-1}   ...  c*b_1 c*b_0
   `+` 0       ....      0 | r_{n-1}       r_{l-n} |   r_{l-n-1}   ...    r_1   r_0
      ------------------------------------------------------------------------------
  ```
  - quotient
  ```
  a_i = b_{i-n} for i in [n; l-1] <==> b_i = a_{i+n} for i in [0; l-n-1]
  ```
  - remainder R(X)
  ```
  a_i = r_i + c*b_i for i in [0; l-n-1] ==> r_i = a_i - c*b_i for i in [0; l-n-1]
  r_i = a_i for i in [l-n; n-1]
  ```
- Multiplying a polynomial `a` by `(X^n + c)`
  ```
  (X^n + c) * (a_{l-1} * X^{l-1} + .. + a_1 * X + a_0) = b_{l+n-1} * X^{l+n-1} + .. + b_2 * X^2 + b_1 * X + b_0
  ```
  **Case 1:** `l-1 >= n`
  ```
  lhs: a_{l-1} * X^{n+l-1} + .. + a_1 * X^{n+1} + a_0 * X^n + c*a_{l-1} * X^{l-1} + .. + c*a_1 * X + c*a_0
         X^{l+n-1} ...| X^{l-1} .......X^n |...    X^1   X^0
       -----------------------------------------------------
         b_{l+n-1} ...| b_{l-1} .......b_n |....   b_1   b_0
       -----------------------------------------------------
  `+`    a_{l-1} ..   | a_{l-n-1}   .. a_0 | 0 0    .....  0
  `+`    0   .... 0 0 | c*a_{l-1} .. c*a_n |...  c*a_1 c*a_0
       -----------------------------------------------------
  ```
  ```
  b_i = c*a_i for i in [0; n-1]
  b_i = a_{i-n} + c*a_i for i in [n; l-1]
  b_i = a_{i-n} for i in [l; l+n-1]
  ```
  **Case 2:** `l-1 < n`
  ```
  lhs: a_{l-1} * X^{n+l-1} + .. + a_1 * X^{n+1} + a_0 * X^n + c*a_{l-1} * X^{l-1} + .. + c*a_1 * X + c*a_0
         X^{l+n-1} ... X^n | X^{n-1} ...... X^l | X^{l-1} ...     X^1   X^0
        --------------------------------------------------------------------
         b_{l+n-1} ... b_n | b_{n-1} ...... b_l | b_{l-1} ...     b_1   b_0
        --------------------------------------------------------------------
  `+`    a_{l-1} ..    a_0 | 0  0         ... 0 | 0       ...     0       0
  `+`    0   ....        0 | 0      ....      0 | c*a_{l-1} ..  c*a_1 c*a_0
        --------------------------------------------------------------------
  ```
  ```
  b_i = c*a_i for i in [0;l-1]
  b_i = 0 for i in [l; n-1]
  b_i = a_{i-n} for i in [n; l+n-1]
  ```
## Arithmetic of polynomials evaluated on `domain`
- Rescale evaluations
  - `[g^0; g^1; ..; g^{k*n-1}]` -> `[g'^0; g'^1; ..; g'^{n-1}]`
  - `step = k*n/n = k`
  - `g'^j = g^{j*k}` for `j=0..(n-1)`
  - [Proof: `g'^j = e^{2*j*pi*i/n} = e^{2*(j*k)*pi*i/k*n} = g^{j*k}`]
  - `eval_out[j] = eval_in[j*step]`
- `composition_gx` computes `a(g^i*x)`, i.e., this is an evaluation of polynomial `a` at point `g^i*x`
  - `x = g^k` ==> `a(g^i*g^k) = a(g^(i+k))`
  - `eval_out[j] = eval_in[(j+i) % eval_len]`
  - [Proof: as `g^eval_len = one`, `g^{j+i} = g^{(j+i) % eval_len}`]
- Addition of polynomials evaluated on `domain`
  - `c(x) = a(x) + b(x) <==> eval_c[domain_i] = eval_a[domain_i * step_a] + eval_b[domain_i * step_b]`
  - degree of c(x) = max(degree of a(x), degree of b(x))
  - size of eval_c = min(size of eval_a, size of eval_b) [NOTE: degree of c(x) <= size of eval_c]
  - step_a = size of eval_a / size of eval_c
  - step_b = size of eval_b / size of eval_c
- Multiplication of a polynomial evaluated on `domain` by a scalar
  - `c(x) = lambda * a(x) <==> eval_c[domain_i] = lambda * eval_a[domain_i]`
  - degree of c(x) = degree of a(x)
  - size of eval_c = size of eval_a
- Multiplication of polynomials evaluated on `domain`
  - `c(x) = a(x) * b(x) <==> eval_c[domain_i] = eval_a[domain_i * step_a] * eval_b[domain_i * step_b]`
  - degree of c(x) = degree of a(x) + degree of b(x)
  - size of eval_c = min(size of eval_a, size of eval_b) [NOTE: degree of c(x) <= size of eval_c]
  - step_a = size of eval_a / size of eval_c
  - step_b = size of eval_b / size of eval_c
- `linear` computes `c(x) = lambda_0 * p_0(g^l_0*x) + lambda_1 * p_1(g^l_1*x) + .. + lambda_k * p_k(g^l_k*x) + constant`
  ```
  c(g^0)     = lambda_0 * p_0(g^l_0*g^0)     + ... + lambda_k * p_k(g^l_k*g^0)     + constant
  c(g^1)     = lambda_0 * p_0(g^l_0*g^1)     + ... + lambda_k * p_k(g^l_k*g^1)     + constant
  ...
  c(g^{n-1}) = lambda_0 * p_0(g^l_0*g^{n-1}) + ... + lambda_k * p_k(g^l_k*g^{n-1}) + constant
  ```
  - we don't go from `c(g^0)` to `c(g^{n-1})`, instead we accumulate the sum from `lambda_0 * p_0(g^l_0*x)` to `lambda_k * p_k(g^l_k*x)`
  - degree of c(x) = max(degree of p_i(x))
  - size of eval_c = min(size of eval_p_i) [NOTE: degree of c(x) <= size of eval_c]
  - step_p_i = size of eval_p_i / size of eval_c
  ```
  eval_c(domain_i) =
    lambda_0 * eval_p_0((domain_i + l_0) * step_p_0 % eval_p_0_len)
  + lambda_1 * eval_p_1((domain_i + l_1) * step_p_1 % eval_p_1_len)
  + ...
  + lambda_k * eval_p_k((domain_i + l_k) * step_p_k % eval_p_k_len) + constant
  ```
- `mul` computes `c(x) = p_0(g^l_0*x)^m_0 * p_1(g^l_1*x)^m_1 * ... * p_k(g^l_k*x)^m_k`
  ```
  c(g^0)     = p_0(g^l_0*g^0)^m_0     * p_1(g^l_1*g^0)^m_1     * ... * p_k(g^l_k*g^0)^m_k
  c(g^1)     = p_0(g^l_0*g^1)^m_0     * p_1(g^l_1*g^1)^m_1     * ... * p_k(g^l_k*g^1)^m_k
  ...
  c(g^{n-1}) = p_0(g^l_0*g^{n-1})^m_0 * p_1(g^l_1*g^{n-1})^m_1 * ... * p_k(g^l_k*g^{n-1})^m_k
  ```
  - we don't go from `c(g^0)` to `c(g^{n-1})`, instead we accumulate the product from `p_0(g^l_0*x)^m_0` to `p_k(g^l_k*x)^m_k`
  - degree of c(x) = `m_0 * degree of p_0 + ... + m_k * degree of p_k`
  - size of eval_c = min (size of eval_p_i) [NOTE: degree of c(x) <= size of eval_c]
  - step_p_i = size of eval_p_i / size of eval_c
  ```
  eval_c(domain_i) =
    eval_p_0((domain_i + l_0) * step_p_0 % eval_p_0_len)^m_0
  * eval_p_1((domain_i + l_1) * step_p_1 % eval_p_1_len)^m_1
  * ...
  * eval_p_k((domain_i + l_k) * step_p_k % eval_p_k_len)^m_k
  ```
