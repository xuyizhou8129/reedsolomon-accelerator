# Reed-Solomon Encoding

## The Big Picture

Reed-Solomon (RS) encoding takes a **message** and transforms it into a longer **codeword** that has built-in redundancy — extra information that lets a receiver detect and fix errors later.

- Your message has **k** symbols.
- The final codeword has **n** symbols, where **n > k**.
- The extra **n − k** symbols are the redundancy (called the "remainder").

---

## Step 1: Represent Your Message as a Polynomial

Take your message symbols `m₀, m₁, ..., m_{k-1}` and treat them as the **coefficients** of a polynomial:

```
p(x) = m₀ + m₁x + m₂x² + ... + m_{k-1}x^{k-1}
```

So instead of working with a list of numbers, you work with a single polynomial of degree k−1.

---

## Step 2: Evaluate the Polynomial at n Points (Matrix / Direct Way)

Pick **n distinct points** `a₀, a₁, ..., a_{n-1}` (these are fixed and known to both sender and receiver).

Evaluate `p(x)` at each point:

```
C(m) = [ p(a₀), p(a₁), ..., p(a_{n-1}) ]
```

This can be written as a **matrix multiplication**:

```
C(m) = Δ · m
```

where **Δ** is the Vandermonde matrix built from your chosen points. The encoded codeword is the result.

> **Key insight:** Any polynomial of degree n−1 is *uniquely* determined by any n points on it (Lagrange interpolation). This is what makes recovery possible even after errors.

---

## Step 3: Polynomial Division Way (Alternative Encoding)

This is another way to build the codeword that makes the decoding math cleaner.

1. **Build a generator polynomial** from n−k chosen roots `x₀, x₁, ..., x_{n-k-1}`:
   ```
   g(x) = (x − x₀)(x − x₁)···(x − x_{n-k-1})
   ```
   When expanded: `g(x) = g₀ + g₁x + ... + g_{n-k}x^{n-k}`, with the leading coefficient = 1.

2. **Shift the message polynomial** up by n−k degrees:
   ```
   p(x) · x^{n-k}
   ```

3. **Divide** by g(x):
   ```
   p(x) · x^{n-k} = q(x)·g(x) + r(x)
   ```
   The remainder `r(x)` has degree at most n−k−1:
   ```
   r(x) = r₀ + r₁x + ... + r_{n-k-1}x^{n-k-1}
   ```

4. **Define the codeword polynomial:**
   ```
   S(x) = p(x)·x^{n-k} − r(x)
   ```

> **Why this works:** Subtracting `r(x)` from `p(x)·x^{n-k}` makes `S(x)` exactly divisible by `g(x)`. This means the roots of `g(x)` are also roots of `S(x)` — a property the decoder will check.

The low-order coefficients of `S(x)` (positions `x⁰` through `x^{n-k-1}`) are the remainder/redundancy. The high-order coefficients (positions `x^{n-k}` through `x^{n-1}`) are just the original message — unchanged.

---

## Summary of What Gets Sent

| Part of S(x) | Contents |
|---|---|
| High-degree coefficients (`x^{n-k}` to `x^{n-1}`) | Original message symbols |
| Low-degree coefficients (`x⁰` to `x^{n-k-1}`) | Redundancy / remainder |

The receiver gets all **n** coefficients and uses the redundancy to detect and correct errors.
