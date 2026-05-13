# RS Decoder Workflow

This document describes the Reed-Solomon (RS) decoder software model located in
`software/software-model/Decoder/`. The decoder targets a systematic RS(15, 11)
code operating in **GF(2⁸)** with irreducible polynomial x⁸ + x⁴ + x³ + x + 1
(`0b100011101`).

---

## Overview

An RS(N, K) code with N = 15, K = 11 can correct up to `t = (N − K) / 2 = 2`
symbol errors. The decoder receives a 15-symbol codeword (polynomial with
coefficients in GF(2⁸)) and either confirms it is error-free or locates and
corrects up to 2 corrupted symbols.

High-level pipeline:

```
Received codeword
      │
      ▼
 1. check_roots        ─── compute syndromes S₁…S₄
      │ corrupted?
     yes
      │
      ▼
 2. comb_indices       ─── enumerate candidate error positions
      │
      ▼
 3. zero_out_columns   ─── mask Vandermonde matrix to assumed positions
      │
      ▼
 4. solve_matrix       ─── GF Gaussian elimination → error magnitudes
      │
      ▼
   error vector e[]  →  corrected codeword = received ⊕ e
```

---

## Step 1 – Syndrome Computation (`check_roots.py`)

**Function:** `check_roots(coeffs, roots_to_check, gf)`

The received polynomial `r(x) = c₀ + c₁x + c₂x² + … + c₁₄x¹⁴` is evaluated
at each of the `t·2 = 4` known roots of the generator polynomial (α¹, α², α³,
α⁴, where α is a primitive element of GF(2⁸)).

For each root `αⁱ`:

```
Sᵢ = r(αⁱ) = c₀ + c₁·αⁱ + c₂·α²ⁱ + … + c₁₄·α¹⁴ⁱ   (all arithmetic in GF(2⁸))
```

**Outputs:**
- `corrupted` – `True` if any syndrome `Sᵢ ≠ 0`.
- `s_vals` – list of syndrome values `[S₁, S₂, S₃, S₄]`.
- `mat` – the Vandermonde-style matrix whose row `i` is `[1, αⁱ, α²ⁱ, …, α¹⁴ⁱ]`.
  This matrix is reused in step 3.

If all syndromes are zero the codeword is valid and decoding stops.

---

## Step 2 – Candidate Position Enumeration (`find_combs.py`)

**Function:** `comb_indices(n, k)`

Generates all C(n, k) index combinations from `{0, 1, …, n−1}` choosing `k`
positions — implemented as a lexicographic iterator without recursion.

The decoder tries error counts `e = 1, 2, …, t` in order. For each count it
generates all C(15, e) candidate position sets. Each set represents a hypothesis
about which symbol positions were corrupted.

**Function:** `zero_out_columns(mat, comb)`

Given a candidate position set `comb` (e.g. `[3, 11]`), all columns of `mat`
*not* in `comb` are zeroed. This restricts the linear system to only the assumed
error positions, turning the (4 × 15) Vandermonde matrix into a (4 × e) system
that has a unique solution when the hypothesis is correct.

---

## Step 3 – Error Magnitude Solve (`solve_matrix.py`)

**Function:** `solve_matrix(A, b, gf)`

Solves `A·e = b` over GF(2⁸) using Gaussian elimination on the augmented matrix
`[A | b]`:

1. **Forward elimination** – for each column find a pivot, normalize its row by
   multiplying by the pivot's multiplicative inverse (`gf.inverse`), then
   eliminate all rows below using `gf.multiply` / `gf.add` (XOR).
2. **Consistency check** – if any row reduces to `[0 … 0 | bᵢ ≠ 0]` the system
   is inconsistent → this candidate position set is wrong, try the next one.
3. **Back substitution** – walks pivot columns in reverse to recover the error
   magnitudes `e[col]`.

The solution vector `x` has length N = 15; non-pivot entries remain 0 (no error
at those positions).

A non-`None`, non-all-zero solution means the current hypothesis is valid: `x[j]`
is the error magnitude at position `j`.

---

## Step 4 – Top-Level Orchestration (`main.py`)

```python
def main():
    gf = GF2m(8, irreducible_poly=0b100011101)
    N, K = 15, 11
    coeffs = [...]            # received 15-symbol codeword
    roots_to_check = [1,2,3,4]

    max_corrupted = (N - K) // 2   # = 2

    corrupted, s_vals, mat = check_roots(coeffs, roots_to_check, gf)

    if corrupted:
        for num_errors in range(1, max_corrupted + 1):
            combs = comb_indices(N, num_errors)
            solution = try_solve(mat, s_vals, gf, combs)
            if solution is not None:
                # solution[j] = error magnitude at position j
                return solution
        # exhausted all hypotheses → uncorrectable
    else:
        return coeffs   # already valid
```

`try_solve` iterates over all position hypotheses for a given error count,
calling `zero_out_columns` + `solve_matrix` for each until a consistent solution
is found.

---

## GF(2⁸) Arithmetic (`GF_pure_python.py`)

All field operations are in `class GF2m`:

| Operation | Method | Implementation |
|-----------|--------|----------------|
| Addition | `add(a, b)` | `a XOR b` |
| Multiplication | `multiply(a, b)` | shift-and-XOR with reduction by irreducible poly |
| Inverse | `inverse(a)` | Fermat's little theorem: `a^(2^m − 2)` |
| Division | `divide(a, b)` | `a * inverse(b)` |
| Power | `power(a, n)` | square-and-multiply with `reduce` after each step |
| Reduce | `reduce(a)` | XOR-shifts until `a < 2^m` |

The field is GF(2⁸) = GF(256) with irreducible polynomial
**x⁸ + x⁴ + x³ + x + 1** (same polynomial used in AES).

---

## File Structure

```
software/software-model/
├── GF_pure_python.py          # GF(2^m) field arithmetic
├── Encoding_full.py           # RS encoder (systematic, polynomial division)
├── GF_op_test.py              # Unit tests for field operations
└── Decoder/
    ├── main.py                # Top-level decoder entry point
    ├── check_roots.py         # Syndrome computation (step 1)
    ├── find_combs.py          # Position enumeration + column masking (step 2)
    ├── solve_matrix.py        # GF Gaussian elimination (step 3)
    └── function_tests.py      # Unit / integration tests for decoder stages
```

---

## Parameters (RS(15, 11) over GF(2⁸))

| Parameter | Value | Meaning |
|-----------|-------|---------|
| N | 15 | Codeword length (symbols) |
| K | 11 | Message length (symbols) |
| N − K | 4 | Number of parity symbols / number of syndromes |
| t | 2 | Maximum correctable errors |
| Field | GF(2⁸) | 256 elements, 8-bit symbols |
| Irreducible poly | `0b100011101` | x⁸ + x⁴ + x³ + x + 1 |
| Roots checked | α¹, α², α³, α⁴ | Generator polynomial roots |
