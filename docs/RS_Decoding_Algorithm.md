# Reed-Solomon Decoding

## The Big Picture

The decoder receives **n** symbols, which may have been corrupted during transmission. Its job is to figure out what errors occurred and recover the original message.

The decoder treats the received symbols as coefficients of a polynomial `S'(x)`, then checks whether they satisfy the mathematical properties a valid codeword must have.

---

## Step 1: Check for Errors

Recall from encoding that the codeword polynomial `S(x)` was constructed so that the roots of the generator `g(x)` — call them `x₀, x₁, ..., x_{n-k-1}` — are also roots of `S(x)`. That means:

```
S(x₀) = 0,  S(x₁) = 0,  ...,  S(x_{n-k-1}) = 0
```

**If no corruption occurred**, the received polynomial `S'(x) = S(x)`, and plugging in each `xᵢ` gives zero. 

**If there is corruption**, `S'(x) ≠ S(x)`, and at least one of those checks will be non-zero. 

---

## Step 2: Model the Error

Define the **error polynomial** `e(x)` as the difference between what was received and what was sent:

```
S'(x) = S(x) + e(x)
```

Since `S(x)` always evaluates to 0 at the roots `x₀, ..., x_{n-k-1}`, plugging those roots into `S'(x)` gives us the error values directly:

```
e₀ + e₁x₀ + e₂x₀² + ... + e_{n-1}x₀^{n-1} = S₀
e₀ + e₁x₁ + e₂x₁² + ... + e_{n-1}x₁^{n-1} = S₁
⋮
e₀ + e₁x_{n-k-1} + ... + e_{n-1}x_{n-k-1}^{n-1} = S_{n-k-1}
```

These values `S₀, S₁, ...` are called the **syndromes** — they are non-zero only where errors exist.

---

## Step 3: Solve for the Error Polynomial

The goal is to find all the coefficients of `e(x)` (i.e., figure out where and how much corruption happened).

**The catch:** There are more unknowns (n possible error positions) than equations (only n−k syndromes). Normally this is unsolvable.

**The exception:** If the number of corrupted symbols is **at most half of (n−k)**, the system becomes solvable. This is the fundamental error-correction capacity of Reed-Solomon:

```
Maximum correctable errors = floor((n − k) / 2)
```

> Think of it like this: each error introduces 2 unknowns (where it is, and how big it is), and each redundancy symbol gives you 1 equation. So you need at least twice as many redundancy symbols as errors.

---

## Step 4: Recover the Original Message

Once you've solved for `e(x)`, subtract it from what you received:

```
S(x) = S'(x) − e(x)
```

Now you have the clean codeword `S(x)`. Extract the high-degree coefficients (the message part) to recover:

```
m₀, m₁, ..., m_{k-1}
```

---

## Summary

| Step | What Happens |
|---|---|
| **Receive** | Get n symbols, treat as `S'(x)` |
| **Check** | Plug known roots into `S'(x)` — non-zero means errors |
| **Model** | Write `S'(x) = S(x) + e(x)`; compute syndromes |
| **Solve** | Find coefficients of `e(x)` (possible if errors ≤ (n−k)/2) |
| **Recover** | Compute `S(x) = S'(x) − e(x)`, read off message |

---

## Key Limits to Remember

- RS can **detect** up to n−k errors.
- RS can **correct** up to **(n−k)/2** errors.
- Beyond that limit, there are more unknowns than equations, and unique recovery is impossible.
