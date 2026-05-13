import sys
import os

# Add parent directory to path to import GF_pure_python
_parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

from GF_pure_python import GF2m  # noqa: E402


def solve_matrix(A, b, gf=None):
    """
    Solve the matrix equation Ax = b using Gaussian elimination
    in Galois Field GF(2^m). Handles both square and non-square matrices.

    Args:
        A: 2D list representing the coefficient matrix (m x n)
        b: 1D list representing the right-hand side vector (m elements)
        gf: GF2m instance for field arithmetic.
            If None, creates GF(2^8) with default polynomial

    Returns:
        x: List of solutions [x0, x1, ..., x(n-1)]
        None: If the system has no solution

    Raises:
        ValueError: If dimensions don't match

    Description: Find all the pivots from the augmented matrix,
    zero out the columns below the pivots.
    Solve for the pivot variables by back substitution.
    """
    if gf is None:
        # Default to GF(2^8) with same polynomial as encoding
        gf = GF2m(8, irreducible_poly=0b100011101)

    # Validate dimensions
    m = len(A)  # number of rows
    if m == 0:
        print("Matrix A is empty")
        return None
    n = len(A[0])  # number of columns
    if n == 0:
        print("Matrix A has no columns")
        return None
    if len(b) != m:
        print("Vector b must have same length as number of rows")
        return None

    # Create augmented matrix [A|b]
    aug = []
    for i in range(m):
        row = A[i][:] + [b[i]]
        aug.append(row)

    # Forward elimination: process all columns
    num_pivots = 0
    for col in range(n):
        # Find pivot (non-zero element) in current column
        pivot_row = None
        for row in range(num_pivots, m):
            if aug[row][col] != 0:
                pivot_row = row
                break

        # If no pivot found, skip this column
        if pivot_row is None:
            continue

        # Swap rows to bring pivot to current pivot position
        if pivot_row != num_pivots:
            aug[num_pivots], aug[pivot_row] = aug[pivot_row], aug[num_pivots]

        # Normalize pivot row (make pivot = 1)
        pivot_val = aug[num_pivots][col]
        if pivot_val != 1:
            inv_pivot = gf.inverse(pivot_val)
            for j in range(col, n + 1):
                aug[num_pivots][j] = gf.multiply(aug[num_pivots][j], inv_pivot)

        # Eliminate column below pivot
        for row in range(num_pivots + 1, m):
            if aug[row][col] != 0:
                factor = aug[row][col]
                for j in range(col, n + 1):
                    # aug[row][j] = aug[row][j] - factor * aug[num_pivots][j]
                    product = gf.multiply(factor, aug[num_pivots][j])
                    aug[row][j] = gf.add(aug[row][j], product)

        num_pivots += 1

    # Check for inconsistency: if any row has all zeros in A but non-zero in b
    for row in range(num_pivots, m):
        if aug[row][n] != 0:  # b value is non-zero
            # Check if all coefficients in this row are zero
            all_zero = True
            for col in range(n):
                if aug[row][col] != 0:
                    all_zero = False
                    break
            if all_zero:
                # print("No solution")
                return None  # Inconsistent system

    # Back substitution
    x = [0] * n  # Initialize solution vector

    # Pivot column for each pivot row
    pivot_cols = []
    for row in range(num_pivots):
        for col in range(n):
            if aug[row][col] != 0:
                pivot_cols.append(col)
                break

    # Solve for pivot variables
    for i in range(len(pivot_cols) - 1, -1, -1):
        col = pivot_cols[i]
        row = i  # Pivot row index
        # Start with the right-hand side value
        x[col] = aug[row][n]

        # Subtract known values (only from pivot columns to the right)
        for j in range(i + 1, len(pivot_cols)):
            pivot_col = pivot_cols[j]
            product = gf.multiply(aug[row][pivot_col], x[pivot_col])
            x[col] = gf.add(x[col], product)

    return x
