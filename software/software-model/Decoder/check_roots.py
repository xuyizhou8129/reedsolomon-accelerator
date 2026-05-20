import sys
import os

# Add parent directory to path to import GF_pure_python
_parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

from GF_pure_python import GF2m  # noqa: E402

# Input: coeffs: list of coefficients of a polynomial
#        roots_to_check: list of roots to check
#        gf: Galois field to use
# Output: True if all roots are roots of the polynomial, False otherwise
# Convention: coeffs[0] is the highest-degree coefficient (matches SWModel.encode).
# Evaluates P(r) = sum_j coeffs[j] * r^(n-1-j); mat[idx][j] = r^(n-1-j).


def check_roots(coeffs, roots_to_check, gf=None):
    corrupted = False
    s_vals = [0] * len(roots_to_check)
    mat = []
    if gf is None:
        # x^8 + x^4 + x^3 + x + 1
        gf = GF2m(8, irreducible_poly=0b100011101)

    n = len(coeffs)
    for index, root in enumerate(roots_to_check):
        row = []
        sum_val = 0
        for j in range(n):
            power = gf.power(root, n - 1 - j)   # r^(n-1-j)
            row.append(power)
            sum_val = gf.add(sum_val, gf.multiply(coeffs[j], power))
        s_vals[index] = sum_val
        mat.append(row)
        if sum_val != 0:
            corrupted = True
    return corrupted, s_vals, mat
