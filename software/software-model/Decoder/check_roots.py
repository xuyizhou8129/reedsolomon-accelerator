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


def check_roots(coeffs, roots_to_check, gf=None):
    # a + bx + cx^2 + ... + nx^(n-1)
    # Default to GF(2^8) if not provided
    corrupted = False
    s_vals = [0] * len(roots_to_check)
    mat = []
    if gf is None:
        # x^8 + x^4 + x^3 + x + 1
        gf = GF2m(8, irreducible_poly=0b100011101)

    # The first term is the constant term
    for index, root in enumerate(roots_to_check):
        value = 0
        row = [1]
        sum = coeffs[0]
        for i in range(1, len(coeffs)):
            value = gf.power(root, i)
            row.append(value)
            value = gf.multiply(value, coeffs[i])
            sum = gf.add(sum, value)
        s_vals[index] = sum
        mat.append(row)
        if sum != 0:
            corrupted = True
    return corrupted, s_vals, mat
