import sys
import os

# Add parent directory to path to import modules
_parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

from GF_pure_python import GF2m  # noqa: E402
from Decoder.check_roots import check_roots  # noqa: E402
from Decoder.find_combs import comb_indices  # noqa: E402
from Decoder.solve_matrix import solve_matrix  # noqa: E402


def test_check_roots():
    gf = GF2m(8, irreducible_poly=0b100011101)

    print("\nTest 1: Polynomial p(x) = x + 1")
    coeffs1 = [1, 1]  # 1 + x
    roots_to_check1 = [1, 2, 3]
    result1, s_vals1, mat1 = check_roots(coeffs1, roots_to_check1, gf)
    print(f"Result: {result1}")
    print("Expected: True")
    print(f"S values: {s_vals1}")
    print(f"Matrix: {mat1}")

    print("\nTest 2: Polynomial p(x) = x^2 + x")
    coeffs2 = [0, 1, 1]  # 0 + x + x^2
    roots_to_check2 = [0, 1, 2]
    result2, s_vals2, mat2 = check_roots(coeffs2, roots_to_check2, gf)
    print(f"Result: {result2}")
    print("Expected: True")
    print(f"S values: {s_vals2}")
    print(f"Matrix: {mat2}")

    print("\nTest 3: Polynomial")
    coeffs3 = [80, 91, 235, 237, 12, 11, 10, 9, 9, 7, 7, 5, 4, 3, 2]
    roots_to_check3 = [1, 2, 3, 4]
    result3, s_vals3, mat3 = check_roots(coeffs3, roots_to_check3, gf)
    print(f"Result: {result3}")
    print("Expected: False")
    print(f"S values: {s_vals3}")
    print(f"Matrix: {mat3}")

    print("\nTest 4: Polynomial")
    # coeffs4 = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 237, 235, 91, 80]
    coeffs4 = [80, 91, 235, 237, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2]
    roots_to_check4 = [1, 2, 3, 4]
    result4, s_vals4, mat4 = check_roots(coeffs4, roots_to_check4, gf)
    print(f"Result: {result4}")
    print("Expected: False")
    print(f"S values: {s_vals4}")
    print(f"Matrix: {mat4}")

    print("\nTest 5: Polynomial")
    # coeffs4 = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 237, 235, 91, 80]
    coeffs5 = [80, 91, 235, 237, 12, 11, 10, 9, 9, 7, 6, 5, 4, 3, 2]
    roots_to_check5 = [1, 2, 3, 4]
    result5, s_vals5, mat5 = check_roots(coeffs5, roots_to_check5, gf)
    print(f"Result: {result5}")
    print("Expected: True")
    print(f"S values: {s_vals5}")
    print(f"Matrix: {mat5}")


def test_comb_indices():
    N = 15
    K = 11
    combs = comb_indices(N, K)
    return combs


def test_matrix_solve():
    A1 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    b1 = [1, 2, 3]
    x1 = solve_matrix(A1, b1)
    print(x1)

    A2 = [[1, 0, 0, 1], [2, 1, 0, 1], [3, 2, 1, 1]]
    b2 = [1, 2, 4]
    x2 = solve_matrix(A2, b2)
    print(x2)


if __name__ == "__main__":
    test_check_roots()
    # print("--------------------------------")
    # test_comb_indices()
    # print("--------------------------------")
    # test_solve_matrix()
    # print("--------------------------------")
    # test_matrix_solve()
