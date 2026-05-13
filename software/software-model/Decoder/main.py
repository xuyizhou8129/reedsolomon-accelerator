import sys
import os

# Add parent directory to path so Decoder and GF_pure_python can be imported
_parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

from Decoder.check_roots import check_roots  # noqa: E402
from Decoder.find_combs import comb_indices, zero_out_columns  # noqa: E402
from Decoder.solve_matrix import solve_matrix  # noqa: E402
from GF_pure_python import GF2m  # noqa: E402


def try_solve(mat, s_vals, gf, combs):
    solution = None
    for comb in combs:
        mat_trial = [row[:] for row in mat]
        zero_out_columns(mat_trial, comb)
        solution = solve_matrix(mat_trial, s_vals, gf)
        if solution is not None and any(solution):
            return solution
    return None


def main():
    gf = GF2m(8, irreducible_poly=0b100011101)
    N = 15
    K = 11
    coeffs = [80, 91, 235, 237, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2]
    roots_to_check = [1, 2, 3, 4]

    # Check if the received word is corrupted
    max_corrupted = (N - K) // 2  # max correctable errors
    corrupted, s_vals, mat = check_roots(coeffs, roots_to_check, gf)
    if corrupted:
        # Try 1 error, then 2 errors. comb = assumed error positions
        for num_errors in range(1, max_corrupted + 1):
            combs = comb_indices(N, num_errors)
            solution = try_solve(mat, s_vals, gf, combs)
            if solution is not None:
                print(solution)
                return
        print("No solution found")
    else:
        print("No corrupted roots found")
        return coeffs


if __name__ == "__main__":
    main()
