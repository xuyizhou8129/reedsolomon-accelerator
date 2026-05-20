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


def encode(msg, gen, gf):
    """Systematic RS encode: returns msg ++ parity (high-degree-first)."""
    n_k = len(gen) - 1
    shifted = msg + [0] * n_k
    _, remainder = gf.polynomial_divide_coeffs(shifted, gen)
    codeword = shifted[:]
    for i, r in enumerate(remainder):
        codeword[len(codeword) - len(remainder) + i] ^= r
    return codeword


def main():
    gf = GF2m(8, irreducible_poly=0b100011101)
    N = 15
    K = 11
    # Generator polynomial for RS(15,11): roots alpha^1..alpha^4 (high-degree-first)
    gen = [1, 4, 7, 26, 24]
    roots_to_check = [1, 2, 3, 4]
    max_corrupted = (N - K) // 2  # 2

    # Test 1: valid codeword — expect no corruption
    msg = list(range(2, K + 2))
    codeword = encode(msg, gen, gf)
    print(f"Valid codeword: {codeword}")
    corrupted, _, _ = check_roots(codeword, roots_to_check, gf)
    print(f"Corrupted: {corrupted} (expected False)\n")

    # Test 2: single-symbol error at position 3 — expect correction
    received = codeword[:]
    received[3] ^= 7
    print(f"Received (1 error at pos 3): {received}")
    corrupted, s_vals, mat = check_roots(received, roots_to_check, gf)
    if corrupted:
        for num_errors in range(1, max_corrupted + 1):
            combs = comb_indices(N, num_errors)
            solution = try_solve(mat, s_vals, gf, combs)
            if solution is not None:
                print(f"Error vector: {solution}")
                corrected = [r ^ e for r, e in zip(received, solution)]
                print(f"Corrected:    {corrected}")
                print(f"Match original: {corrected == codeword}")
                return
        print("No solution found")
    else:
        print("No corruption detected")


if __name__ == "__main__":
    main()
