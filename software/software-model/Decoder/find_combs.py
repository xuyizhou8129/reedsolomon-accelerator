# To find all combinations of corrupted positions
# Takes in the numer of corrupted positions
# Returns a list of matrices of corrupted positions
# There are at least (N-(N-K)/2) elements in e0, e1, e2, ..., e(n-1)
# that are zero
# N = 15
# K = 11
# max_corrupted = (N-K)/2


def comb_indices(n, k):
    if k < 0 or k > n:
        return []
    # Initialize list to store all combinations
    result = []
    # first combination: [0,1,2,...,k-1]
    c = list(range(k))
    while True:
        result.append(c[:])  # append a copy to the result list

        # find rightmost element that can be incremented
        i = k - 1
        while i >= 0 and c[i] == n - k + i:
            i -= 1
        if i < 0:
            break  # done

        # increment it and reset the tail
        c[i] += 1
        for j in range(i + 1, k):
            c[j] = c[j - 1] + 1
    return result


def zero_out_columns(mat, comb):
    cols_to_zero = [col for col in range(len(mat[0])) if col not in comb]
    for col in cols_to_zero:
        for row in range(len(mat)):
            mat[row][col] = 0
    return mat
