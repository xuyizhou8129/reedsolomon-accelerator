from GF_pure_python import GF2m

# Reed-Solomon encoding using coefficient arrays
# GF(2^8) with irreducible polynomial x^8 + x^4 + x^3 + x + 1 (0x11B)
gf = GF2m(8, irreducible_poly=0b100011101)  # x^8 + x^4 + x^3 + x + 1
fieldSize = 8
k = 11
n = 15
print(f"Working in {gf}")
print(f"Field size: {gf.field_size}")
print(f"Elements: 0 to {gf.max_element}")
print(f"k: {k}")
print(f"n: {n}")

# Test message with k=11 coefficients (matching the Scala test)
message_coeffs_binary = [
    0b00000001,  # coefficient 0: 1
    0b00000010,  # coefficient 1: 2
    0b00000011,  # coefficient 2: 3
    0b00000100,  # coefficient 3: 4
    0b00000101,  # coefficient 4: 5
    0b00000110,  # coefficient 5: 6
    0b00000111,  # coefficient 6: 7
    0b00001000,  # coefficient 7: 8
    0b00001001,  # coefficient 8: 9
    0b00001010,  # coefficient 9: 10
    0b00001011,  # coefficient 10: 11
]
print(
    f"Message coefficients (binary): "
    f"{[f'{c:08b}' for c in message_coeffs_binary]}"
)

# Multiply by x^(n-k)
zeroes = []
for i in range(1, n - k + 1):
    zeroes.append(0b00000000)
shifted_message_coeffs = message_coeffs_binary + zeroes
print(
    f"M(x) * x^(n-k) coefficients (binary):"
    f"{[f'{c:08b}' for c in shifted_message_coeffs]}"
)

# Generator polynomial: G(x) for GF(2^8) with n=15, k=11 (degree 4)
# This is a placeholder - you may need to adjust based on your specific RS code
# For a systematic RS code, G(x) = (x - α^1)(x - α^2)...(x - α^(n-k))
generator_coeffs_binary = [0b00000001, 0b11110110,
                           0b00100011, 0b11001110, 0b00011000]
print(
    f"Generator polynomial coefficients (binary):"
    f" {[f'{c:08b}' for c in generator_coeffs_binary]}"
)

# Perform polynomial division: (M(x) * x^(n-k)) / G(x)
quotient_coeffs, remainder_coeffs = gf.polynomial_divide_coeffs(
    shifted_message_coeffs, generator_coeffs_binary
)
print(f"Quotient coefficients (binary): "
      f"{[f'{c:08b}' for c in quotient_coeffs]}")
print(f"Remainder coefficients (binary):"
      f" {[f'{c:08b}' for c in remainder_coeffs]}")

# Add the remainder to the shifted message (polynomial addition in GF)
codeword_coeffs = shifted_message_coeffs.copy()
# Add remainder coefficients to the last positions
for i, rem_coeff in enumerate(remainder_coeffs):
    if i < len(remainder_coeffs):
        # Add remainder coefficient to the corresponding position
        pos = len(codeword_coeffs) - len(remainder_coeffs) + i
        codeword_coeffs[pos] = gf.add(codeword_coeffs[pos], rem_coeff)

print(f"\nCodeword coefficients (binary): "
      f"{[f'{c:08b}' for c in codeword_coeffs]}")
print(f"Codeword coefficients (decimal): {codeword_coeffs}")
