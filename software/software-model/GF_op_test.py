
from GF_pure_python import GF2m
gf = GF2m(8, irreducible_poly=0b100011101)

# print(f"Working in {gf}")
# print(f"Field size: {gf.field_size}")
# print(f"Elements: 0 to {gf.max_element}")

# # Test your values
# p_raw = 0b1100011101
# g_raw = 0b1001011101
# p = gf.reduce(p_raw)
# g = gf.reduce(g_raw)

# dividend = 0b1101  # x^3 + x^2 + 1
# divisor = 0b101    # x^2 + 1
# quotient, remainder = gf.polynomial_divide(dividend, divisor)
# print(f"Dividend: {dividend} (0b{dividend:04b}) = x^3 + x^2 + 1")
# print(f"Divisor: {divisor} (0b{divisor:03b}) = x^2 + 1")
# print(f"Quotient: {quotient} (0b{quotient:04b})")
# print(f"Remainder: {remainder} (0b{remainder:04b})")

# product = gf.reduce(gf.multiply(p, g))
# quotient = gf.reduce(gf.divide(p, g))

# print(f"p: {p} (0b{p:04b})")
# print(f"g: {g} (0b{g:04b})")
# print(f"Product: {product} (0b{product:04b})")
# print(f"Quotient: {quotient} (0b{quotient:04b})")
print(gf.add(3, 11))

# sum = gf.add(p, g)
# print(f"Sum: {sum} (0b{sum:04b})")
