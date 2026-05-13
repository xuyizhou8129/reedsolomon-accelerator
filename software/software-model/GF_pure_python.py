"""
Pure Python implementation of Galois Field arithmetic for GF(2^m)
No external dependencies required
"""


class GF2m:
    def __init__(self, m, irreducible_poly=None):
        """
        Initialize Galois Field GF(2^m)
        m: field size (elements are 0 to 2^m-1)
        irreducible_poly: polynomial that defines the field
        """
        self.m = m
        self.field_size = 2**m
        self.max_element = self.field_size - 1
        # Default irreducible polynomial: x^m + 1
        if irreducible_poly is None:
            # self.irreducible = (1 << m) | 1  # x^m + 1
            self.irreducible = 0b10001  # x^4 + 1
        else:
            self.irreducible = irreducible_poly

    def add(self, a, b):
        """Addition in GF(2^m) is XOR"""
        return a ^ b

    def multiply(self, a, b):
        """Multiplication in GF(2^m)"""
        if a == 0 or b == 0:
            return 0
        result = 0
        while b:
            if b & 1:
                result ^= a
                # acumulates the result by adding a
            a <<= 1
            #   shifts a by 1 bit no matter the digit of b is zero or not
            if a >= self.field_size:
                # if a is greater than the field size,
                # subtract the irreducible polynomial
                a ^= self.irreducible
            b >>= 1
        return result

    def inverse(self, a):
        """Find the multiplicative inverse of a in GF(2^m)"""
        if a == 0:
            raise ValueError("Cannot find inverse of 0")
        if a == 1:
            return 1

        # For GF(2^m), we can use the fact that a^(2^m - 2) = a^(-1)
        power = self.field_size - 2
        result = 1
        current = a

        while power > 0:
            if power & 1:
                result = self.multiply(result, current)
            current = self.multiply(current, current)
            power >>= 1

        return result

    def divide(self, a, b):
        """Division in GF(2^m): a / b = a * b^(-1)"""
        if b == 0:
            raise ValueError("Division by zero")
        return self.multiply(a, self.inverse(b))

    def power(self, a, n):
        """Compute a^n in GF(2^m)"""
        if n < 0:
            raise ValueError("Negative exponent not supported")
        if n == 0:
            return 1
        if a == 0:
            return 0
        if a == 1:
            return 1

        result = 1
        current = a
        while n > 0:
            if n & 1:
                result = self.multiply(result, current)
                result = self.reduce(result)
            current = self.multiply(current, current)
            current = self.reduce(current)
            n >>= 1
        return result

    def reduce(self, a):
        """
        Reduce a number until it is within the field GF(2^m)
        Returns a value in the range [0, 2^m - 1]
        """
        # Keep reducing until within field range
        while a > self.max_element:
            # Shift irreducible polynomial to align with highest bit of a
            shift = a.bit_length() - self.irreducible.bit_length()
            a ^= self.irreducible << shift

        return a

    def polynomial_divide_coeffs(self, dividend_coeffs, divisor_coeffs):
        """
        Polynomial division on coefficient arrays
        Each coefficient is a 4-bit value (0-15) in GF(2^4)
        """
        if not divisor_coeffs or divisor_coeffs[0] == 0:
            raise ValueError("Division by zero")
        if not dividend_coeffs or dividend_coeffs[0] == 0:
            return [0], [0]

        # Initialize quotient and remainder
        quotient_coeffs = [0] * \
            (len(dividend_coeffs) - len(divisor_coeffs) + 1)
        remainder_coeffs = dividend_coeffs.copy()

        # Perform polynomial long division in GF(2^4)
        for i in range(len(quotient_coeffs)):
            if remainder_coeffs[i] != 0:
                quotient_coeff = self.divide(
                    remainder_coeffs[i], divisor_coeffs[0])
                quotient_coeffs[i] = quotient_coeff

                for j in range(len(divisor_coeffs)):
                    if i + j < len(remainder_coeffs):
                        product = self.multiply(
                            quotient_coeff, divisor_coeffs[j])
                        remainder_coeffs[i + j] = self.add(
                            remainder_coeffs[i + j], product
                        )

        # Extract the actual remainder (last len(divisor)-1 coefficients)
        actual_remainder = (
            remainder_coeffs[-(len(divisor_coeffs) - 1):]
            if len(divisor_coeffs) > 1
            else [0]
        )

        return quotient_coeffs, actual_remainder

    def polynomial_divide(self, dividend, divisor):
        """
        Simple polynomial division with remainder (legacy method)
        Returns (quotient, remainder)
        """
        if divisor == 0:
            raise ValueError("Division by zero")
        if dividend == 0:
            return 0, 0

        quotient = 0
        remainder = dividend

        # Find highest bit positions
        while remainder.bit_length() >= divisor.bit_length():
            # Calculate shift amount
            shift = remainder.bit_length() - divisor.bit_length()

            # Add this term to quotient
            quotient ^= 1 << shift

            # Subtract (XOR) shifted divisor from remainder
            remainder ^= divisor << shift

        return quotient, remainder

    def __str__(self):
        return f"GF(2^{self.m})"
