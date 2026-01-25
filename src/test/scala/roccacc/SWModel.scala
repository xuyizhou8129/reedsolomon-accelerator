package roccacc

class GFNumber(val value: BigInt, val m: Int = 8, val irreduciblePoly: BigInt = 0b100011101) {
    val fieldSize: BigInt = BigInt(2).pow(m)
    val maxElement: BigInt = fieldSize - 1

    // Helper to convert BigInt to GFNumber in the same field
    private def toGF(x: BigInt): GFNumber = {
        new GFNumber(x, m, irreduciblePoly)
    }

    // Addition in GF(2^m) is XOR
    def add(other: GFNumber): GFNumber = {
        require(m == other.m && irreduciblePoly == other.irreduciblePoly, 
                "GF numbers must be from the same field")
        new GFNumber(value ^ other.value, m, irreduciblePoly)
    }

    // Multiplication in GF(2^m)
    def multiply(other: GFNumber): GFNumber = {
        require(m == other.m && irreduciblePoly == other.irreduciblePoly, 
                "GF numbers must be from the same field")
        if (value == 0 || other.value == 0) {
            return new GFNumber(0, m, irreduciblePoly)
        }
        
        var result: BigInt = 0
        var a = value
        var b = other.value
        
        while (b != 0) {
            if ((b & 1) != 0) {
                result ^= a
            }
            a <<= 1
            if (a >= fieldSize) {
                a ^= irreduciblePoly
            }
            b >>= 1
        }
        
        new GFNumber(result, m, irreduciblePoly)
    }

    // Find the multiplicative inverse of a in GF(2^m)
    def inverse(): GFNumber = {
        if (value == 0) {
            throw new IllegalArgumentException("Cannot find inverse of 0")
        }
        if (value == 1) {
            return new GFNumber(1, m, irreduciblePoly)
        }

        // For GF(2^m), we can use the fact that a^(2^m - 2) = a^(-1)
        val power = fieldSize - 2
        var result = toGF(1)
        var current = toGF(value)

        var powerRemaining = power
        while (powerRemaining > 0) {
            if ((powerRemaining & 1) != 0) {
                result = result.multiply(current)
            }
            current = current.multiply(current)
            powerRemaining >>= 1
        }

        result
    }

    // Division in GF(2^m): a / b = a * b^(-1)
    def divide(other: GFNumber): GFNumber = {
        require(m == other.m && irreduciblePoly == other.irreduciblePoly, 
                "GF numbers must be from the same field")
        if (other.value == 0) {
            throw new IllegalArgumentException("Division by zero")
        }
        multiply(other.inverse())
    }

    // Reduce a number until it is within the field GF(2^m)
    def reduce(): GFNumber = {
        var a = value
        while (a > maxElement) {
            // Shift irreducible polynomial to align with highest bit of a
            val shift = a.bitLength - irreduciblePoly.bitLength
            a ^= (irreduciblePoly << shift)
        }
        new GFNumber(a, m, irreduciblePoly)
    }

    // Polynomial division on coefficient arrays
    def polynomialDivideCoeffs(dividendCoeffs: Seq[BigInt], divisorCoeffs: Seq[BigInt]): (Seq[BigInt], Seq[BigInt]) = {
        if (divisorCoeffs.isEmpty || divisorCoeffs.head == 0) {
            throw new IllegalArgumentException("Division by zero")
        }
        if (dividendCoeffs.isEmpty || dividendCoeffs.head == 0) {
            return (Seq(0), Seq(0))
        }

        // Initialize quotient and remainder
        val quotientLength = dividendCoeffs.length - divisorCoeffs.length + 1
        val quotientCoeffs = Array.fill(quotientLength)(BigInt(0))
        val remainderCoeffs = dividendCoeffs.toArray

        // Perform polynomial long division in GF(2^m)
        for (i <- 0 until quotientLength) {
            if (remainderCoeffs(i) != 0) {
                val remainderGF = toGF(remainderCoeffs(i))
                val divisorHeadGF = toGF(divisorCoeffs.head)
                val quotientCoeff = remainderGF.divide(divisorHeadGF).value
                quotientCoeffs(i) = quotientCoeff

                for (j <- divisorCoeffs.indices) {
                    if (i + j < remainderCoeffs.length) {
                        val quotientGF = toGF(quotientCoeff)
                        val divisorGF = toGF(divisorCoeffs(j))
                        val product = quotientGF.multiply(divisorGF).value
                        remainderCoeffs(i + j) ^= product  // Addition in GF is XOR
                    }
                }
            }
        }

        // Extract the actual remainder (last len(divisor)-1 coefficients)
        val actualRemainder = if (divisorCoeffs.length > 1) {
            remainderCoeffs.slice(remainderCoeffs.length - (divisorCoeffs.length - 1), remainderCoeffs.length).toSeq
        } else {
            Seq(BigInt(0))
        }

        (quotientCoeffs.toSeq, actualRemainder)
    }
}


object SWModel {
    // Reed-Solomon encoding parameters
    val DEFAULT_N = 15  // codeword length
    val DEFAULT_K = 11  // message length
    val DEFAULT_FIELD_SIZE = 8
    val DEFAULT_IRREDUCIBLE_POLY = BigInt("100011101", 2)  // 0b100011101
    // Default generator polynomial coefficients for RS(15,11)
    val DEFAULT_GENERATOR_COEFFS = Seq(
        BigInt(1),      // 0b00000001
        BigInt(4),      // 0b00000100
        BigInt(7),      // 0b00000111
        BigInt(26),     // 0b00011010
        BigInt(24)      // 0b00011000
    )

    def encode(
        msg: Seq[BigInt],
        n: Int = DEFAULT_N,
        k: Int = DEFAULT_K,
        fieldSize: Int = DEFAULT_FIELD_SIZE,
        irreduciblePoly: BigInt = DEFAULT_IRREDUCIBLE_POLY,
        generatorCoeffs: Seq[BigInt] = DEFAULT_GENERATOR_COEFFS
    ): Seq[BigInt] = {
        require(msg.length == k, s"Message length must be $k, got ${msg.length}")
        
        // Create a GFNumber instance for field operations
        val gf = new GFNumber(0, fieldSize, irreduciblePoly)
        
        // Multiply by x^(n-k) - append zeros
        val zeros = Seq.fill(n - k)(BigInt(0))
        val shiftedMessageCoeffs = msg ++ zeros
        
        // Perform polynomial division: (M(x) * x^(n-k)) / G(x)
        val (quotientCoeffs, remainderCoeffs) = gf.polynomialDivideCoeffs(
            shiftedMessageCoeffs, generatorCoeffs
        )
        
        // Add the remainder to the shifted message (polynomial addition in GF)
        val codewordCoeffs = shiftedMessageCoeffs.toArray
        for (i <- remainderCoeffs.indices) {
            val pos = codewordCoeffs.length - remainderCoeffs.length + i
            // Addition in GF(2^m) is XOR
            codewordCoeffs(pos) ^= remainderCoeffs(i)
        }
        
        codewordCoeffs.toSeq
    }
}