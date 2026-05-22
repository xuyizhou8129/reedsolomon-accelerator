package roccacc

/** Precomputes the Mastrovito reduction matrix for GF(2^m) at Chisel elaboration time.
  *
  * For a 2m-bit input and a fixed irreducible polynomial p(x), builds a table M where
  *   output_bit(j) = XOR { input_bit(i) | M(i)(j) == true }
  *
  * Low bits i < m pass through via identity; high bits i >= m map to x^i mod p(x),
  * computed by successive multiplication-by-x starting from x^m mod p(x).
  *
  * Tests can import this object directly to cross-check the matrix against a
  * reference software reduction loop without instantiating any Chisel module.
  */
object GFReduceGen {

  /** Irreducible polynomial for GF(2^8): x^8 + x^4 + x^3 + x^2 + 1 = 0x11D. */
  val GF256_POLY: Int = 0x11D

  /**
    * Build the reduction matrix.
    *
    * @param fieldSize      m, so the field is GF(2^m)
    * @param irreduciblePoly full polynomial including the leading x^m term (e.g. 0x11D)
    * @return Array of length fieldSize where entry j is the sorted list of input-bit
    *         indices (0 to 2m-1) that XOR together to produce output bit j.
    */
  def buildReductionMatrix(fieldSize: Int, irreduciblePoly: Int): Array[Seq[Int]] = {
    val width = 2 * fieldSize

    // contrib(i)(j): input bit i contributes to output bit j
    val contrib = Array.ofDim[Boolean](width, fieldSize)

    // Lower m input bits pass straight through (identity)
    for (i <- 0 until fieldSize) {
      contrib(i)(i) = true
    }

    // Strip the leading x^m term to get the low-m-bit reduction polynomial
    val reducePoly = irreduciblePoly ^ (1 << fieldSize)

    // Compute x^i mod p(x) for i in [m, 2m-1] by repeated multiplication by x:
    //   x^m mod p = reducePoly
    //   x^(i+1) mod p = (x^i mod p) shifted left, XOR reducePoly if the MSB was 1
    var cur = reducePoly
    for (i <- fieldSize until width) {
      //record the contribution of each input bit to each output bit
      for (j <- 0 until fieldSize) {
        contrib(i)(j) = ((cur >> j) & 1) == 1
      }
      //update the current value of x^i mod p(x)
      if (i < width - 1) {
        val msb = (cur >> (fieldSize - 1)) & 1
        cur = (cur << 1) & ((1 << fieldSize) - 1)
        if (msb == 1) cur ^= reducePoly
      }
    }

    // Transpose: for each output bit j, collect the input-bit indices that feed it
    Array.tabulate(fieldSize) { j =>
      (0 until width).filter(i => contrib(i)(j)).toSeq
    }
  }
}
