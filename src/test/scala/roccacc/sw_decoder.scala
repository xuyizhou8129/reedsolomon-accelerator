package roccacc

object SWDecoder {

  case class DecodeResult(
    corrupted: Boolean,
    solved:    Boolean,
    errorVec:  Seq[BigInt],
    corrected: Seq[BigInt]
  )

  // Generate all k-element subsets of {0..n-1} in lexicographic order.
  // Mirrors comb_indices(n, k) from find_combs.py.
  def combIndices(n: Int, k: Int): Seq[Seq[Int]] = {
    if (k < 0 || k > n) return Seq.empty
    val result = scala.collection.mutable.ArrayBuffer.empty[Seq[Int]]
    val c = Array.range(0, k)
    var running = true
    while (running) {
      result += c.toSeq
      var i = k - 1
      while (i >= 0 && c(i) == n - k + i) i -= 1
      if (i < 0) {
        running = false
      } else {
        c(i) += 1
        for (j <- i + 1 until k) c(j) = c(j - 1) + 1
      }
    }
    result.toSeq
  }

  // Decode a received RS codeword.  Mirrors the full main.py pipeline:
  //   1. check_roots → get corrupted flag, Vandermonde matrix M, syndrome vector s
  //   2. If corrupted: try error counts 1..t, enumerating all combinations via combIndices
  //   3. For each combo: zero non-active columns of M, run solve_matrix
  //   4. Return first non-trivial solution found, or failure if none found
  def decode(
    received:        Seq[BigInt],
    n:               Int         = 15,
    k:               Int         = 11,
    roots:           Seq[BigInt] = Seq(1, 2, 3, 4).map(BigInt(_)),
    m:               Int         = 8,
    irreduciblePoly: BigInt      = BigInt("100011101", 2)
  ): DecodeResult = {
    val swCR = SWCheckRoots.checkRoots(received, roots, m, irreduciblePoly)

    if (!swCR.corrupted) {
      return DecodeResult(
        corrupted = false,
        solved    = false,
        errorVec  = Seq.fill(n)(BigInt(0)),
        corrected = received
      )
    }

    val t     = (n - k) / 2
    val mat   = swCR.mat
    val sVals = swCR.sVals

    var result: Option[DecodeResult] = None
    var numErrors = 1
    while (numErrors <= t && result.isEmpty) {
      val combs = combIndices(n, numErrors)
      var ci = 0
      while (ci < combs.length && result.isEmpty) {
        val comb = combs(ci)
        // Zero all columns not in this combination (mirrors zero_out_columns)
        val A = mat.map { row =>
          row.zipWithIndex.map { case (v, j) =>
            if (comb.contains(j)) v else BigInt(0)
          }
        }
        SWMatSolve.solve(A, sVals, m, irreduciblePoly) match {
          case Some(x) if x.exists(_ != 0) =>
            val corrected = received.zip(x).map { case (r, e) => r ^ e }
            result = Some(DecodeResult(
              corrupted = true,
              solved    = true,
              errorVec  = x,
              corrected = corrected
            ))
          case _ =>
        }
        ci += 1
      }
      numErrors += 1
    }

    result.getOrElse(DecodeResult(
      corrupted = true,
      solved    = false,
      errorVec  = Seq.fill(n)(BigInt(0)),
      corrected = received
    ))
  }
}
