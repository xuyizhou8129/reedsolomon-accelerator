package roccacc

// Scala software model of check_roots.py.
// Evaluates a polynomial at each root and builds the Vandermonde matrix.
// Uses GFNumber from SWModel.scala for field arithmetic.
object SWCheckRoots {

  case class Result(
    corrupted: Boolean,
    sVals:     Seq[BigInt],
    mat:       Seq[Seq[BigInt]]
  )

  // Convention: coeffs(0) is the highest-degree coefficient (matches SWModel.encode).
  // Evaluates P(r) = sum_j coeffs(j) * r^(n-1-j); mat(idx)(j) = r^(n-1-j).
  def checkRoots(
    coeffs:          Seq[BigInt],
    rootsToCheck:    Seq[BigInt]  = Seq(1, 2, 3, 4).map(BigInt(_)),
    m:               Int          = 8,
    irreduciblePoly: BigInt       = BigInt("100011101", 2)
  ): Result = {
    def GF(v: BigInt) = new GFNumber(v, m, irreduciblePoly)

    val n     = coeffs.length
    var corrupted = false
    val sVals = Array.fill(rootsToCheck.length)(BigInt(0))
    val mat   = Array.tabulate(rootsToCheck.length, n)((_, _) => BigInt(0))

    for (idx <- rootsToCheck.indices) {
      val root = rootsToCheck(idx)
      var sum   = BigInt(0)
      var power = BigInt(1)   // r^0 for j = n-1; multiplied by r each step
      for (j <- (n - 1) to 0 by -1) {
        mat(idx)(j)  = power
        val product  = GF(power).multiply(GF(coeffs(j))).value
        sum          = sum ^ product
        power        = GF(power).multiply(GF(root)).value
      }
      sVals(idx) = sum
      if (sum != 0) corrupted = true
    }

    Result(corrupted, sVals.toSeq, mat.map(_.toSeq).toSeq)
  }
}
