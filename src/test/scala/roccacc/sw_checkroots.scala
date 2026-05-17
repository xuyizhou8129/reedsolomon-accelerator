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

  def checkRoots(
    coeffs:          Seq[BigInt],
    rootsToCheck:    Seq[BigInt]  = Seq(1, 2, 3, 4).map(BigInt(_)),
    m:               Int          = 8,
    irreduciblePoly: BigInt       = BigInt("100011101", 2)
  ): Result = {
    def GF(v: BigInt) = new GFNumber(v, m, irreduciblePoly)

    var corrupted = false
    val sVals = Array.fill(rootsToCheck.length)(BigInt(0))
    val mat   = Array.tabulate(rootsToCheck.length, coeffs.length)((_, _) => BigInt(0))

    for (idx <- rootsToCheck.indices) {
      val root = rootsToCheck(idx)
      var sum  = coeffs(0)
      mat(idx)(0) = BigInt(1)
      var power = BigInt(1)
      for (i <- 1 until coeffs.length) {
        power        = GF(power).multiply(GF(root)).value  // r^i incrementally
        mat(idx)(i)  = power
        val product  = GF(power).multiply(GF(coeffs(i))).value
        sum          = sum ^ product  // GF addition is XOR
      }
      sVals(idx) = sum
      if (sum != 0) corrupted = true
    }

    Result(corrupted, sVals.toSeq, mat.map(_.toSeq).toSeq)
  }
}
