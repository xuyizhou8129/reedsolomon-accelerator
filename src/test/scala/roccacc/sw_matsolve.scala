package roccacc

// Scala software model of solve_matrix.py.
// Implements Gaussian elimination with back-substitution in GF(2^8).
// Uses GFNumber from SWModel.scala for field arithmetic.
object SWMatSolve {

  def solve(
    A: Seq[Seq[BigInt]],
    b: Seq[BigInt],
    m: Int = 8,
    irreduciblePoly: BigInt = BigInt("100011101", 2)
  ): Option[Seq[BigInt]] = {
    val rows = A.length
    if (rows == 0) return None
    val cols = A(0).length
    if (cols == 0) return None
    require(b.length == rows, s"b length ${b.length} must equal rows $rows")

    def GF(v: BigInt) = new GFNumber(v, m, irreduciblePoly)

    // Build mutable augmented matrix [A | b]
    val aug = Array.tabulate(rows, cols + 1) { (i, j) =>
      if (j < cols) A(i)(j) else b(i)
    }

    // Forward elimination
    var numPivots = 0
    for (col <- 0 until cols) {
      // Find first non-zero element in column at or below numPivots
      val pivotRow = (numPivots until rows).find(r => aug(r)(col) != 0)
      pivotRow match {
        case None => // no pivot in this column, skip
        case Some(pr) =>
          // Swap pivot row to position numPivots
          if (pr != numPivots) {
            val tmp = aug(numPivots).clone()
            aug(numPivots) = aug(pr).clone()
            aug(pr) = tmp
          }
          // Normalize pivot row so pivot element becomes 1
          val pivotVal = aug(numPivots)(col)
          if (pivotVal != 1) {
            val invPivot = GF(pivotVal).inverse().value
            for (j <- col to cols) {
              aug(numPivots)(j) = GF(aug(numPivots)(j)).multiply(GF(invPivot)).value
            }
          }
          // Eliminate all rows below the pivot
          for (row <- numPivots + 1 until rows) {
            if (aug(row)(col) != 0) {
              val factor = aug(row)(col)
              for (j <- col to cols) {
                val product = GF(factor).multiply(GF(aug(numPivots)(j))).value
                aug(row)(j) = aug(row)(j) ^ product  // GF addition is XOR
              }
            }
          }
          numPivots += 1
      }
    }

    // Check for inconsistency: row with all-zero A entries but non-zero b
    for (row <- numPivots until rows) {
      if (aug(row)(cols) != 0) {
        val allZero = (0 until cols).forall(c => aug(row)(c) == 0)
        if (allZero) return None
      }
    }

    // Back substitution
    val x = Array.fill(cols)(BigInt(0))

    // Find pivot column for each pivot row
    val pivotCols = (0 until numPivots).map { row =>
      (0 until cols).find(c => aug(row)(c) != 0).get
    }

    for (i <- pivotCols.length - 1 to 0 by -1) {
      val col = pivotCols(i)
      var acc = aug(i)(cols)
      for (j <- i + 1 until pivotCols.length) {
        val pc = pivotCols(j)
        val product = GF(aug(i)(pc)).multiply(GF(x(pc))).value
        acc = acc ^ product
      }
      x(col) = acc
    }

    Some(x.toSeq)
  }
}
