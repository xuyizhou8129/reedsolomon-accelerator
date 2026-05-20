package roccacc

import chisel3._
import chisel3.util._

object CheckRoots {
  // Compile-time roots for RS(15,11): consecutive powers of primitive element
  // matching generator (x-1)(x-2)(x-3)(x-4) used in check_roots.py / main.py.
  val DEFAULT_ROOTS      = Seq(1, 2, 3, 4)
  val DEFAULT_NUM_COEFFS = 15
}

// Evaluates a polynomial at each compile-time root, writes the Vandermonde
// matrix M and syndrome vector S to external memory one word at a time, and
// asserts corrupted if any syndrome is non-zero.
//
// Memory address map (write-only port; caller owns the backing store):
//
//   Matrix M (written):    baseM + i*numCoeffs + j   i=0..numRoots-1  row-major
//   Syndromes S (written): baseS + i                 i=0..numRoots-1
//
// io.coeffs must be held stable from the start pulse until done fires.
// io.corrupted is valid and stable in the same cycle io.done pulses.
// io.busy is asserted from start until (and including) done.
class CheckRoots(
  numCoeffs: Int      = CheckRoots.DEFAULT_NUM_COEFFS,
  roots:     Seq[Int] = CheckRoots.DEFAULT_ROOTS,
  addrWidth: Int      = 32,
  fieldSize: Int      = GFOperations.DEFAULT_FIELD_SIZE
) extends Module {

  val numRoots = roots.length
  val ww       = 2 * fieldSize

  val io = IO(new Bundle {
    val start     = Input(Bool())
    val busy      = Output(Bool())
    val done      = Output(Bool())
    val corrupted = Output(Bool())

    // Polynomial coefficients: index 0 is the highest-degree coefficient (matches SWModel.encode).
    // Must be held stable from start until done.
    val coeffs = Input(Vec(numCoeffs, UInt(fieldSize.W)))

    // Base addresses for write-only memory port
    val baseM = Input(UInt(addrWidth.W))
    val baseS = Input(UInt(addrWidth.W))

    // Write-only single-word memory port (same handshake as MatSolve)
    val memAddr  = Output(UInt(addrWidth.W))
    val memWData = Output(UInt(fieldSize.W))
    val memWrite = Output(Bool())
    val memReady = Input(Bool())
  })

  object S extends ChiselEnum {
    val sIdle,
        sInitRoot,     // 1-cycle: set curPow=1, sval=0, j=numCoeffs-1
        sWrMj,         // write curPow (= r^(numCoeffs-1-j)) to M[i][j]
        sMulS, sMulW,  // GFMul2: product = curPow * coeffs[j]
        sAddS, sAddW,  // GFAdd:  sval ^= product; stop if j==0
        sPowS, sPowW,  // GFMul1: curPow = curPow * root[i]; j--
        sWrS,          // write S[i] = sval
        sNextI,        // update corrupted flag; advance root index
        sDone = Value
  }
  import S._

  val state = RegInit(sIdle)

  // Small scalar FSM state — no matrix-sized registers
  val iReg       = RegInit(0.U(log2Ceil(numRoots + 1).W))
  val jReg       = RegInit(0.U(log2Ceil(numCoeffs + 1).W))
  val curPow     = Reg(UInt(fieldSize.W))
  val svalReg    = Reg(UInt(fieldSize.W))
  val product    = Reg(UInt(fieldSize.W))
  val corruptReg = RegInit(false.B)
  val baseMReg   = Reg(UInt(addrWidth.W))
  val baseSReg   = Reg(UInt(addrWidth.W))

  // Roots encoded as 2*fieldSize-wide constants for GFMul inputs
  val rootsVec = VecInit(roots.map(r => r.U(ww.W)))

  val gfMul1 = Module(new GFMul(fieldSize))
  val gfMul2 = Module(new GFMul(fieldSize))
  val gfAdd1 = Module(new GFAdd(fieldSize))

  gfMul1.io.in1.bits := 0.U(ww.W); gfMul1.io.in1.valid := false.B
  gfMul1.io.in2.bits := 0.U(ww.W); gfMul1.io.in2.valid := false.B
  gfMul2.io.in1.bits := 0.U(ww.W); gfMul2.io.in1.valid := false.B
  gfMul2.io.in2.bits := 0.U(ww.W); gfMul2.io.in2.valid := false.B
  gfAdd1.io.in1.bits := 0.U(ww.W); gfAdd1.io.in1.valid := false.B
  gfAdd1.io.in2.bits := 0.U(ww.W); gfAdd1.io.in2.valid := false.B

  io.memAddr  := 0.U
  io.memWData := 0.U
  io.memWrite := false.B
  io.done      := false.B
  io.busy      := state =/= sIdle
  io.corrupted := corruptReg

  def pz(u: UInt): UInt = u.pad(ww)
  def addrMij(i: UInt, j: UInt): UInt = baseMReg + i * numCoeffs.U + j

  switch(state) {
    is(sIdle) {
      when(io.start) {
        baseMReg   := io.baseM
        baseSReg   := io.baseS
        corruptReg := false.B
        iReg       := 0.U
        state      := sInitRoot
        printf("(CheckRoots) start\n")
      }
    }

    // One-cycle setup: initialise accumulator and starting power for root i.
    // Iterates j from numCoeffs-1 down to 0; curPow = r^(numCoeffs-1-j).
    // M[i][j] = r^(numCoeffs-1-j); sval = sum_j coeffs[j] * r^(numCoeffs-1-j).
    is(sInitRoot) {
      curPow  := 1.U
      svalReg := 0.U
      jReg    := (numCoeffs - 1).U
      state   := sWrMj
      printf("(CheckRoots) root[%d]=%d init j=%d\n",
             iReg, rootsVec(iReg), (numCoeffs - 1).U)
    }

    // Write M[i][j] = curPow (= r^(numCoeffs-1-j))
    is(sWrMj) {
      io.memAddr  := addrMij(iReg, jReg)
      io.memWData := curPow
      io.memWrite := true.B
      when(io.memReady) { state := sMulS }
    }

    // GFMul2: product = curPow * coeffs[j]
    is(sMulS) {
      when(gfMul2.io.in1.ready && gfMul2.io.in2.ready) {
        gfMul2.io.in1.bits  := pz(curPow)
        gfMul2.io.in1.valid := true.B
        gfMul2.io.in2.bits  := pz(io.coeffs(jReg))
        gfMul2.io.in2.valid := true.B
        state               := sMulW
      }
    }

    is(sMulW) {
      when(gfMul2.io.out.valid) {
        product := gfMul2.io.out.bits
        state   := sAddS
      }
    }

    // GFAdd: sval ^= product; stop when j==0, else go update power and decrement j
    is(sAddS) {
      when(gfAdd1.io.in1.ready && gfAdd1.io.in2.ready) {
        gfAdd1.io.in1.bits  := pz(svalReg)
        gfAdd1.io.in1.valid := true.B
        gfAdd1.io.in2.bits  := pz(product)
        gfAdd1.io.in2.valid := true.B
        state               := sAddW
      }
    }

    is(sAddW) {
      when(gfAdd1.io.out.valid) {
        svalReg := gfAdd1.io.out.bits
        when(jReg === 0.U) {
          state := sWrS
        }.otherwise {
          state := sPowS
        }
      }
    }

    // GFMul1: curPow = curPow * root[i]; then decrement j and write next M entry
    is(sPowS) {
      when(gfMul1.io.in1.ready && gfMul1.io.in2.ready) {
        gfMul1.io.in1.bits  := pz(curPow)
        gfMul1.io.in1.valid := true.B
        gfMul1.io.in2.bits  := rootsVec(iReg)
        gfMul1.io.in2.valid := true.B
        state               := sPowW
      }
    }

    is(sPowW) {
      when(gfMul1.io.out.valid) {
        curPow := gfMul1.io.out.bits
        jReg   := jReg - 1.U
        state  := sWrMj
        printf("(CheckRoots) root[%d] j=%d->%d newPow=%x\n",
               iReg, jReg, jReg - 1.U, gfMul1.io.out.bits)
      }
    }

    // Write S[i] = sval  (syndrome for root i)
    is(sWrS) {
      io.memAddr  := baseSReg + iReg
      io.memWData := svalReg
      io.memWrite := true.B
      when(io.memReady) {
        state := sNextI
        printf("(CheckRoots) root[%d] syndrome=%x\n", iReg, svalReg)
      }
    }

    // Update corrupted flag; advance to next root or finish
    is(sNextI) {
      when(svalReg =/= 0.U) {
        corruptReg := true.B
        printf("(CheckRoots) root[%d] nonzero syndrome => corrupted\n", iReg)
      }
      val nextI = iReg + 1.U
      iReg := nextI
      when(nextI >= numRoots.U) {
        state := sDone
      }.otherwise {
        state := sInitRoot
      }
    }

    is(sDone) {
      io.done := true.B
      state   := sIdle
      printf("(CheckRoots) done, corrupted=%b\n", corruptReg)
    }
  }
}
