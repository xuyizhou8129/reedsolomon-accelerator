package roccacc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

object RS_Decoder {
  val DEFAULT_N = 15  // codeword length
  val DEFAULT_K = 11  // message length
  val DEFAULT_FIELD_SIZE = 8
  // Default generator polynomial coefficients for RS(15,11)
  // (x-1)(x-2)(x-3)(x-4)=x^4-10x^3+35x^2-50x+24
  val DEFAULT_GENERATOR_COEFFS = Seq(1, 246, 35, 206, 24)
}

class RS_Decoder(
  fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE,
  n: Int = RS_Decoder.DEFAULT_N,
  k: Int = RS_Decoder.DEFAULT_K,
  generatorCoeffs: Seq[Int] = RS_Decoder.DEFAULT_GENERATOR_COEFFS
) extends Module {
  // Calculate error correction capability: t = (n - k) / 2
  require((n - k) % 2 == 0, "n - k must be even for RS codes")
  val t = (n - k) / 2
  
  // Convert generator coefficients to UInt registers
  val generatorPolynomial = VecInit(generatorCoeffs.map(_.U(fieldSize.W)))
  val io = IO(new Bundle {
    val message = Flipped(Decoupled(Vec(k, UInt(fieldSize.W))))
    val out = Valid(Vec(n, UInt(fieldSize.W)))
  })
  object RS_DecoderState extends ChiselEnum {
    val idle, extended, find_coeffs, multiplying, deducting, done = Value
  }

  // RS Decoder specific registers
  val rs_decoder_state = RegInit(RS_DecoderState.idle)
  val messageReg = RegInit(VecInit(Seq.fill(k)(0.U(fieldSize.W))))
  val intermediateReg = RegInit(VecInit(Seq.fill(n)(0.U(fieldSize.W))))  // Size n to hold extended message
  val codewordReg = RegInit(VecInit(Seq.fill(n)(0.U(fieldSize.W))))
  val foundCoeffIndex = RegInit(0.U(log2Ceil(n).W))
  val divisionResult = RegInit(0.U(fieldSize.W))
  val findCoeffsDone = RegInit(false.B)
  io.message.ready := rs_decoder_state === RS_DecoderState.idle
  io.out.valid := false.B
  io.out.bits := VecInit(Seq.fill(n)(0.U(fieldSize.W)))
  val divider1 = Module(new GFDiv(fieldSize))
  divider1.io.in1.bits := 0.U((2 * fieldSize).W)
  divider1.io.in1.valid := false.B
  divider1.io.in2.bits := 0.U((2 * fieldSize).W)
  divider1.io.in2.valid := false.B
  val multiplier1 = Module(new GFMul(fieldSize))
  multiplier1.io.in1.bits := 0.U((2 * fieldSize).W)
  multiplier1.io.in1.valid := false.B
  multiplier1.io.in2.bits := 0.U((2 * fieldSize).W)
  multiplier1.io.in2.valid := false.B
  val adder1 = Module(new GFAdd(fieldSize))
  adder1.io.in1.bits := 0.U((2 * fieldSize).W)
  adder1.io.in1.valid := false.B
  adder1.io.in2.bits := 0.U((2 * fieldSize).W)
  adder1.io.in2.valid := false.B
  val mul_count = RegInit(0.U(log2Ceil(n-k+1).W))  // +1 to handle n-k as a value
  val add_count = RegInit(0.U(log2Ceil(n-k+1).W))  // Counter for deducting state
  val multiplyingDone = RegInit(false.B)
  val deductingDone = RegInit(false.B)
  // Store multiplied values for (n-k) generator coefficients
  val multipliedValues = RegInit(VecInit(Seq.fill(n-k)(0.U(fieldSize.W))))
  
  switch(rs_decoder_state) {
  is(RS_DecoderState.idle) { // IDLE
      when(io.message.valid) {
        for (i <- 0 until k) {
          messageReg(i) := io.message.bits(i)
        }
      mul_count := 0.U
      rs_decoder_state := RS_DecoderState.extended
      printf("(RS_Decoder) In idle state, received message coeffs")
    }
  }
  is(RS_DecoderState.extended) {
    val extender = Module(new RS_extender(fieldSize, n, k))
    extender.io.message := messageReg
    intermediateReg := extender.io.out
    printf("(RS_Decoder) In extended state, extended message done")
    // Reset find_coeffs state
    findCoeffsDone := false.B
    foundCoeffIndex := 0.U
    // Transition to next state immediately (extension is combinational)
    rs_decoder_state := RS_DecoderState.find_coeffs
  }
  is(RS_DecoderState.find_coeffs) { 
    when(!findCoeffsDone) {
      // Build a vector of non-zero flags
      val nonZeroFlags = VecInit((0 until n).map(i => intermediateReg(i) =/= 0.U))
      // Use PriorityEncoder to find first non-zero index
      val firstNonZeroIdx = PriorityEncoder(nonZeroFlags.asUInt)
      val hasNonZero = nonZeroFlags.asUInt.orR
      
      when(hasNonZero) {
        foundCoeffIndex := firstNonZeroIdx
        val foundCoeff = intermediateReg(firstNonZeroIdx)
        printf("(RS_Decoder) Found first non-zero coefficient at index %d: %b\n", firstNonZeroIdx, foundCoeff)
        when(firstNonZeroIdx >= k.U) {
          // Skip division, go directly to done
          printf("(RS_Decoder) First non-zero at index %d is in last %d positions, skipping to done\n", firstNonZeroIdx, (n-k-1).U)
          findCoeffsDone := true.B
          rs_decoder_state := RS_DecoderState.done
        }.otherwise {
          // Start division: divide found coefficient by first generator coefficient
          when(divider1.io.in1.ready && divider1.io.in2.ready) {
            divider1.io.in1.bits := foundCoeff  // Zero-extend to (2*fieldSize).W
            divider1.io.in1.valid := true.B
            divider1.io.in2.bits := generatorPolynomial(0)  // Zero-extend to (2*fieldSize).W
            divider1.io.in2.valid := true.B
            printf("(RS_Decoder) Dividing %b by %b (first generator coeff)\n", foundCoeff, generatorPolynomial(0))
          }
        }
    }.otherwise {
        // All coefficients are zero
        printf("(RS_Decoder) All coefficients are zero\n")
        findCoeffsDone := true.B
        rs_decoder_state := RS_DecoderState.done
      }
    }
      
      // Check if division is complete
      when(divider1.io.out.valid) {
        divisionResult := divider1.io.out.bits
        divider1.io.in1.valid := false.B
        divider1.io.in2.valid := false.B
        findCoeffsDone := true.B
        printf("(RS_Decoder) Division result: %b\n", divisionResult)
        // Reset multiplying state
        mul_count := 0.U
        multiplyingDone := false.B
        rs_decoder_state := RS_DecoderState.multiplying
      }
    }
  
  is(RS_DecoderState.multiplying) { // MULTIPLYING
    // Multiply each generator coefficient with division result
    // and store the results for later addition
    when(!multiplyingDone) {
      // Check if we've processed all (n-k) generator coefficients
      when(mul_count < (n - k).U) {
        // Start multiplication: generatorPolynomial(mul_count) * divisionResult
        when(multiplier1.io.in1.ready && multiplier1.io.in2.ready) {
          multiplier1.io.in1.bits := generatorPolynomial(mul_count).pad(2 * fieldSize)
          multiplier1.io.in1.valid := true.B
          multiplier1.io.in2.bits := divisionResult.pad(2 * fieldSize)
          multiplier1.io.in2.valid := true.B
          printf("(RS_Decoder) Multiplying generator[%d]=%b by divisionResult=%b\n", 
                 mul_count, generatorPolynomial(mul_count), divisionResult)
        }
        
        // When multiplication completes, store the result
        when(multiplier1.io.out.valid) {
          multipliedValues(mul_count) := multiplier1.io.out.bits
          multiplier1.io.in1.valid := false.B
          multiplier1.io.in2.valid := false.B
          printf("(RS_Decoder) Stored multiplied value[%d]: %b\n", mul_count, multiplier1.io.out.bits)
          mul_count := mul_count + 1.U
        }
      }.otherwise {
        // All (n-k) multiplications complete
        printf("(RS_Decoder) All multiplications complete, processed %d coefficients\n", (n-k).U)
        multiplyingDone := true.B
        add_count := 0.U
        deductingDone := false.B
        // Transition to deducting state to perform GF additions
        rs_decoder_state := RS_DecoderState.deducting
      }
    }
  }
  is(RS_DecoderState.deducting) { // DEDUCTING
    // Use GFAdd to add each multiplied value to intermediateReg
    when(!deductingDone) {
      // Check if we've processed all (n-k) additions
      when(add_count < (n - k).U) {
        val targetIdx = foundCoeffIndex + add_count
        when(targetIdx < n.U) {
          // Start GF addition: intermediateReg(targetIdx) + multipliedValues(add_count)
          when(adder1.io.in1.ready && adder1.io.in2.ready) {
            adder1.io.in1.bits := intermediateReg(targetIdx).pad(2 * fieldSize)
            adder1.io.in1.valid := true.B
            adder1.io.in2.bits := multipliedValues(add_count).pad(2 * fieldSize)
            adder1.io.in2.valid := true.B
            printf("(RS_Decoder) Adding intermediateReg[%d]=%b + multipliedValues[%d]=%b\n", 
                   targetIdx, intermediateReg(targetIdx), add_count, multipliedValues(add_count))
          }
          
          // When addition completes, update intermediateReg
          when(adder1.io.out.valid) {
            intermediateReg(targetIdx) := adder1.io.out.bits
            adder1.io.in1.valid := false.B
            adder1.io.in2.valid := false.B
            printf("(RS_Decoder) Deducted: intermediateReg[%d] = %b\n", targetIdx, adder1.io.out.bits)
            add_count := add_count + 1.U
          }
        }.otherwise {
          // Target index out of bounds, skip this addition
          printf("(RS_Decoder) Target index %d out of bounds, skipping\n", targetIdx)
          add_count := add_count + 1.U
        }
      }.otherwise {
        // All (n-k) additions complete
        printf("(RS_Decoder) Deduction complete, processed %d additions\n", (n-k).U)
        deductingDone := true.B
        add_count := 0.U
        // Go back to find_coeffs to find next non-zero
        findCoeffsDone := false.B
        rs_decoder_state := RS_DecoderState.find_coeffs
      }
    }
  }
  is(RS_DecoderState.done) { // DONE
    printf("(RS_Decoder) Done, returning %d codeword coefficients\n", n.U)
    rs_decoder_state := RS_DecoderState.idle
    io.out.valid := true.B
    // Output the n codeword coefficients
    for (i <- 0 until n) {
      io.out.bits(i) := intermediateReg(i)
    }
    }
  }
}

class RS_extender(
  fieldSize: Int = GFOperations.DEFAULT_FIELD_SIZE,
  n: Int = RS_Decoder.DEFAULT_N,
  k: Int = RS_Decoder.DEFAULT_K,
) extends Module{
  val io = IO(new Bundle {
    val message = Input(Vec(k, UInt(fieldSize.W)))
    val out = Output(Vec(n, UInt(fieldSize.W)))
  })
  for (i <- 0 until k) {
      io.out(i) := io.message(i)
  }
    // Add (n-k) zeros to complete the extended message
  for (i <- k until n) {
    io.out(i) := 0.U(fieldSize.W)
  }
}