For CheckRoots module, I would like it to be individually tested before combining it with the rest of the system.
1. Compare CheckRoots.scala with software/software-model/Decoder/check_roots.py , in terms of calculation algorithm and functionality, they should be equivalent, help me implement CheckRoots.scala
2. In CheckRoots.scala, the roots should be configured as global variables. CheckRoots.scala should not keep any sort of local buffer of the entire matrix. It should write caculated values of the matrix into an external scratchpad, which is an instance of Matrix.sclala. The corrupted signal is an output signal
3. Create a Scala version of the software model in /pool/xuyi/Project1_C/chipyardfork/accelerator/generators/rocc-acc/src/test/scala/roccacc
called sw_checkroots.scala, reference the SWModel.scala for structure
This software model should functionally the same as solve_matrix.py
4. Create a CheckRootSimpleTest file in the same directory as RSEncoderSimpleTest
Use the software golden for differential testing in that file
Reference RSEncoderSimpleTest for structure
5. You can verify you design by 
cd /pool/xuyi/Project1_C/chipyardfork/accelerator
nix develop
sbt "roccacc/testOnly roccacc.RSEncoderSimpleTest" (I used RSEncoderSimpleTest here as an example)
6. For any design decisions you made and progress, list them out in log.md in docs/


1. Modify CheckRoots so it matches check_roots.py: for each root from a fixed compile-time list, evaluate the received polynomial at that root to get syndrome values; set corrupted if any syndrome is nonzero; store the Vandermonde-style matrix entries in external memory as you go instead of caching the whole matrix inside the module. Roots must be generator or companion constants, not free-running FSM inputs. Do not keep a full numRoots by numCoeffs register array inside CheckRoots; only keep small state such as loop indices, current power of the root, running syndrome, and GF unit operands. Connect to storage the same way MatSolve does, with a single-word memory port using address, read and write strobes, write data, read data, and a ready signal, and document the address map for matrix rows and columns. Do not instantiate Matrix inside CheckRoots; if Matrix.scala is the backing store, the parent owns it and must adapt it to this port because Matrix today uses wide vector ports, not one element per cycle. Expose corrupted and done and state clearly when corrupted is valid.
2. You can verify you design by 
cd /pool/xuyi/Project1_C/chipyardfork/accelerator
nix develop
sbt "roccacc/testOnly roccacc.RSEncoderSimpleTest" (I used RSEncoderSimpleTest here as an example)
3. For any design decisions you made and progress, list them out in log.md in docs/


1. Implement CheckRoots like check_roots.py with compile-time roots and coefficients only on io.coeffs (no scratchpad reads for them). Keep the single-word write port and address map unchanged: write M row-major at baseM, write syndromes S[i] at baseS. Do not read M back. Expose start, busy, done, and corrupted; avoid a full internal M, only small FSM and GF temps.
2. You can verify you design by 
cd /pool/xuyi/Project1_C/chipyardfork/accelerator
nix develop
sbt "roccacc/testOnly roccacc.RSEncoderSimpleTest" (I used RSEncoderSimpleTest here as an example)
3. For any design decisions you made and progress, list them out in log.md in docs/