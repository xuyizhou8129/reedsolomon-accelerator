// Testing RS(15,11) encode
#include <rocc.h>
#include <stdint.h>

// How to run (from the repo root, inside `nix develop`):
//   export RISCV=$PWD/.conda-env/riscv-tools PATH=$PATH:$PWD/.conda-env/riscv-tools/bin
//   make -C generators/rocc-acc/test                      # builds bin/rocc_rs_encode.riscv
//   make -C sims/verilator CONFIG=RoccAccConfig run-binary LOADMEM=1 \
//        BINARY=$PWD/generators/rocc-acc/test/bin/rocc_rs_encode.riscv
// The verdict is the "*** PASSED ***" / "*** FAILED ***" line in
// sims/verilator/output/chipyard.harness.TestHarness.RoccAccConfig/rocc_rs_encode.out

#define RS_ENCODE 1

static uint8_t message[16]  __attribute__((aligned(8))) = {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
static uint8_t codeword[16] __attribute__((aligned(8)));

int main() {
    uint64_t status;
    ROCC_INSTRUCTION_DSS(0, status, message, codeword, RS_ENCODE);
    __asm__ __volatile__ ("fence" ::: "memory"); // wait for the accelerator's writes
    return status != 0 || codeword[11] != 237 || codeword[12] != 235 || codeword[13] != 91 || codeword[14] != 80;
}
