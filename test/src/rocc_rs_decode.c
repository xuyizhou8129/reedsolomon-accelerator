// Testing RS(15,11) decode of a codeword with two errors (symbol 0 ^= 5, symbol 11 ^= 13)
#include <rocc.h>
#include <stdint.h>
// How to run (from the repo root, inside `nix develop`):
//   export RISCV=$PWD/.conda-env/riscv-tools PATH=$PATH:$PWD/.conda-env/riscv-tools/bin
//   make -C generators/rocc-acc/test                      # builds bin/rocc_rs_decode.riscv
//   make -C sims/verilator CONFIG=RoccAccConfig run-binary LOADMEM=1 \
//        BINARY=$PWD/generators/rocc-acc/test/bin/rocc_rs_decode.riscv
// The verdict is the "*** PASSED ***" / "*** FAILED ***" line in
// sims/verilator/output/chipyard.harness.TestHarness.RoccAccConfig/rocc_rs_decode.out
#define RS_DECODE 2

static uint8_t received[16] __attribute__((aligned(8))) = {7, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 224, 235, 91, 80};
static uint8_t errvec[16]   __attribute__((aligned(8)));

int main() {
    uint64_t status;
    ROCC_INSTRUCTION_DSS(0, status, received, errvec, RS_DECODE);
    __asm__ __volatile__ ("fence" ::: "memory"); // wait for the accelerator's writes
    return status != 3 || errvec[0] != 5 || errvec[11] != 13; // 3 = corrupted | solved
}
