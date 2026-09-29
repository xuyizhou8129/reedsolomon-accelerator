// End-to-end Reed-Solomon RS(15,11) over GF(2^8) through the accelerator.
//
// Instruction contract (see Vcode.scala, DATA FETCH FOR ENCODER/DECODER):
//   rs1 = input symbols, one per byte, 8-byte aligned
//         (encode reads K, decode reads N)
//   rs2 = output buffer, 8-byte aligned, N bytes written
//         (encode: codeword, decode: error vector)
//   rd  = status, bit 0 = corrupted, bit 1 = solved (0 for encode)
#include <rocc.h>
#include <stdint.h>

#define RS_ENCODE 1
#define RS_DECODE 2
#define N 15
#define K 11

#define STATUS_CORRUPTED 0x1
#define STATUS_SOLVED    0x2

// Byte 15 of each output buffer is a guard: the accelerator writes exactly N
// bytes, so it must survive untouched.
#define GUARD 0xA5

static uint8_t message[16]  __attribute__((aligned(8))) = {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
static uint8_t codeword[16] __attribute__((aligned(8)));
static uint8_t received[16] __attribute__((aligned(8)));
static uint8_t errvec[16]   __attribute__((aligned(8)));

// Codeword from RSUnitSimpleTest (hardware matches the Scala software model)
static const uint8_t expected[N] = {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 237, 235, 91, 80};

// The RoCC macros are not compiler barriers, and a later load that does not
// use rd could run before the accelerator finishes. fence waits on io.busy.
#define ROCC_SYNC() __asm__ __volatile__ ("fence" ::: "memory")

int main() {
    uint64_t status;

    codeword[15] = GUARD;
    ROCC_INSTRUCTION_DSS(0, status, message, codeword, RS_ENCODE);
    ROCC_SYNC();
    if (status != 0) return 1;
    for (int i = 0; i < N; i++)
        if (codeword[i] != expected[i]) return 10 + i;
    if (codeword[15] != GUARD) return 2;

    // Two errors, the most RS(15,11) can correct
    for (int i = 0; i < N; i++) received[i] = codeword[i];
    received[0]  ^= 5;
    received[11] ^= 13;

    errvec[15] = GUARD;
    ROCC_INSTRUCTION_DSS(0, status, received, errvec, RS_DECODE);
    ROCC_SYNC();
    if (status != (STATUS_CORRUPTED | STATUS_SOLVED)) return 3;
    for (int i = 0; i < N; i++)
        if ((uint8_t)(received[i] ^ errvec[i]) != expected[i]) return 30 + i;
    if (errvec[15] != GUARD) return 4;

    // A clean codeword decodes as not corrupted with a zero error vector
    ROCC_INSTRUCTION_DSS(0, status, codeword, errvec, RS_DECODE);
    ROCC_SYNC();
    if (status & STATUS_CORRUPTED) return 5;
    for (int i = 0; i < N; i++)
        if (errvec[i] != 0) return 50 + i;

    return 0;
}
