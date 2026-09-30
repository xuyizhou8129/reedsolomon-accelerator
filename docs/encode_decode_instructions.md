# RS_ENCODE and RS_DECODE — Issuing the Instructions

This page covers how software asks the accelerator to **encode** or **decode** one Reed-Solomon block: which instruction bits to set, what goes in each register, and how the data must be laid out in memory. What comes back is covered in `encode_decode_response.md`.

The hardware side lives in `Vcode.scala` (sections **DATA FETCH FOR ENCODER/DECODER** and **EXECUTE RS_Unit**). The end-to-end tests are `test/src/rocc_rs.c` (encode) and `test/src/rocc_rs_decode.c` (decode).

---

## Code parameters

These are fixed when the hardware is generated. Changing them means regenerating the design.

| Parameter | Value | Where it is set |
|-----------|-------|-----------------|
| Field | GF(2^8), polynomial **`0x11D`** | `GFOperations.DEFAULT_FIELD_SIZE`, `GFReduceGen.GF256_POLY` |
| Codeword length **n** | 15 symbols | `RS_Encoder.DEFAULT_N` |
| Message length **k** | 11 symbols | `RS_Encoder.DEFAULT_K` |
| Correctable errors **t** | 2 symbols, i.e. (n − k) / 2 | derived |

---

## Instruction encoding

Both instructions are RoCC **R-type** instructions on the **custom-0** opcode (`0b0001011`). The accelerator only looks at **funct7** to tell them apart.

| Field | Bits | RS_ENCODE | RS_DECODE |
|-------|------|-----------|-----------|
| funct7 | 31:25 | **`0000001`** | **`0000010`** |
| rs2 | 24:20 | output buffer address | output buffer address |
| rs1 | 19:15 | input buffer address | input buffer address |
| funct3 = {xd, xs1, xs2} | 14:12 | `111` (or `011`, see below) | `111` (or `011`) |
| rd | 11:7 | status register | status register |
| opcode | 6:0 | `0001011` | `0001011` |

The funct7 values are defined in `Instructions.scala`. The decode table in `IDecode.scala` (`BinOpDecode`) maps them to the execution unit codes `UNIT_RS_ENC` and `UNIT_RS_DEC`. Any funct7 not in the table raises the accelerator's interrupt as an illegal instruction.

**xs1 and xs2 must be set.** The core only guarantees that rs1 and rs2 carry the register values when these bits are 1.

**xd is optional.** With xd = 1 the accelerator writes a status word into rd when it finishes. With xd = 0 there is no response and software must use `fence` to wait. The xd = 1 form is the tested one.

---

## Operands

| Register | RS_ENCODE | RS_DECODE |
|----------|-----------|-----------|
| **rs1** | Address of the **message**, k = 11 symbols | Address of the **received codeword**, n = 15 symbols |
| **rs2** | Address where the **codeword** is written, n = 15 symbols | Address where the **error vector** is written, n = 15 symbols |
| **rd** | Status, always 0 | Status, bit 0 = corrupted, bit 1 = solved |

The registers carry **addresses**, not data. The accelerator reads and writes the buffers itself through the core's L1 data cache.

---

## Memory layout

**One symbol per byte.** Symbol j is at byte offset j from the buffer address. In C this is just a `uint8_t` array indexed by symbol number. The hardware unpacks bytes lowest-address first, which matches RISC-V's little-endian loads, so no reordering is needed.

**Both buffers must be 8-byte aligned.** The accelerator moves data in whole 64-bit words, and a misaligned address is not handled.

**Reads are rounded up to whole words.** Encode needs 11 bytes but reads 16, and decode needs 15 bytes but reads 16. The extra bytes are ignored, but they must be readable memory. The simplest rule is to give every buffer 16 bytes.

**Writes are exact.** Exactly n = 15 bytes are written to rs2. The last word is written with a byte-masked store, so byte 15 and anything after it is left untouched.

**Input and output may be the same buffer.** The whole input is read before any output is written.

| Buffer | Bytes the hardware touches | Suggested declaration |
|--------|----------------------------|-----------------------|
| Encode input | reads 0–15 | `uint8_t msg[16] __attribute__((aligned(8)))` |
| Decode input | reads 0–15 | `uint8_t rx[16] __attribute__((aligned(8)))` |
| Output | writes 0–14 | `uint8_t out[16] __attribute__((aligned(8)))` |

---

## Issuing from C

The macros in `test/include/rocc.h` emit the instruction with `.insn`. The first argument selects custom-0, and the last is funct7.

```c
#include <rocc.h>
#include <stdint.h>

#define RS_ENCODE 1
#define RS_DECODE 2

static uint8_t msg[16]      __attribute__((aligned(8))) = {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
static uint8_t codeword[16] __attribute__((aligned(8)));

uint64_t status;
ROCC_INSTRUCTION_DSS(0, status, msg, codeword, RS_ENCODE);   // xd = xs1 = xs2 = 1
asm volatile ("fence" ::: "memory");
```

**Always follow the instruction with `fence` before touching the output buffer.** There are two reasons:

1. The RoCC macros are not compiler barriers, so the compiler may move loads of `codeword` above the instruction.
2. The core only stalls on the accelerator when an instruction reads rd. A load from `codeword` does not read rd, so without a fence it can run before the accelerator has finished. Rocket's `fence` waits until the accelerator drops `busy`.

Without a status register, use the xd = 0 form. The `fence` is then the only way to know the operation finished.

```c
ROCC_INSTRUCTION_SS(0, msg, codeword, RS_ENCODE);            // xd = 0, no response
asm volatile ("fence" ::: "memory");
```

---

## What the hardware does with the command

1. **Capture and decode.** The command is latched into `roccCmd`, and funct7 is decoded into the target unit.
2. **Fetch.** `BlockFetcher` reads the input buffer at rs1, 2 words for either operation.
3. **Compute.** The fetched bytes are unpacked into symbols and handed to `RSUnit`, which runs the encoder or the decoder.
4. **Write back.** `BlockWriter` stores the n output symbols to rs2. It waits for the cache to acknowledge every store.
5. **Respond.** If xd = 1, the status word is sent to rd. `busy` stays high until this point.

---

## Limits and things not handled

- **One command at a time.** The accelerator holds `cmd.ready` low while busy, so the core stalls a second RoCC instruction until the first finishes.
- **No memory fault reporting.** An unmapped or misaligned buffer address is not reported back to software.
- **Tested bare-metal only.** The tests run in machine mode with no virtual memory. Use under an operating system with paging has not been tried.
