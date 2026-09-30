# RS_ENCODE and RS_DECODE — What Comes Back

This page covers the results of the two RS instructions: what is written to the output buffer, what the status word in rd means, and when software can safely read them. How to issue the instructions is covered in `encode_decode_instructions.md`.

Results come back through two channels:

| Channel | Carries | Size |
|---------|---------|------|
| **Output buffer at rs2** | The codeword, or the error vector | n = 15 bytes |
| **rd** | A status word | 2 meaningful bits |

The data goes through memory because 15 symbols do not fit in a 64-bit register.

---

## Output buffer

### After RS_ENCODE: the codeword

The codeword is **systematic**. The first k bytes are the message unchanged, and the last n − k bytes are the parity symbols.

| Bytes | Contents |
|-------|----------|
| 0–10 | The 11 message symbols, copied from the input |
| 11–14 | The 4 parity symbols |

Example from `test/src/rocc_rs.c`:

```
message  :  2  3  4  5  6  7  8  9 10 11 12
codeword :  2  3  4  5  6  7  8  9 10 11 12 237 235 91 80
```

### After RS_DECODE: the error vector

The decoder does not write the corrected codeword. It writes the **error vector** e, which has the same length as the codeword. e[j] is the value that was added to symbol j, and it is 0 where the symbol was not damaged.

Software gets the corrected codeword by XORing the error vector into what it received:

```c
for (int j = 0; j < 15; j++)
    corrected[j] = received[j] ^ errvec[j];
```

The message is then bytes 0–10 of `corrected`.

Example with two symbols damaged:

```
received : 2^5  3  4  5  6  7  8  9 10 11 12 237^13 235 91 80
errvec   :   5  0  0  0  0  0  0  0  0  0  0     13   0  0  0
```

The error vector is only meaningful when the status says **solved**. For a clean codeword it is all zeros.

**Byte 15 is never written**, for either instruction.

---

## Status word in rd

The status is built in `Vcode.scala` as `Cat(solved, corrupted)` and zero-extended to 64 bits.

| Bit | Name | Meaning |
|-----|------|---------|
| 0 | corrupted | The received codeword had at least one error |
| 1 | solved | The decoder found every error and the error vector is valid |
| 63–2 | — | Always 0 |

This gives three possible values after a decode:

| rd | Meaning | What to do |
|----|---------|------------|
| **0** | Clean codeword, no errors | Use the received data as is. The error vector is all zeros. |
| **3** | Errors found and corrected | XOR the error vector into the received data. |
| **1** | Errors found but not correctable, for example more than t = 2 damaged symbols | Discard the block. Do not trust the error vector. |

A value of 2 cannot occur, because the decoder only reports solved after first finding the codeword corrupted.

**After RS_ENCODE, rd is always 0.** Encoding cannot fail, and `RSUnit` clears both flags at the start of every operation. The response is still useful as a completion signal.

Matching constants (`test/src/rocc_rs_decode.c` checks for `status == 3`):

```c
#define STATUS_CORRUPTED 0x1
#define STATUS_SOLVED    0x2
```

---

## When results are visible

The accelerator finishes in this order:

1. The RS unit completes.
2. Every store to the output buffer is sent and **acknowledged by the L1 data cache**.
3. Only then, the status is sent to rd, if the instruction had xd = 1.
4. `busy` drops once the core has accepted the response.

Because of step 2, the output buffer is complete by the time rd is written or `busy` drops. Software still needs a `fence` before reading the buffer, as explained in `encode_decode_instructions.md`. A load that does not use rd is not otherwise held back.

With **xd = 0**, no response is sent. The `fence` alone waits for `busy` to drop.

---

## Timing

Cycle counts for the RS unit alone, measured in `RSUnitSimpleTest`. They do not include the memory transfers or the core issuing the instruction.

| Operation | Cycles |
|-----------|--------|
| Encode | 972 |
| Decode, 1 error | 1,142 |
| Decode, 2 errors | 6,003 |

Decode time depends on the number of errors, because the decoder tries possible error positions one combination at a time. The two-error case has more combinations to try.

---

## Errors the accelerator reports

- **Illegal funct7.** The accelerator raises its interrupt line. No status and no output are produced.
- **Uncorrectable block.** Reported through the status word, with rd = 1.
- **Bad buffer address.** Not reported. See the limits in `encode_decode_instructions.md`.
