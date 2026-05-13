#ROCC Accelerator - Chipyard Integration Guide

## Step 1: Register as Chipyard Submodule

Teach Chipyard Git repo about my module by registering it as a submodule
(Force it to be registered at a special commit)

## Step 2: Update Chipyard's build.sbt

Edit `build.sbt` in the Chipyard root to include your project:

1. Define lazy val roccacc
2. Add roccacc to the aggregate list (dependency of lazy val chipyard)

## Step 3: Create Chipyard Configuration

Create a new configuration file in `generators/chipyard/src/main/scala/config/`:

## Step 4: Create Pingpong Test

Sims verilator, run a make command with a configs set

1. Building Chipyard (only need to be done once)
source env.sh
(Set up environment variables)
./scripts/build-setup.sh -s 1
./build-setup.sh --skip-conda --skip-precompile --build-circt --skip-firesim --skip-marshal
(Build and skip conda initialization)

2. Building tests
cd rocc-acc/test
make clean
make build

3. Using sbt to do hardware compiling (from chisel/scala to verilog)
cd /pool/xuyi/Project1_C/chipyardfork/accelerator && sbt
project roccacc
compile
(hardware compilation, compile chisel and scala to verilog, create hardware description)

4.Compilation from verilog to binary
cd /pool/xuyi/Project1_C/chipyardfork/accelerator
source env.sh
cd sims/verilator
make CONFIG=RoccAccConfig

5.Running Code: 
./simulator-chipyard.harness-RoccAccConfig /pool/xuyi/Project1_C/chipyardfork/accelerator/generators/rocc-acc/test/bin/rocc_add.riscv