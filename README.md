# xilinx-lava
A Haskell library for combinator-based design of systems, especially digital hardware.

![example workflow](https://github.com/satnam6502/xilinx-lava/actions/workflows/haskell-ci.yml/badge.svg)

Tested with GHC versions 9.4.8 and 9.8.2.

`xilinx-lava` is library that implements a domain specific language (DSL) digital hardware design, originally developed at Xilinx for producing FPGA designs with high-level layout information. The current version produces generic SystemVerilog code that can be used for any kind of digital hardware design project.

## Installation
You can use Lava to generate and simulate digital circuits without installing any extra external tools. To install Lava, first install the Haskell compiler [GHC](https://www.haskell.org/ghc/download.html) from any version from 9.4.8 to 9.8.2 inclusive (the versions tested by GitHub CI), and the [cabal](https://www.haskell.org/cabal/) Haskell package manager. One easy way you can install GHC and `cabal` is by using the [ghcup](https://www.haskell.org/ghcup/) tool. If you are not sure which version to install, pick 9.8.2. Also install `cabal` using `ghcup` (pick the latest, recommended version).

First, clone the `xilinx-lava` repo.

```console
$ git clone git@github.com:satnam6502/xilinx-lava.git
```

Then move into the directory and run the tests. This will cause `xilinx-lava` to be built and installed and the tests will make sure the installtion was successful.

```console
$ cd xilinx-lava
$ cabal test
...
Completed    bitvec-1.1.5.0 (lib)
Configuring test suite 'test-verilator' for xilinx-lava-6.0.0.0..
Preprocessing test suite 'test-verilator' for xilinx-lava-6.0.0.0..
Building test suite 'test-verilator' for xilinx-lava-6.0.0.0..
[1 of 1] Compiling Main             ( test-verilator/Main.hs, /home/satnam/xilinx-lava/dist-newstyle/build/x86_64-linux/ghc-9.8.2/xilinx-lava-6.0.0.0/t/test-verilator/build/test-verilator/test-verilator-tmp/Main.o )
[2 of 2] Linking /home/satnam/xilinx-lava/dist-newstyle/build/x86_64-linux/ghc-9.8.2/xilinx-lava-6.0.0.0/t/test-verilator/build/test-verilator/test-verilator
Running 1 test suites...
Test suite test-verilator: RUNNING...
Test suite test-verilator: PASS
Test suite logged to:
/home/satnam/xilinx-lava/dist-newstyle/build/x86_64-linux/ghc-9.8.2/xilinx-lava-6.0.0.0/t/test-verilator/test/xilinx-lava-6.0.0.0-test-verilator.log
1 of 1 test suites (1 of 1 test cases) passed.
```

The Hackage page for `xilinx-lava` is at [https://hackage.haskell.org/package/xilinx-lava](https://hackage.haskell.org/package/xilinx-lava).
