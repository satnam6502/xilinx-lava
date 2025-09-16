VERSION=6.0.0.0
VERILATOR ?= verilator
VCOMP = $(VERILATOR) +1800-2017ext+sv +define+XIL_XECLIB=1 verilator.vlt -j  --threads 8 -y unisims --timing --binary -Wall -Wno-fatal 

.PHONY: build doc sdist test tests all formal pc

all:	test tests formal
        
tests: nandgate-sim onebitadd-sim adder4-sim sub4-sim twoSorter-sim twoSorterReg-sim sorter4-sim

formal:	adder4-eqy sub4-eqy muxN8-eqy twoSorter-eqy twoSorterReg-eqy

doc:
	cabal haddock

sdist:	build doc
	cabal sdist

upload:	sdist
	cabal upload dist-newstyle/sdist/xilinx-lava-$(VERSION).tar.gz

publish:	sdist
		cabal upload --publish dist-newstyle/sdist/xilinx-lava-$(VERSION).tar.gz

test:
	cabal test

nandgate-sim:
	$(VCOMP) --top-module altNandGate altNandGate.sv altNandGate_sim.sv 
	obj_dir/ValtNandGate +trace

onebitadd-sim:
	$(VERILATOR) +1800-2017ext+sv +define+XIL_XECLIB=1 verilator.vlt -y unisims --lint-only oneBitAdder.sv

adder4-sim:
	$(VCOMP) --top-module adder4_tb adder4.sv adder4_tb.sv
	obj_dir/Vadder4_tb +trace

sub4-sim:
	$(VCOMP) --top-module sub4_tb sub4.sv sub4_tb.sv
	obj_dir/Vsub4_tb +trace

twoSorter-sim:
	$(VCOMP) --top-module twoSorter_tb twoSorter.sv twoSorter_tb.sv
	obj_dir/VtwoSorter_tb +trace

twoSorterReg-sim:
	$(VCOMP) --top-module twoSorterReg_tb twoSorterReg.sv twoSorterReg_tb.sv
	obj_dir/VtwoSorterReg_tb +trace

sorter4-sim:
	$(VCOMP) --top-module sorter4_tb sorter4.sv sorter4_tb.sv
	obj_dir/Vsorter4_tb +trace

adder4-eqy:
	eqy -f adder4.eqy

sub4-eqy:
	eqy -f sub4.eqy

adder4-impl:
	vivado -mode batch -source adder4.tcl

nandgate-impl:
	vivado -mode batch -source nandgate.tcl

muxN8-eqy:
	eqy -f muxN8.eqy

twoSorter-eqy:
	eqy -f twoSorter.eqy

twoSorterReg-eqy:
	eqy -f twoSorterReg.eqy

pc:
	cp *.sv /Volumes/ardberg/home/satnam/xilinx-lava
	cp *.v /Volumes/ardberg/home/satnam/xilinx-lava
	cp *.tcl /Volumes/ardberg/home/satnam/xilinx-lava
	cp *.xdc /Volumes/ardberg/home/satnam/xilinx-lava

# Generate a GitHub CI workflow config file using haskell-ci.
workflow:
	haskell-ci github xilinx-lava.cabal

clean:
	rm -rf obj_dir
