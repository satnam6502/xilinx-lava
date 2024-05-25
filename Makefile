VERSION=6.0.0.0
VERILATOR ?= verilator

.PHONT: build doc sdist

build:
	cabal build

doc:
	cabal haddock

sdist:	build doc
	cabal sdist

upload:	sdist
	cabal upload dist-newstyle/sdist/xilinx-lava-$(VERSION).tar.gz

publish:	sdist
		cabal upload --publish dist-newstyle/sdist/xilinx-lava-$(VERSION).tar.gz

nandgate_sim:
	cabal test
	# $(VERILATOR) +1800-2017ext+sv verilator.vlt -trace -Wall -cc --build --clk clk --top-module nandgate_sim nandgate.sv nandgate_sim.sv -exe nandgate_sim_driver.cpp
	$(VERILATOR) +1800-2017ext+sv verilator.vlt --binary --trace -Wall -cc --build --clk clk --top-module nandgate_sim nandgate.sv nandgate_sim.sv 
	obj_dir/Vnandgate_sim +trace

# Generate a GitHub CI workflow config file using haskell-ci.
workflow:
	haskell-ci github xilinx-lava.cabal

clean:
	rm -rf obj_dir
