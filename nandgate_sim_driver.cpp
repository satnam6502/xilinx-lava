#include <stdlib.h>
#include <memory>
#include "Vnandgate_sim.h"
#include "verilated.h"

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

int main(int argc, char **argv) {

Vnandgate_sim *tb = new Vnandgate_sim;
Verilated::traceEverOn(true);

tb->clk = 1;
tb->eval();
while(!Verilated::gotFinish()) {
Verilated::timeInc(1);
  tb->clk ^= 1;
  tb->eval();
}
tb->final();
return 0;
}
