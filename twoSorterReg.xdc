set_property -dict { PACKAGE_PIN R4    IOSTANDARD LVCMOS33 } [get_ports { clk }]; #IO_L13P_T2_MRCC_34 Sch=sysclk
create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports {clk}];

set_property -dict { PACKAGE_PIN G4   IOSTANDARD LVCMOS33} [get_ports { rstN }]; #IO_L12N_T1_MRCC_35 Sch=cpu_resetn
