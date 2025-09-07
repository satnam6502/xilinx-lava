set project_name "adder4"
set top_module "adder4"
set source_file "adder4.sv"
set part "xc7k70tfbv676-1"
set build_dir "build"
set reports_dir "build/reports"
set checkpoints_dir "build/checkpoints"
set impl_checkpoint "$checkpoints_dir/post_route.dcp"

file mkdir $build_dir
file mkdir $reports_dir
file mkdir $checkpoints_dir

create_project -in_memory -part $part
# Set project properties for SystemVerilog
set_property target_language SystemVerilog [current_project]
set_property default_lib xil_defaultlib [current_project]

read_verilog $source_file
synth_design -top $top_module -part $part
opt_design
place_design
route_design
phys_opt_design
write_checkpoint -force $impl_checkpoint