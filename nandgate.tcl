set project_name "nandgate"
set top_module "nandgate"
set source_file "nandgate.sv"
set part "xc7a200tsbg484-1"
set build_dir "build"
set reports_dir "build/reports"
set checkpoints_dir "build/checkpoints"
set impl_checkpoint "$checkpoints_dir/post_route.dcp"

file mkdir $build_dir
file mkdir $reports_dir
file mkdir $checkpoints_dir

create_project -in_memory -part $part

read_verilog $source_file
synth_design -top $top_module -part $part
opt_design
place_design
route_design
phys_opt_design
write_checkpoint -force $impl_checkpoint