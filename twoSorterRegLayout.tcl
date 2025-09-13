set project_name "twoSorterRegLayout"
set top_module "twoSorterRegLayout"
set source_file "twoSorterRegLayout.sv"
set part "xc7a200tsbg484-1"
set build_dir $project_name
set reports_dir "$build_dir/reports"
set checkpoints_dir "$build_dir/checkpoints"
set impl_checkpoint "$checkpoints_dir/$top_module\_post_route.dcp"

file mkdir $build_dir
file mkdir $reports_dir
file mkdir $checkpoints_dir

create_project -in_memory -part $part

read_verilog $source_file
add_files -fileset constrs_1 -norecurse "twoSorterReg.xdc"

synth_design -top $top_module -part $part
opt_design
place_design
route_design
phys_opt_design
write_checkpoint -force $impl_checkpoint