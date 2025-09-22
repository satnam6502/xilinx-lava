set project_name "sorter8"
set top_module "sorter8"
set source_file "sorter8.sv"
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
synth_design -top $top_module -part $part
place_design
route_design
write_checkpoint -force $impl_checkpoint