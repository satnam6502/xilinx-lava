module nandgate(
  input logic a, // ri lint_check_waive MIN_NAME_LEN
  input logic b, // ri lint_check_waive MIN_NAME_LEN
  output logic c // ri lint_check_waive MIN_NAME_LEN
  );
  // BEGIN: local net declarations
  logic net1;
  logic net0;
  // END: local net declarations
  assign c = net1;
  not not_1 (net1, net0);
  and and_0 (net0, a, b);
endmodule: nandgate
