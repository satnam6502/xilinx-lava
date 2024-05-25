module nandgate_sim(
  output logic c // ri lint_check_waive MIN_NAME_LEN
  );

  logic clk = 0;
  always #10 clk <= ~clk;
  integer cycle = 0;

  always @(posedge clk) begin: cycle_counter
    if (cycle == 3) $finish(1);
    else cycle <= cycle + 1;
  end: cycle_counter;

  nandgate nandgate_dut (.*);

  logic a_vectors[4] = '{1'b0,1'b1,1'b0,1'b1}; // ri lint_check_waive INIT_ASSIGN
  logic a;
  assign a = a_vectors[cycle];
  logic b_vectors[4] = '{1'b0,1'b0,1'b1,1'b1}; // ri lint_check_waive INIT_ASSIGN
  logic b;
  assign b = b_vectors[cycle];

  initial begin
    $dumpfile("nandgate_sim.vcd");
    $dumpvars;
  end

endmodule: nandgate_sim
