module sub4(
  input logic[3:0] a,
  input logic[3:0] b,
  output logic [3:0] subOut,
  output logic carryOut
  );

  assign subOut = a - b;
  assign carryOut = a >= b ? 1'b1 : 1'b0;

endmodule: sub4
