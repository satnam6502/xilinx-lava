module sub4(
  input logic[3:0] a,
  input logic[3:0] b,
  output logic[3:0] subOut,
  output logic carryOut
  );

  assign {carryOut, subOut} = a - b;

endmodule: sub4
