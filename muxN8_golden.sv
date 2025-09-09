module muxN8(
  input logic sel,
  input logic[7:0] a,
  input logic[7:0] b,
  output logic[7:0] c
  );

  assign c = sel ? b : a;

endmodule: muxN8