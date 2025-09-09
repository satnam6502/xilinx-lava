module adder4(
  input logic[3:0] a,
  input logic[3:0] b,
  output logic[3:0] sumOut
  );

  assign sumOut = a + b;

endmodule: adder4
