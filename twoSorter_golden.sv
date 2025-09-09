module twoSorter(
  input logic[3:0] a,
  input logic[3:0] b,
  output logic[3:0] c,
  output logic[3:0] d
  );

  assign c = b >= a ? a : b;
  assign d = b >= a ? b : a;

endmodule: twoSorter