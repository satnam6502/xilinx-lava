module twoSorterReg(
  input logic clk,
  input logic rstN,
  input logic[3:0] a,
  input logic[3:0] b,
  output logic[3:0] c,
  output logic[3:0] d
  );
  
always_ff @(posedge clk or negedge rstN) begin
    if (!rstN) begin
      // Asynchronous reset - clear outputs
      c <= 4'b0000;
      d <= 4'b0000;
    end else begin
      // Compare inputs and assign sorted values
      if (a <= b) begin
        c <= a;  // a is smaller (or equal)
        d <= b;  // b is larger (or equal)
      end else begin
        c <= b;  // b is smaller
        d <= a;  // a is larger
      end
    end
  end

endmodule: twoSorterReg
