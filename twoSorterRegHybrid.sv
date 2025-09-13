module twoSorterReg(
  input logic clk,
  input logic rstN,
  input logic[3:0] a,
  input logic[3:0] b,
  output logic[3:0] c,
  output logic[3:0] d
  );
  
  logic [3:0] co1;
  logic [3:0] co2;

always_ff @(posedge clk or negedge rstN) begin
    if (!rstN) begin
      // Asynchronous reset - clear outputs
      c <= 4'b0000;
      d <= 4'b0000;
    end else begin
      // Compare inputs and assign sorted values
      if (co1[3]) begin
        c <= b;  // a is smaller (or equal)
      end else begin
        c <= a;  // b is smaller
      end
      if (co2[3]) begin
        d <= b;  
      end else begin
        d <= a;  
      end
    end
  end


  logic [3:0] ps1;
  logic [3:0] ps2;
  assign ps1 = ~(a ^ b);
  assign ps2 = ~(a ^ b);
  logic [3:0] diffA;
  logic [3:0] diffB;
  CARRY4 carry4_A (.CI(1'b0), .CYINIT(1'b1), .DI(a),.S(ps1),.O(diffA),.CO(co1));
  CARRY4 carry4_B (.CI(1'b0), .CYINIT(1'b1), .DI(b),.S(ps2),.O(diffB),.CO(co2));

endmodule: twoSorterReg
