///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1995/2016 Xilinx, Inc.
// All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /    Vendor : Xilinx
// \   \   \/     Version : 2017.1 (Verilator Compatible)
//  \   \         Description : Xilinx Unified Simulation Library Component
//  /   /                  D Flip-Flop with Clock Enable and Asynchronous Clear
// /___/   /\     Filename : FDCE_verilator.v
// \   \  /  \
//  \___\/\___\
//
// Revision:
//    Modified for Verilator compatibility - removed tri-state signals,
//    global reset dependency, timing constructs, and procedural assigns
// End Revision

`timescale  1 ps / 1 ps

module FDCE #(
  parameter [0:0] INIT = 1'b0,
  parameter [0:0] IS_CLR_INVERTED = 1'b0,
  parameter [0:0] IS_C_INVERTED = 1'b0,
  parameter [0:0] IS_D_INVERTED = 1'b0
)(
  output reg Q,
  
  input C,
  input CE,
  input CLR,
  input D
);

    // Internal registers for inverted parameters
    reg [0:0] IS_CLR_INVERTED_REG = IS_CLR_INVERTED;
    reg [0:0] IS_C_INVERTED_REG = IS_C_INVERTED;
    reg [0:0] IS_D_INVERTED_REG = IS_D_INVERTED;
    
    // Processed signals
    wire CLR_in;
    wire C_in;
    wire D_in;
    
    // Process inverted signals
    assign CLR_in = (CLR ^ IS_CLR_INVERTED_REG);
    assign C_in = C ^ IS_C_INVERTED_REG;
    assign D_in = D ^ IS_D_INVERTED_REG;

    // Main flip-flop behavior - simplified for synthesis
    generate
      if (IS_C_INVERTED == 1'b0) begin : pos_edge_clk
        always @(posedge C_in or posedge CLR_in) begin
          if (CLR_in)
            Q <= 1'b0;
          else if (CE)
            Q <= D_in;
          // If CE is low, Q holds its value (implicit latch behavior)
        end
      end else begin : neg_edge_clk
        always @(negedge C_in or posedge CLR_in) begin
          if (CLR_in)
            Q <= 1'b0;
          else if (CE)
            Q <= D_in;
          // If CE is low, Q holds its value (implicit latch behavior)
        end
      end
    endgenerate

endmodule