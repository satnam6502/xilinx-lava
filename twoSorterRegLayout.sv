module twoSorterRegLayout(
  input logic clk,
  input logic rstN,
  input logic[3:0] a,
  input logic[3:0] b,
  output logic[3:0] c,
  output logic[3:0] d
  );
  logic net28;
  logic net27;
  logic net26;
  logic net25;
  logic net24;
  logic net23;
  logic net22;
  logic net21;
  logic net20;
  logic net19;
  logic net18;
  logic net17;
  logic[3:0] net16;
  logic[3:0] net15;
  logic net14;
  logic net13;
  logic net12;
  logic net11;
  logic net10;
  logic net9;
  logic net8;
  logic net7;
  logic[3:0] net6;
  logic[3:0] net5;
  logic net4;
  logic net3;
  logic net2;
  logic net1;
  logic net0;
  assign d = {net28,net27,net26,net25};
  assign c = {net24,net23,net22,net21};
  (* RLOC = "X1Y0" *) FDCE  fdce_26 (.D(net20), .C(clk), .CE(1'b1), .CLR(net0), .Q(net28));
  (* RLOC = "X1Y0" *) FDCE  fdce_25 (.D(net19), .C(clk), .CE(1'b1), .CLR(net0), .Q(net27));
  (* RLOC = "X1Y0" *) FDCE  fdce_24 (.D(net18), .C(clk), .CE(1'b1), .CLR(net0), .Q(net26));
  (* RLOC = "X1Y0" *) FDCE  fdce_23 (.D(net17), .C(clk), .CE(1'b1), .CLR(net0), .Q(net25));
  (* RLOC = "X1Y0" *) FDCE  fdce_22 (.D(net10), .C(clk), .CE(1'b1), .CLR(net0), .Q(net24));
  (* RLOC = "X1Y0" *) FDCE  fdce_21 (.D(net9), .C(clk), .CE(1'b1), .CLR(net0), .Q(net23));
  (* RLOC = "X1Y0" *) FDCE  fdce_20 (.D(net8), .C(clk), .CE(1'b1), .CLR(net0), .Q(net22));
  (* RLOC = "X1Y0" *) FDCE  fdce_19 (.D(net7), .C(clk), .CE(1'b1), .CLR(net0), .Q(net21));
  (* RLOC = "X1Y1" *) LUT3 #(.INIT(8'he4)) lut3_18(.I0(net16[3]), .I1(a[3]), .I2(b[3]), .O(net20));
  (* RLOC = "X1Y1" *) LUT3 #(.INIT(8'he4)) lut3_17(.I0(net16[3]), .I1(a[2]), .I2(b[2]), .O(net19));
  (* RLOC = "X1Y1" *) LUT3 #(.INIT(8'he4)) lut3_16(.I0(net16[3]), .I1(a[1]), .I2(b[1]), .O(net18));
  (* RLOC = "X1Y1" *) LUT3 #(.INIT(8'he4)) lut3_15(.I0(net16[3]), .I1(a[0]), .I2(b[0]), .O(net17));
  (* RLOC = "X0Y1" *) CARRY4 carry4_14 (.CI(1'b0), .CYINIT(1'b1), .DI({b[3],b[2],b[1],b[0]}),.S({net14,net13,net12,net11}),.O({net15[3],net15[2],net15[1],net15[0]}),.CO({net16[3],net16[2],net16[1],net16[0]}));
  LUT2 #(.INIT(4'h9)) lut2_13(.I0(b[3]), .I1(a[3]), .O(net14));
  LUT2 #(.INIT(4'h9)) lut2_12(.I0(b[2]), .I1(a[2]), .O(net13));
  LUT2 #(.INIT(4'h9)) lut2_11(.I0(b[1]), .I1(a[1]), .O(net12));
  LUT2 #(.INIT(4'h9)) lut2_10(.I0(b[0]), .I1(a[0]), .O(net11));
  (* RLOC = "X1Y0" *) LUT3 #(.INIT(8'he4)) lut3_9(.I0(net6[3]), .I1(a[3]), .I2(b[3]), .O(net10));
  (* RLOC = "X1Y0" *) LUT3 #(.INIT(8'he4)) lut3_8(.I0(net6[3]), .I1(a[2]), .I2(b[2]), .O(net9));
  (* RLOC = "X1Y0" *) LUT3 #(.INIT(8'he4)) lut3_7(.I0(net6[3]), .I1(a[1]), .I2(b[1]), .O(net8));
  (* RLOC = "X1Y0" *) LUT3 #(.INIT(8'he4)) lut3_6(.I0(net6[3]), .I1(a[0]), .I2(b[0]), .O(net7));
  (* RLOC = "X0Y0",  RLOC_ORIGIN = "X0Y0" *) CARRY4 carry4_5 (.CI(1'b0), .CYINIT(1'b1), .DI({a[3],a[2],a[1],a[0]}),.S({net4,net3,net2,net1}),.O({net5[3],net5[2],net5[1],net5[0]}),.CO({net6[3],net6[2],net6[1],net6[0]}));
  LUT2 #(.INIT(4'h9)) lut2_4(.I0(a[3]), .I1(b[3]), .O(net4));
  LUT2 #(.INIT(4'h9)) lut2_3(.I0(a[2]), .I1(b[2]), .O(net3));
  LUT2 #(.INIT(4'h9)) lut2_2(.I0(a[1]), .I1(b[1]), .O(net2));
  LUT2 #(.INIT(4'h9)) lut2_1(.I0(a[0]), .I1(b[0]), .O(net1));
  not not_0 (net0, rstN);
endmodule: twoSorterRegLayout
