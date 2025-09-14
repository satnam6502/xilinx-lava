// Copyright 1986-2022 Xilinx, Inc. All Rights Reserved.
// Copyright 2022-2025 Advanced Micro Devices, Inc. All Rights Reserved.
// --------------------------------------------------------------------------------
// Tool Version: Vivado v.2025.1 (win64) Build 6140274 Thu May 22 00:12:29 MDT 2025
// Date        : Fri Sep 12 14:27:47 2025
// Host        : ardberg running 64-bit major release  (build 9200)
// Command     : write_verilog E:/home/satnam/xilinx-lava/twoSorterRegHybrid/checkpoints/project_twoSorterReg_post_route.v
// Design      : twoSorterReg
// Purpose     : This is a Verilog netlist of the current design or from a specific cell of the design. The output is an
//               IEEE 1364-2001 compliant Verilog HDL file that contains netlist information obtained from the input
//               design files.
// Device      : xc7a200tsbg484-1
// --------------------------------------------------------------------------------
`timescale 1 ps / 1 ps

(* ECO_CHECKSUM = "e203eed3" *) 
(* STRUCTURAL_NETLIST = "yes" *)
(* \DesignAttr:ENABLE_NOC_NETLIST_VIEW  *) 
(* \DesignAttr:ENABLE_AIE_NETLIST_VIEW  *) 
module twoSorterReg
   (clk,
    rstN,
    a,
    b,
    c,
    d);
  input clk;
  input rstN;
  input [3:0]a;
  input [3:0]b;
  output [3:0]c;
  output [3:0]d;

  wire \<const0> ;
  wire \<const1> ;
  wire [3:0]a;
  wire [3:0]a_IBUF;
  wire [3:0]b;
  wire [3:0]b_IBUF;
  wire [3:0]c;
  wire \c[0]_i_1_n_0 ;
  wire \c[1]_i_1_n_0 ;
  wire \c[2]_i_1_n_0 ;
  wire \c[3]_i_1_n_0 ;
  wire \c[3]_i_2_n_0 ;
  wire [3:0]c_OBUF;
  wire clk;
  wire clk_IBUF;
  wire clk_IBUF_BUFG;
  wire [3:3]co1;
  wire [3:3]co2;
  wire [3:0]d;
  wire \d[0]_i_1_n_0 ;
  wire \d[1]_i_1_n_0 ;
  wire \d[2]_i_1_n_0 ;
  wire \d[3]_i_1_n_0 ;
  wire [3:0]d_OBUF;
  wire [3:0]ps;
  wire rstN;
  wire rstN_IBUF;
  wire [3:0]NLW_carry4_A_CO_UNCONNECTED;
  wire [3:0]NLW_carry4_B_CO_UNCONNECTED;

  GND GND
       (.G(\<const0> ));
  VCC VCC
       (.P(\<const1> ));
  IBUF \a_IBUF[0]_inst 
       (.I(a[0]),
        .O(a_IBUF[0]));
  IBUF \a_IBUF[1]_inst 
       (.I(a[1]),
        .O(a_IBUF[1]));
  IBUF \a_IBUF[2]_inst 
       (.I(a[2]),
        .O(a_IBUF[2]));
  IBUF \a_IBUF[3]_inst 
       (.I(a[3]),
        .O(a_IBUF[3]));
  IBUF \b_IBUF[0]_inst 
       (.I(b[0]),
        .O(b_IBUF[0]));
  IBUF \b_IBUF[1]_inst 
       (.I(b[1]),
        .O(b_IBUF[1]));
  IBUF \b_IBUF[2]_inst 
       (.I(b[2]),
        .O(b_IBUF[2]));
  IBUF \b_IBUF[3]_inst 
       (.I(b[3]),
        .O(b_IBUF[3]));
  (* SOFT_HLUTNM = "soft_lutpair0" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \c[0]_i_1 
       (.I0(b_IBUF[0]),
        .I1(a_IBUF[0]),
        .I2(co1),
        .O(\c[0]_i_1_n_0 ));
  (* SOFT_HLUTNM = "soft_lutpair1" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \c[1]_i_1 
       (.I0(b_IBUF[1]),
        .I1(a_IBUF[1]),
        .I2(co1),
        .O(\c[1]_i_1_n_0 ));
  (* SOFT_HLUTNM = "soft_lutpair2" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \c[2]_i_1 
       (.I0(b_IBUF[2]),
        .I1(a_IBUF[2]),
        .I2(co1),
        .O(\c[2]_i_1_n_0 ));
  (* SOFT_HLUTNM = "soft_lutpair3" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \c[3]_i_1 
       (.I0(b_IBUF[3]),
        .I1(a_IBUF[3]),
        .I2(co1),
        .O(\c[3]_i_1_n_0 ));
  LUT1 #(
    .INIT(2'h1)) 
    \c[3]_i_2 
       (.I0(rstN_IBUF),
        .O(\c[3]_i_2_n_0 ));
  OBUF \c_OBUF[0]_inst 
       (.I(c_OBUF[0]),
        .O(c[0]));
  OBUF \c_OBUF[1]_inst 
       (.I(c_OBUF[1]),
        .O(c[1]));
  OBUF \c_OBUF[2]_inst 
       (.I(c_OBUF[2]),
        .O(c[2]));
  OBUF \c_OBUF[3]_inst 
       (.I(c_OBUF[3]),
        .O(c[3]));
  FDCE #(
    .INIT(1'b0)) 
    \c_reg[0] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\c[0]_i_1_n_0 ),
        .Q(c_OBUF[0]));
  FDCE #(
    .INIT(1'b0)) 
    \c_reg[1] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\c[1]_i_1_n_0 ),
        .Q(c_OBUF[1]));
  FDCE #(
    .INIT(1'b0)) 
    \c_reg[2] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\c[2]_i_1_n_0 ),
        .Q(c_OBUF[2]));
  FDCE #(
    .INIT(1'b0)) 
    \c_reg[3] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\c[3]_i_1_n_0 ),
        .Q(c_OBUF[3]));
  (* BOX_TYPE = "PRIMITIVE" *) 
  CARRY4 carry4_A
       (.CI(\<const0> ),
        .CO({co1,NLW_carry4_A_CO_UNCONNECTED[2:0]}),
        .CYINIT(\<const1> ),
        .DI(a_IBUF),
        .S(ps));
  LUT2 #(
    .INIT(4'h9)) 
    carry4_A_i_1
       (.I0(b_IBUF[3]),
        .I1(a_IBUF[3]),
        .O(ps[3]));
  LUT2 #(
    .INIT(4'h9)) 
    carry4_A_i_2
       (.I0(b_IBUF[2]),
        .I1(a_IBUF[2]),
        .O(ps[2]));
  LUT2 #(
    .INIT(4'h9)) 
    carry4_A_i_3
       (.I0(b_IBUF[1]),
        .I1(a_IBUF[1]),
        .O(ps[1]));
  LUT2 #(
    .INIT(4'h9)) 
    carry4_A_i_4
       (.I0(b_IBUF[0]),
        .I1(a_IBUF[0]),
        .O(ps[0]));
  (* BOX_TYPE = "PRIMITIVE" *) 
  CARRY4 carry4_B
       (.CI(\<const0> ),
        .CO({co2,NLW_carry4_B_CO_UNCONNECTED[2:0]}),
        .CYINIT(\<const1> ),
        .DI(b_IBUF),
        .S(ps));
  BUFG clk_IBUF_BUFG_inst
       (.I(clk_IBUF),
        .O(clk_IBUF_BUFG));
  IBUF clk_IBUF_inst
       (.I(clk),
        .O(clk_IBUF));
  (* SOFT_HLUTNM = "soft_lutpair0" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \d[0]_i_1 
       (.I0(b_IBUF[0]),
        .I1(a_IBUF[0]),
        .I2(co2),
        .O(\d[0]_i_1_n_0 ));
  (* SOFT_HLUTNM = "soft_lutpair1" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \d[1]_i_1 
       (.I0(b_IBUF[1]),
        .I1(a_IBUF[1]),
        .I2(co2),
        .O(\d[1]_i_1_n_0 ));
  (* SOFT_HLUTNM = "soft_lutpair2" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \d[2]_i_1 
       (.I0(b_IBUF[2]),
        .I1(a_IBUF[2]),
        .I2(co2),
        .O(\d[2]_i_1_n_0 ));
  (* SOFT_HLUTNM = "soft_lutpair3" *) 
  LUT3 #(
    .INIT(8'hAC)) 
    \d[3]_i_1 
       (.I0(b_IBUF[3]),
        .I1(a_IBUF[3]),
        .I2(co2),
        .O(\d[3]_i_1_n_0 ));
  OBUF \d_OBUF[0]_inst 
       (.I(d_OBUF[0]),
        .O(d[0]));
  OBUF \d_OBUF[1]_inst 
       (.I(d_OBUF[1]),
        .O(d[1]));
  OBUF \d_OBUF[2]_inst 
       (.I(d_OBUF[2]),
        .O(d[2]));
  OBUF \d_OBUF[3]_inst 
       (.I(d_OBUF[3]),
        .O(d[3]));
  FDCE #(
    .INIT(1'b0)) 
    \d_reg[0] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\d[0]_i_1_n_0 ),
        .Q(d_OBUF[0]));
  FDCE #(
    .INIT(1'b0)) 
    \d_reg[1] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\d[1]_i_1_n_0 ),
        .Q(d_OBUF[1]));
  FDCE #(
    .INIT(1'b0)) 
    \d_reg[2] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\d[2]_i_1_n_0 ),
        .Q(d_OBUF[2]));
  FDCE #(
    .INIT(1'b0)) 
    \d_reg[3] 
       (.C(clk_IBUF_BUFG),
        .CE(\<const1> ),
        .CLR(\c[3]_i_2_n_0 ),
        .D(\d[3]_i_1_n_0 ),
        .Q(d_OBUF[3]));
  IBUF rstN_IBUF_inst
       (.I(rstN),
        .O(rstN_IBUF));
endmodule
