// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "Vnandgate_sim__Syms.h"


VL_ATTR_COLD void Vnandgate_sim___024root__trace_init_sub__TOP__0(Vnandgate_sim___024root* vlSelf, VerilatedVcd* tracep) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_init_sub__TOP__0\n"); );
    // Init
    const int c = vlSymsp->__Vm_baseCode;
    // Body
    tracep->declBit(c+13,0,"c",-1, VerilatedTraceSigDirection::OUTPUT, VerilatedTraceSigKind::WIRE, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->pushPrefix("nandgate_sim", VerilatedTracePrefixType::SCOPE_MODULE);
    tracep->declBit(c+13,0,"c",-1, VerilatedTraceSigDirection::OUTPUT, VerilatedTraceSigKind::WIRE, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->declBit(c+14,0,"clk",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->declBus(c+9,0,"cycle",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::INTEGER, false,-1, 31,0);
    tracep->pushPrefix("a_vectors", VerilatedTracePrefixType::ARRAY_UNPACKED);
    for (int i = 0; i < 4; ++i) {
        tracep->declBit(c+1+i*1,0,"",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::LOGIC, true,(i+0));
    }
    tracep->popPrefix();
    tracep->declBit(c+10,0,"a",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->pushPrefix("b_vectors", VerilatedTracePrefixType::ARRAY_UNPACKED);
    for (int i = 0; i < 4; ++i) {
        tracep->declBit(c+5+i*1,0,"",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::LOGIC, true,(i+0));
    }
    tracep->popPrefix();
    tracep->declBit(c+11,0,"b",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->pushPrefix("nandgate_dut", VerilatedTracePrefixType::SCOPE_MODULE);
    tracep->declBit(c+10,0,"a",-1, VerilatedTraceSigDirection::INPUT, VerilatedTraceSigKind::WIRE, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->declBit(c+11,0,"b",-1, VerilatedTraceSigDirection::INPUT, VerilatedTraceSigKind::WIRE, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->declBit(c+13,0,"c",-1, VerilatedTraceSigDirection::OUTPUT, VerilatedTraceSigKind::WIRE, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->declBit(c+13,0,"net1",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->declBit(c+12,0,"net0",-1, VerilatedTraceSigDirection::NONE, VerilatedTraceSigKind::VAR, VerilatedTraceSigType::LOGIC, false,-1);
    tracep->popPrefix();
    tracep->popPrefix();
}

VL_ATTR_COLD void Vnandgate_sim___024root__trace_init_top(Vnandgate_sim___024root* vlSelf, VerilatedVcd* tracep) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_init_top\n"); );
    // Body
    Vnandgate_sim___024root__trace_init_sub__TOP__0(vlSelf, tracep);
}

VL_ATTR_COLD void Vnandgate_sim___024root__trace_const_0(void* voidSelf, VerilatedVcd::Buffer* bufp);
VL_ATTR_COLD void Vnandgate_sim___024root__trace_full_0(void* voidSelf, VerilatedVcd::Buffer* bufp);
void Vnandgate_sim___024root__trace_chg_0(void* voidSelf, VerilatedVcd::Buffer* bufp);
void Vnandgate_sim___024root__trace_cleanup(void* voidSelf, VerilatedVcd* /*unused*/);

VL_ATTR_COLD void Vnandgate_sim___024root__trace_register(Vnandgate_sim___024root* vlSelf, VerilatedVcd* tracep) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_register\n"); );
    // Body
    tracep->addConstCb(&Vnandgate_sim___024root__trace_const_0, 0U, vlSelf);
    tracep->addFullCb(&Vnandgate_sim___024root__trace_full_0, 0U, vlSelf);
    tracep->addChgCb(&Vnandgate_sim___024root__trace_chg_0, 0U, vlSelf);
    tracep->addCleanupCb(&Vnandgate_sim___024root__trace_cleanup, vlSelf);
}

VL_ATTR_COLD void Vnandgate_sim___024root__trace_const_0(void* voidSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_const_0\n"); );
    // Init
    Vnandgate_sim___024root* const __restrict vlSelf VL_ATTR_UNUSED = static_cast<Vnandgate_sim___024root*>(voidSelf);
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
}

VL_ATTR_COLD void Vnandgate_sim___024root__trace_full_0_sub_0(Vnandgate_sim___024root* vlSelf, VerilatedVcd::Buffer* bufp);

VL_ATTR_COLD void Vnandgate_sim___024root__trace_full_0(void* voidSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_full_0\n"); );
    // Init
    Vnandgate_sim___024root* const __restrict vlSelf VL_ATTR_UNUSED = static_cast<Vnandgate_sim___024root*>(voidSelf);
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    // Body
    Vnandgate_sim___024root__trace_full_0_sub_0((&vlSymsp->TOP), bufp);
}

VL_ATTR_COLD void Vnandgate_sim___024root__trace_full_0_sub_0(Vnandgate_sim___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_full_0_sub_0\n"); );
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode);
    // Body
    bufp->fullBit(oldp+1,(vlSelf->nandgate_sim__DOT__a_vectors[0]));
    bufp->fullBit(oldp+2,(vlSelf->nandgate_sim__DOT__a_vectors[1]));
    bufp->fullBit(oldp+3,(vlSelf->nandgate_sim__DOT__a_vectors[2]));
    bufp->fullBit(oldp+4,(vlSelf->nandgate_sim__DOT__a_vectors[3]));
    bufp->fullBit(oldp+5,(vlSelf->nandgate_sim__DOT__b_vectors[0]));
    bufp->fullBit(oldp+6,(vlSelf->nandgate_sim__DOT__b_vectors[1]));
    bufp->fullBit(oldp+7,(vlSelf->nandgate_sim__DOT__b_vectors[2]));
    bufp->fullBit(oldp+8,(vlSelf->nandgate_sim__DOT__b_vectors[3]));
    bufp->fullIData(oldp+9,(vlSelf->nandgate_sim__DOT__cycle),32);
    bufp->fullBit(oldp+10,(vlSelf->nandgate_sim__DOT__a_vectors
                           [(3U & vlSelf->nandgate_sim__DOT__cycle)]));
    bufp->fullBit(oldp+11,(vlSelf->nandgate_sim__DOT__b_vectors
                           [(3U & vlSelf->nandgate_sim__DOT__cycle)]));
    bufp->fullBit(oldp+12,((vlSelf->nandgate_sim__DOT__a_vectors
                            [(3U & vlSelf->nandgate_sim__DOT__cycle)] 
                            & vlSelf->nandgate_sim__DOT__b_vectors
                            [(3U & vlSelf->nandgate_sim__DOT__cycle)])));
    bufp->fullBit(oldp+13,(vlSelf->c));
    bufp->fullBit(oldp+14,(vlSelf->nandgate_sim__DOT__clk));
}
