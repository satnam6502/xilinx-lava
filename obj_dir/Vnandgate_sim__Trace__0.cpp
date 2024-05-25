// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "Vnandgate_sim__Syms.h"


void Vnandgate_sim___024root__trace_chg_0_sub_0(Vnandgate_sim___024root* vlSelf, VerilatedVcd::Buffer* bufp);

void Vnandgate_sim___024root__trace_chg_0(void* voidSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_chg_0\n"); );
    // Init
    Vnandgate_sim___024root* const __restrict vlSelf VL_ATTR_UNUSED = static_cast<Vnandgate_sim___024root*>(voidSelf);
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    if (VL_UNLIKELY(!vlSymsp->__Vm_activity)) return;
    // Body
    Vnandgate_sim___024root__trace_chg_0_sub_0((&vlSymsp->TOP), bufp);
}

void Vnandgate_sim___024root__trace_chg_0_sub_0(Vnandgate_sim___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_chg_0_sub_0\n"); );
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode + 1);
    // Body
    if (VL_UNLIKELY(vlSelf->__Vm_traceActivity[0U])) {
        bufp->chgBit(oldp+0,(vlSelf->nandgate_sim__DOT__a_vectors[0]));
        bufp->chgBit(oldp+1,(vlSelf->nandgate_sim__DOT__a_vectors[1]));
        bufp->chgBit(oldp+2,(vlSelf->nandgate_sim__DOT__a_vectors[2]));
        bufp->chgBit(oldp+3,(vlSelf->nandgate_sim__DOT__a_vectors[3]));
        bufp->chgBit(oldp+4,(vlSelf->nandgate_sim__DOT__b_vectors[0]));
        bufp->chgBit(oldp+5,(vlSelf->nandgate_sim__DOT__b_vectors[1]));
        bufp->chgBit(oldp+6,(vlSelf->nandgate_sim__DOT__b_vectors[2]));
        bufp->chgBit(oldp+7,(vlSelf->nandgate_sim__DOT__b_vectors[3]));
    }
    if (VL_UNLIKELY(vlSelf->__Vm_traceActivity[1U])) {
        bufp->chgIData(oldp+8,(vlSelf->nandgate_sim__DOT__cycle),32);
        bufp->chgBit(oldp+9,(vlSelf->nandgate_sim__DOT__a_vectors
                             [(3U & vlSelf->nandgate_sim__DOT__cycle)]));
        bufp->chgBit(oldp+10,(vlSelf->nandgate_sim__DOT__b_vectors
                              [(3U & vlSelf->nandgate_sim__DOT__cycle)]));
        bufp->chgBit(oldp+11,((vlSelf->nandgate_sim__DOT__a_vectors
                               [(3U & vlSelf->nandgate_sim__DOT__cycle)] 
                               & vlSelf->nandgate_sim__DOT__b_vectors
                               [(3U & vlSelf->nandgate_sim__DOT__cycle)])));
    }
    bufp->chgBit(oldp+12,(vlSelf->c));
    bufp->chgBit(oldp+13,(vlSelf->nandgate_sim__DOT__clk));
}

void Vnandgate_sim___024root__trace_cleanup(void* voidSelf, VerilatedVcd* /*unused*/) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root__trace_cleanup\n"); );
    // Init
    Vnandgate_sim___024root* const __restrict vlSelf VL_ATTR_UNUSED = static_cast<Vnandgate_sim___024root*>(voidSelf);
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    // Body
    vlSymsp->__Vm_activity = false;
    vlSymsp->TOP.__Vm_traceActivity[0U] = 0U;
    vlSymsp->TOP.__Vm_traceActivity[1U] = 0U;
}
