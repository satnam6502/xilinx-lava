// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vnandgate_sim.h for the primary calling header

#include "Vnandgate_sim__pch.h"
#include "Vnandgate_sim__Syms.h"
#include "Vnandgate_sim___024root.h"

#ifdef VL_DEBUG
VL_ATTR_COLD void Vnandgate_sim___024root___dump_triggers__act(Vnandgate_sim___024root* vlSelf);
#endif  // VL_DEBUG

void Vnandgate_sim___024root___eval_triggers__act(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_triggers__act\n"); );
    // Body
    vlSelf->__VactTriggered.set(0U, ((IData)(vlSelf->nandgate_sim__DOT__clk) 
                                     & (~ (IData)(vlSelf->__Vtrigprevexpr___TOP__nandgate_sim__DOT__clk__0))));
    vlSelf->__VactTriggered.set(1U, vlSelf->__VdlySched.awaitingCurrentTime());
    vlSelf->__Vtrigprevexpr___TOP__nandgate_sim__DOT__clk__0 
        = vlSelf->nandgate_sim__DOT__clk;
#ifdef VL_DEBUG
    if (VL_UNLIKELY(vlSymsp->_vm_contextp__->debug())) {
        Vnandgate_sim___024root___dump_triggers__act(vlSelf);
    }
#endif
}
