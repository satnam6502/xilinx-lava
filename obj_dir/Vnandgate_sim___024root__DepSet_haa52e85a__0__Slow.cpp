// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vnandgate_sim.h for the primary calling header

#include "Vnandgate_sim__pch.h"
#include "Vnandgate_sim___024root.h"

VL_ATTR_COLD void Vnandgate_sim___024root___eval_static__TOP(Vnandgate_sim___024root* vlSelf);

VL_ATTR_COLD void Vnandgate_sim___024root___eval_static(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_static\n"); );
    // Body
    Vnandgate_sim___024root___eval_static__TOP(vlSelf);
    vlSelf->__Vm_traceActivity[1U] = 1U;
    vlSelf->__Vm_traceActivity[0U] = 1U;
}

VL_ATTR_COLD void Vnandgate_sim___024root___eval_static__TOP(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_static__TOP\n"); );
    // Body
    vlSelf->nandgate_sim__DOT__clk = 0U;
    vlSelf->nandgate_sim__DOT__cycle = 0U;
    vlSelf->nandgate_sim__DOT__a_vectors[0U] = 0U;
    vlSelf->nandgate_sim__DOT__a_vectors[1U] = 1U;
    vlSelf->nandgate_sim__DOT__a_vectors[2U] = 0U;
    vlSelf->nandgate_sim__DOT__a_vectors[3U] = 1U;
    vlSelf->nandgate_sim__DOT__b_vectors[0U] = 0U;
    vlSelf->nandgate_sim__DOT__b_vectors[1U] = 0U;
    vlSelf->nandgate_sim__DOT__b_vectors[2U] = 1U;
    vlSelf->nandgate_sim__DOT__b_vectors[3U] = 1U;
}

VL_ATTR_COLD void Vnandgate_sim___024root___eval_final(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_final\n"); );
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vnandgate_sim___024root___dump_triggers__stl(Vnandgate_sim___024root* vlSelf);
#endif  // VL_DEBUG
VL_ATTR_COLD bool Vnandgate_sim___024root___eval_phase__stl(Vnandgate_sim___024root* vlSelf);

VL_ATTR_COLD void Vnandgate_sim___024root___eval_settle(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_settle\n"); );
    // Init
    IData/*31:0*/ __VstlIterCount;
    CData/*0:0*/ __VstlContinue;
    // Body
    __VstlIterCount = 0U;
    vlSelf->__VstlFirstIteration = 1U;
    __VstlContinue = 1U;
    while (__VstlContinue) {
        if (VL_UNLIKELY((0x64U < __VstlIterCount))) {
#ifdef VL_DEBUG
            Vnandgate_sim___024root___dump_triggers__stl(vlSelf);
#endif
            VL_FATAL_MT("nandgate_sim.sv", 1, "", "Settle region did not converge.");
        }
        __VstlIterCount = ((IData)(1U) + __VstlIterCount);
        __VstlContinue = 0U;
        if (Vnandgate_sim___024root___eval_phase__stl(vlSelf)) {
            __VstlContinue = 1U;
        }
        vlSelf->__VstlFirstIteration = 0U;
    }
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vnandgate_sim___024root___dump_triggers__stl(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___dump_triggers__stl\n"); );
    // Body
    if ((1U & (~ vlSelf->__VstlTriggered.any()))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if ((1ULL & vlSelf->__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 0 is active: Internal 'stl' trigger - first iteration\n");
    }
}
#endif  // VL_DEBUG

VL_ATTR_COLD void Vnandgate_sim___024root___stl_sequent__TOP__0(Vnandgate_sim___024root* vlSelf);

VL_ATTR_COLD void Vnandgate_sim___024root___eval_stl(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_stl\n"); );
    // Body
    if ((1ULL & vlSelf->__VstlTriggered.word(0U))) {
        Vnandgate_sim___024root___stl_sequent__TOP__0(vlSelf);
    }
}

VL_ATTR_COLD void Vnandgate_sim___024root___stl_sequent__TOP__0(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___stl_sequent__TOP__0\n"); );
    // Body
    vlSelf->c = (1U & (~ (vlSelf->nandgate_sim__DOT__a_vectors
                          [(3U & vlSelf->nandgate_sim__DOT__cycle)] 
                          & vlSelf->nandgate_sim__DOT__b_vectors
                          [(3U & vlSelf->nandgate_sim__DOT__cycle)])));
}

VL_ATTR_COLD void Vnandgate_sim___024root___eval_triggers__stl(Vnandgate_sim___024root* vlSelf);

VL_ATTR_COLD bool Vnandgate_sim___024root___eval_phase__stl(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_phase__stl\n"); );
    // Init
    CData/*0:0*/ __VstlExecute;
    // Body
    Vnandgate_sim___024root___eval_triggers__stl(vlSelf);
    __VstlExecute = vlSelf->__VstlTriggered.any();
    if (__VstlExecute) {
        Vnandgate_sim___024root___eval_stl(vlSelf);
    }
    return (__VstlExecute);
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vnandgate_sim___024root___dump_triggers__act(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___dump_triggers__act\n"); );
    // Body
    if ((1U & (~ vlSelf->__VactTriggered.any()))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if ((1ULL & vlSelf->__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 0 is active: @(posedge nandgate_sim.clk)\n");
    }
    if ((2ULL & vlSelf->__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 1 is active: @([true] __VdlySched.awaitingCurrentTime())\n");
    }
}
#endif  // VL_DEBUG

#ifdef VL_DEBUG
VL_ATTR_COLD void Vnandgate_sim___024root___dump_triggers__nba(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___dump_triggers__nba\n"); );
    // Body
    if ((1U & (~ vlSelf->__VnbaTriggered.any()))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if ((1ULL & vlSelf->__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 0 is active: @(posedge nandgate_sim.clk)\n");
    }
    if ((2ULL & vlSelf->__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 1 is active: @([true] __VdlySched.awaitingCurrentTime())\n");
    }
}
#endif  // VL_DEBUG

VL_ATTR_COLD void Vnandgate_sim___024root___ctor_var_reset(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___ctor_var_reset\n"); );
    // Body
    vlSelf->c = VL_RAND_RESET_I(1);
    vlSelf->nandgate_sim__DOT__clk = VL_RAND_RESET_I(1);
    vlSelf->nandgate_sim__DOT__cycle = VL_RAND_RESET_I(32);
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->nandgate_sim__DOT__a_vectors[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->nandgate_sim__DOT__b_vectors[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__Vdlyvval__nandgate_sim__DOT__clk__v0 = VL_RAND_RESET_I(1);
    vlSelf->__Vdlyvset__nandgate_sim__DOT__clk__v0 = 0;
    vlSelf->__Vtrigprevexpr___TOP__nandgate_sim__DOT__clk__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vm_traceActivity[__Vi0] = 0;
    }
}
