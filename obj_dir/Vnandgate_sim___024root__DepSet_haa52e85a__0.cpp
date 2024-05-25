// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vnandgate_sim.h for the primary calling header

#include "Vnandgate_sim__pch.h"
#include "Vnandgate_sim___024root.h"

VL_ATTR_COLD void Vnandgate_sim___024root___eval_initial__TOP(Vnandgate_sim___024root* vlSelf);
VlCoroutine Vnandgate_sim___024root___eval_initial__TOP__Vtiming__0(Vnandgate_sim___024root* vlSelf);

void Vnandgate_sim___024root___eval_initial(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_initial\n"); );
    // Body
    Vnandgate_sim___024root___eval_initial__TOP(vlSelf);
    Vnandgate_sim___024root___eval_initial__TOP__Vtiming__0(vlSelf);
    vlSelf->__Vtrigprevexpr___TOP__nandgate_sim__DOT__clk__0 
        = vlSelf->nandgate_sim__DOT__clk;
}

VL_INLINE_OPT VlCoroutine Vnandgate_sim___024root___eval_initial__TOP__Vtiming__0(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_initial__TOP__Vtiming__0\n"); );
    // Body
    while (1U) {
        co_await vlSelf->__VdlySched.delay(0xaULL, 
                                           nullptr, 
                                           "nandgate_sim.sv", 
                                           6);
        vlSelf->__Vdlyvval__nandgate_sim__DOT__clk__v0 
            = (1U & (~ (IData)(vlSelf->nandgate_sim__DOT__clk)));
        vlSelf->__Vdlyvset__nandgate_sim__DOT__clk__v0 = 1U;
    }
}

void Vnandgate_sim___024root___eval_act(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_act\n"); );
}

void Vnandgate_sim___024root___nba_sequent__TOP__0(Vnandgate_sim___024root* vlSelf);
void Vnandgate_sim___024root___nba_sequent__TOP__1(Vnandgate_sim___024root* vlSelf);

void Vnandgate_sim___024root___eval_nba(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_nba\n"); );
    // Body
    if ((1ULL & vlSelf->__VnbaTriggered.word(0U))) {
        Vnandgate_sim___024root___nba_sequent__TOP__0(vlSelf);
        vlSelf->__Vm_traceActivity[1U] = 1U;
    }
    if ((2ULL & vlSelf->__VnbaTriggered.word(0U))) {
        Vnandgate_sim___024root___nba_sequent__TOP__1(vlSelf);
    }
}

VL_INLINE_OPT void Vnandgate_sim___024root___nba_sequent__TOP__0(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___nba_sequent__TOP__0\n"); );
    // Body
    if (VL_UNLIKELY((3U == vlSelf->nandgate_sim__DOT__cycle))) {
        VL_FINISH_MT("nandgate_sim.sv", 10, "");
    } else {
        vlSelf->nandgate_sim__DOT__cycle = ((IData)(1U) 
                                            + vlSelf->nandgate_sim__DOT__cycle);
    }
    vlSelf->c = (1U & (~ (vlSelf->nandgate_sim__DOT__a_vectors
                          [(3U & vlSelf->nandgate_sim__DOT__cycle)] 
                          & vlSelf->nandgate_sim__DOT__b_vectors
                          [(3U & vlSelf->nandgate_sim__DOT__cycle)])));
}

VL_INLINE_OPT void Vnandgate_sim___024root___nba_sequent__TOP__1(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___nba_sequent__TOP__1\n"); );
    // Body
    if (vlSelf->__Vdlyvset__nandgate_sim__DOT__clk__v0) {
        vlSelf->nandgate_sim__DOT__clk = vlSelf->__Vdlyvval__nandgate_sim__DOT__clk__v0;
        vlSelf->__Vdlyvset__nandgate_sim__DOT__clk__v0 = 0U;
    }
}

void Vnandgate_sim___024root___timing_resume(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___timing_resume\n"); );
    // Body
    if ((2ULL & vlSelf->__VactTriggered.word(0U))) {
        vlSelf->__VdlySched.resume();
    }
}

void Vnandgate_sim___024root___eval_triggers__act(Vnandgate_sim___024root* vlSelf);

bool Vnandgate_sim___024root___eval_phase__act(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_phase__act\n"); );
    // Init
    VlTriggerVec<2> __VpreTriggered;
    CData/*0:0*/ __VactExecute;
    // Body
    Vnandgate_sim___024root___eval_triggers__act(vlSelf);
    __VactExecute = vlSelf->__VactTriggered.any();
    if (__VactExecute) {
        __VpreTriggered.andNot(vlSelf->__VactTriggered, vlSelf->__VnbaTriggered);
        vlSelf->__VnbaTriggered.thisOr(vlSelf->__VactTriggered);
        Vnandgate_sim___024root___timing_resume(vlSelf);
        Vnandgate_sim___024root___eval_act(vlSelf);
    }
    return (__VactExecute);
}

bool Vnandgate_sim___024root___eval_phase__nba(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_phase__nba\n"); );
    // Init
    CData/*0:0*/ __VnbaExecute;
    // Body
    __VnbaExecute = vlSelf->__VnbaTriggered.any();
    if (__VnbaExecute) {
        Vnandgate_sim___024root___eval_nba(vlSelf);
        vlSelf->__VnbaTriggered.clear();
    }
    return (__VnbaExecute);
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vnandgate_sim___024root___dump_triggers__nba(Vnandgate_sim___024root* vlSelf);
#endif  // VL_DEBUG
#ifdef VL_DEBUG
VL_ATTR_COLD void Vnandgate_sim___024root___dump_triggers__act(Vnandgate_sim___024root* vlSelf);
#endif  // VL_DEBUG

void Vnandgate_sim___024root___eval(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval\n"); );
    // Init
    IData/*31:0*/ __VnbaIterCount;
    CData/*0:0*/ __VnbaContinue;
    // Body
    __VnbaIterCount = 0U;
    __VnbaContinue = 1U;
    while (__VnbaContinue) {
        if (VL_UNLIKELY((0x64U < __VnbaIterCount))) {
#ifdef VL_DEBUG
            Vnandgate_sim___024root___dump_triggers__nba(vlSelf);
#endif
            VL_FATAL_MT("nandgate_sim.sv", 1, "", "NBA region did not converge.");
        }
        __VnbaIterCount = ((IData)(1U) + __VnbaIterCount);
        __VnbaContinue = 0U;
        vlSelf->__VactIterCount = 0U;
        vlSelf->__VactContinue = 1U;
        while (vlSelf->__VactContinue) {
            if (VL_UNLIKELY((0x64U < vlSelf->__VactIterCount))) {
#ifdef VL_DEBUG
                Vnandgate_sim___024root___dump_triggers__act(vlSelf);
#endif
                VL_FATAL_MT("nandgate_sim.sv", 1, "", "Active region did not converge.");
            }
            vlSelf->__VactIterCount = ((IData)(1U) 
                                       + vlSelf->__VactIterCount);
            vlSelf->__VactContinue = 0U;
            if (Vnandgate_sim___024root___eval_phase__act(vlSelf)) {
                vlSelf->__VactContinue = 1U;
            }
        }
        if (Vnandgate_sim___024root___eval_phase__nba(vlSelf)) {
            __VnbaContinue = 1U;
        }
    }
}

#ifdef VL_DEBUG
void Vnandgate_sim___024root___eval_debug_assertions(Vnandgate_sim___024root* vlSelf) {
    (void)vlSelf;  // Prevent unused variable warning
    Vnandgate_sim__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vnandgate_sim___024root___eval_debug_assertions\n"); );
}
#endif  // VL_DEBUG
