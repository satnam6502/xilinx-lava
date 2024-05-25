// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See Vnandgate_sim.h for the primary calling header

#ifndef VERILATED_VNANDGATE_SIM___024ROOT_H_
#define VERILATED_VNANDGATE_SIM___024ROOT_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class Vnandgate_sim__Syms;

class alignas(VL_CACHE_LINE_BYTES) Vnandgate_sim___024root final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    CData/*0:0*/ nandgate_sim__DOT__clk;
    VL_OUT8(c,0,0);
    CData/*0:0*/ __Vdlyvval__nandgate_sim__DOT__clk__v0;
    CData/*0:0*/ __Vdlyvset__nandgate_sim__DOT__clk__v0;
    CData/*0:0*/ __VstlFirstIteration;
    CData/*0:0*/ __Vtrigprevexpr___TOP__nandgate_sim__DOT__clk__0;
    CData/*0:0*/ __VactContinue;
    IData/*31:0*/ nandgate_sim__DOT__cycle;
    IData/*31:0*/ __VactIterCount;
    VlUnpacked<CData/*0:0*/, 4> nandgate_sim__DOT__a_vectors;
    VlUnpacked<CData/*0:0*/, 4> nandgate_sim__DOT__b_vectors;
    VlUnpacked<CData/*0:0*/, 2> __Vm_traceActivity;
    VlDelayScheduler __VdlySched;
    VlTriggerVec<1> __VstlTriggered;
    VlTriggerVec<2> __VactTriggered;
    VlTriggerVec<2> __VnbaTriggered;

    // INTERNAL VARIABLES
    Vnandgate_sim__Syms* const vlSymsp;

    // CONSTRUCTORS
    Vnandgate_sim___024root(Vnandgate_sim__Syms* symsp, const char* v__name);
    ~Vnandgate_sim___024root();
    VL_UNCOPYABLE(Vnandgate_sim___024root);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
