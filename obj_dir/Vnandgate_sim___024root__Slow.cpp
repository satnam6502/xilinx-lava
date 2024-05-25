// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vnandgate_sim.h for the primary calling header

#include "Vnandgate_sim__pch.h"
#include "Vnandgate_sim__Syms.h"
#include "Vnandgate_sim___024root.h"

void Vnandgate_sim___024root___ctor_var_reset(Vnandgate_sim___024root* vlSelf);

Vnandgate_sim___024root::Vnandgate_sim___024root(Vnandgate_sim__Syms* symsp, const char* v__name)
    : VerilatedModule{v__name}
    , __VdlySched{*symsp->_vm_contextp__}
    , vlSymsp{symsp}
 {
    // Reset structure values
    Vnandgate_sim___024root___ctor_var_reset(this);
}

void Vnandgate_sim___024root::__Vconfigure(bool first) {
    (void)first;  // Prevent unused variable warning
}

Vnandgate_sim___024root::~Vnandgate_sim___024root() {
}
