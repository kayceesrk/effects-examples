(*
    A shallowly embedded DSL for Probabilistic Programming for effect handlers
    
    Uses the Hamiltonian Monte Carlo for inference.    
*)

module Hmc = Hmc
module Primitive = Primitive
module Printers = Printers
module Helpers = Helpers

module Infer : Hmc.S = Hmc.Infer
module Print : Printers.S' = Printers.Print
module Stats : Helpers.S = Helpers.Basics

