module Symbolics

using DocStringExtensions

using LinearAlgebra

using Reexport

@reexport using SymbolicUtils

import SymbolicUtils: Term, Add, Mul, Pow, Sym, symtype,
                      FnType, @rule, Rewriters, substitute, similarterm,
                      promote_symtype, istree, operation, arguments

import SymbolicUtils.Rewriters: Chain, Prewalk, Postwalk, Fixpoint

import SymbolicUtils.Code: toexpr

using RuntimeGeneratedFunctions
RuntimeGeneratedFunctions.init(@__MODULE__)

# re-export

export simplify, substitute

using SciMLBase
export Num
include("num.jl")

export Equation, ConstrainedEquation
include("equations.jl")

include("utils.jl")

using MacroTools
import MacroTools: splitdef, combinedef, postwalk, striplines
export @register
include("register.jl")

using TreeViews
export @variables, Variable
include("variable.jl")

include("linearity.jl")

using DiffRules, SpecialFunctions, NaNMath

using SparseArrays

export Differential, expand_derivatives, derivative, gradient,
       jacobian, jacobian_sparsity, sparsejacobian,
       hessian, sparsehessian, hessian_sparsity

include("diff.jl")

include("linear_algebra.jl")

include("build_function.jl")
export build_function

using IfElse, Distributions
include("extra_functions.jl")

using Latexify
include("latexify_recipes.jl")

end # module
