@symbolic_wrap struct Num <: Number
    val
end

unwrap(x::Num) = x.val

"""
    Num(val)

Wrap anything in a type that is a subtype of Number
"""
Num

const show_numwrap = Ref(false)

Num(x::Num) = x # ideally this should never be called
(n::Num)(args...) = Num(value(n)(map(value,args)...))
value(x) = unwrap(x)

SymbolicUtils.@number_methods(
                              Num,
                              Num(f(value(a))),
                              Num(f(value(a), value(b))),
                              [conj, real, transpose]
                             )
Base.conj(x::Num) = Num(conj(value(x)))
Base.transpose(x::Num) = x

Base.float(x::Num) = x

IfElse.ifelse(x::Num,y,z) = Num(IfElse.ifelse(value(x), value(y), value(z)))


Base.:^(::Irrational{:ℯ}, x::Num) = exp(x)


# TODO: move this to SymbolicUtils
substitute(expr, s::Pair; kw...) = substituter([s[1] => s[2]])(expr; kw...)
substitute(expr, s::Vector; kw...) = substituter(s)(expr; kw...)

substituter(pair::Pair) = substituter((pair,))
function substituter(pairs)
    dict = Dict(value(k) => value(v)  for (k, v) in pairs)
    (expr; kw...) -> SymbolicUtils.substitute(value(expr), dict; kw...)
end

SymbolicUtils.symtype(n::Num) = symtype(value(n))
Base.nameof(n::Num) = nameof(value(n))

Base.iszero(x::Num) = SymbolicUtils.fraction_iszero(unwrap(x))
Base.isone(x::Num) = SymbolicUtils.fraction_isone(unwrap(x))

Base.show(io::IO, n::Num) = show_numwrap[] ? print(io, :(Num($(value(n))))) : Base.show(io, value(n))

import SymbolicUtils: <ₑ, Symbolic, Term, istree, operation, arguments, similarterm

Base.promote_rule(::Type{<:Number}, ::Type{<:Num}) = Num
Base.promote_rule(::Type{<:Symbolic{<:Number}}, ::Type{<:Num}) = Num

istree(x::Num) = istree(value(x))
operation(x::Num) = operation(value(x))
arguments(x::Num) = arguments(value(x))

<ₑ(s::Num, x) = value(s) <ₑ value(x)
<ₑ(s, x::Num) = value(s) <ₑ value(x)
<ₑ(s::Num, x::Num) = value(s) <ₑ value(x)

Num(q::AbstractIrrational) = Num(Term(identity, [q]))

for T in (Integer, Rational)
    @eval Base.:(^)(n::Num, i::$T) = Num(value(n)^i)
end

macro num_method(f, expr, Ts=nothing)
    if Ts === nothing
        Ts = [Any]
    else
        @assert Ts.head == :tuple
        # e.g. a tuple or vector
        Ts = Ts.args
    end

    ms = [quote
              $f(a::$T, b::$Num) = $expr
              $f(a::$Num, b::$T) = $expr
          end for T in Ts]
    quote
        $f(a::$Num, b::$Num) = $expr
        $(ms...)
    end |> esc
end

# Boolean operations
for (f, Domain) in [:(==) => :((AbstractFloat, Number)), :(!=) => :((AbstractFloat, Number)),
                    :(<=) => :((Real,)),   :(>=) => :((Real,)),
                    :(isless) => :((Real, AbstractFloat)),
                    :(<) => :((Real,)),   :(> ) => :((Real,)),
                    :(& )=> :((Bool,)),  :(| ) => :((Bool,)),
                    :xor => :((Bool,))]
    @eval @num_method Base.$f (val = $f(value(a), value(b)); val isa Bool ? val : Num(val)) $Domain
end

for f in [:!, :~]
    @eval Base.$f(x::Num) = (val = $f(value(x)); val isa Bool ? val : Num(val))
end
@num_method Base.isequal begin
  va = value(a)
  vb = value(b)
  if va isa SymbolicUtils.BasicSymbolic{Real} && vb isa SymbolicUtils.BasicSymbolic{Real}
    isequal(va, vb)::Bool
  else
    isequal(va, vb)::Bool
  end
end (AbstractFloat, Number, Symbolic)

Base.to_index(x::Num) = Base.to_index(value(x))

Base.hash(x::Num, h::UInt) = hash(value(x), h)::UInt

Base.convert(::Type{Num}, x::Symbolic{<:Number}) = Num(x)
Base.convert(::Type{Num}, x::Number) = Num(x)
Base.convert(::Type{Num}, x::Num) = x

Base.convert(::Type{<:Array{Num}}, x::AbstractArray) = map(Num, x)
Base.convert(::Type{<:Array{Num}}, x::AbstractArray{Num}) = x
Base.convert(::Type{Sym}, x::Num) = value(x) isa Sym ? value(x) : error("cannot convert $x to Sym")

LinearAlgebra.lu(x::Union{Adjoint{<:Num},Transpose{<:Num},Array{<:Num}}; check=true, kw...) = sym_lu(x; check=check)

_iszero(x::Number) = iszero(x)
_isone(x::Number) = isone(x)
_iszero(::Symbolic) = false
_isone(::Symbolic) = false
_iszero(x::Num) = _iszero(value(x))::Bool
_isone(x::Num) = _isone(value(x))::Bool

Code.cse(x::Num) = Code.cse(unwrap(x))

similarterm(t::Num, f, args; metadata=nothing) =
    Num(similarterm(unwrap(t), f, args, SymbolicUtils._promote_symtype(f, args); metadata=metadata))
## Documentation
# This method makes the docstring show all entries in the metadata dict associated with an instance of Num
function Base.Docs.getdoc(x::Num)
    x = unwrap(x)
    strings =
        ["A variable of type Symbolics.Num (Num wraps anything in a type that is a subtype of Number)";
        "# Metadata"]
    for (key, val) in collect(pairs(x.metadata))
        push!(strings, string(string(key), ": ", string(val)))
    end
    Markdown.parse(join(strings, "\n\n  "))
end
