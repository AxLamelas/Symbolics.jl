# Implement a few of the LogExpFunctions methods when those rely on boolean workflows.

LogExpFunctions.log1mexp(x::Num) = log(1 - exp(x))
LogExpFunctions.log1pexp(x::Num) = log(1 + exp(x))
LogExpFunctions.logexpm1(x::Num) = log(exp(x) - 1)
LogExpFunctions.logmxp1(x::Num) = log(x) - x + 1
for (f, op) in ((:logaddexp, +), (:logsubexp, -))
    @eval begin
        LogExpFunctions.$(f)(x::Num, y::Real) = log($(op)(exp(x), exp(y)))
        LogExpFunctions.$(f)(x::Real, y::Num) = log($(op)(exp(x), exp(y)))
        LogExpFunctions.$(f)(x::Num, y::Num) = log($(op)(exp(x), exp(y)))
    end
end
function LogExpFunctions.logsumexp(x::Union{AbstractVector{<:Num}, Arr})
    log(sum(exp, x; init = 0.0))
end
