# For broadcatins with sparse Arrays to work
function Base.Broadcast.combine_eltypes(_,::Tuple{Vararg{SparseMatrixCSC{<: Num}}})
    return Num
end

function Base.Broadcast.combine_eltypes(::typeof(substitute),::Tuple{T,Base.RefValue{Dict{Num,R}}}) where {T <: SparseMatrixCSC{<:Num},R <: Number}
    return Num
end

function Base.Broadcast.combine_eltypes(::typeof(substitute),::Tuple{T,Pair{Num,R}}) where {T <: SparseMatrixCSC{<:Num},R <: Number}
    return Num
end
