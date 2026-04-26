mutable struct BinarySearchTree{T}
	data::T
	left::Union{Nothing, BinarySearchTree}
	right::Union{Nothing, BinarySearchTree}
end

BinarySearchTree() = BinarySearchTree{Int}(-1, nothing, nothing)

BinarySearchTree(val) = BinarySearchTree(val, nothing, nothing)

function BinarySearchTree(vals::Vector{T}) where T
	root = BinarySearchTree(vals[1], nothing, nothing)
	println("Inside the vector implementation")
	root
end