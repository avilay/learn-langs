module BinarySearchTrees

export BinarySearchTree, nodedata, rightnode, leftnode

mutable struct BinarySearchTree{T}
	data::Union{Nothing, T}
	left::Union{Nothing, BinarySearchTree{T}}
	right::Union{Nothing, BinarySearchTree{T}}
end

BinarySearchTree() = BinarySearchTree{Int}(nothing, nothing, nothing)

BinarySearchTree(val) = BinarySearchTree(val, nothing, nothing)

function BinarySearchTree(vals::Vector{T}) where T
	root = BinarySearchTree(vals[1], nothing, nothing)
	foreach(val -> push!(root, val), vals[2:end])
	root
end

function Base.push!(node::BinarySearchTree{T}, data::T) where T
	if data <= node.data
		if node.left === nothing
			node.left = BinarySearchTree(data, nothing, nothing)
		else
			push!(node.left, data)
		end
	else
		if node.right === nothing
			node.right = BinarySearchTree(data, nothing, nothing)
		else
			push!(node.right, data)
		end
	end
	node
end

function Base.in(item::T, node::Union{Nothing, BinarySearchTree{T}}) where T
	node === nothing && return false
	item == node.data && return true
	item < node.data && return in(item, node.left)
	return in(item, node.right)
end

function Base.sort(node::Union{Nothing,BinarySearchTree{T}}) where T
	node === nothing && return []
	[sort(node.left)..., node.data, sort(node.right)...]
end

nodedata(tree::BinarySearchTree) = tree.data
rightnode(tree::BinarySearchTree) = tree.right
leftnode(tree::BinarySearchTree) = tree.left

end