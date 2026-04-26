### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 83e8a717-27d0-476f-843a-ded6b7f94171
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ 011650c3-f5fc-4446-9fd7-e67937be8998
using DataFrames

# ╔═╡ f97e36ae-ed51-11f0-ae69-65fa61ef4fad
md"""
# Learning DataFrames
"""

# ╔═╡ 9337fd97-e181-44ec-8b53-b338af037652
function genquarks(nrows; has_missing=true, pct_missing=10)
	(has_missing && pct_missing == 0) && error("cannot have 0% missing values!")
	# if has_missing is set to false, then ignore the pct_missing param
	
	quark = rand(["up", "down", "top", "bottom", "strange", "charm"], nrows)
	collisions = rand(1:100, nrows)
	duration = rand(nrows)
	if has_missing
		collisions_::Vector{Union{Missing,Int}} = collisions
		duration_::Vector{Union{Missing,Float64}} = duration
		n_missing = nrows * pct_missing ÷ 100
		collisions_[rand(1:nrows, n_missing)] .= missing
		duration_[rand(1:nrows, n_missing)] .= missing
		return DataFrame(:quark => quark, :collisions => collisions_, :duration => duration_)
	end
	DataFrame(:quark => quark, :collisions => collisions, :duration => duration)
end

# ╔═╡ 616cf6fa-b310-44d4-8bec-52f34396b68b
quarks = genquarks(20)

# ╔═╡ 44fc2ba2-bf25-446f-8f5e-b4185a6815af
# Drops rows which have a missing value in any column
dropmissing(quarks)

# ╔═╡ 55c74e07-b3fe-4021-bd49-e12386967480
# Or I can choose the columns in which missing values matter
# Rows with only duration missing are still there
dropmissing(quarks, :collisions)

# ╔═╡ d6caf28b-bb93-4d01-963b-96726d9c5df5
# Trying to sum the collisions will propagate missing because x + missing = missing
sum(quarks.collisions)

# ╔═╡ 54564097-c8c5-46ef-8a2c-4d6999bfe260
# The raw output of skipmissing still has missing values
skipmissing(quarks.collisions)

# ╔═╡ d2869998-93a6-48bd-91ec-77fb5cf8d411
# But if I collect it, they will disappear
(collect ∘ skipmissing)(quarks.collisions)

# ╔═╡ 33deb8ef-97e1-4d92-93d0-ca22d15cd38a
# I can pass the output of skipmissing to any function and it will get rid of the
# missing values
(sum ∘ skipmissing)(quarks.collisions)

# ╔═╡ 0954c4d5-06e7-47b0-a5c4-726b6a864eb5
# Another common scenario is to replace the missing value with something else
# Use the coalesce function. It is a binary function, i.e., it accepts a scalar
# which may or may not be missing, and the replacement value.
# If I want to apply it to the entire column, I must use dot
coalesce.(quarks.collisions, -1)

# ╔═╡ Cell order:
# ╟─f97e36ae-ed51-11f0-ae69-65fa61ef4fad
# ╠═83e8a717-27d0-476f-843a-ded6b7f94171
# ╠═011650c3-f5fc-4446-9fd7-e67937be8998
# ╠═9337fd97-e181-44ec-8b53-b338af037652
# ╠═616cf6fa-b310-44d4-8bec-52f34396b68b
# ╠═44fc2ba2-bf25-446f-8f5e-b4185a6815af
# ╠═55c74e07-b3fe-4021-bd49-e12386967480
# ╠═d6caf28b-bb93-4d01-963b-96726d9c5df5
# ╠═54564097-c8c5-46ef-8a2c-4d6999bfe260
# ╠═d2869998-93a6-48bd-91ec-77fb5cf8d411
# ╠═33deb8ef-97e1-4d92-93d0-ca22d15cd38a
# ╠═0954c4d5-06e7-47b0-a5c4-726b6a864eb5
