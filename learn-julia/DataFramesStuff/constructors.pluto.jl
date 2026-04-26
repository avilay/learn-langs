### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 3805d13d-a150-4847-86c8-fd09c2bac043
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ 39d3bd8f-fe28-431d-9da1-7ca124ced9eb
using DataFrames

# ╔═╡ ee47b9c4-ec42-11f0-ba40-0d910dbae6fb
md"""
# Learning DataFrames
"""

# ╔═╡ 815eff54-ef9d-4270-8fac-444ce9b3f642
md"""
## Constructors

Different ways of creating a dataframe -
  * Use kwargs
  * Use pairs
  * Use dicts
  * Use named tuples (with dictcolumntable for missing values)
"""

# ╔═╡ 95d349c1-8aea-4647-9a6d-6fcfe0c11033
# the value of the desert column will be broadcasted
DataFrame(
	flavor=["Chocoloate Chip", "Snicker Doodle"], 
	calories=[200, 220], 
	desert=true
)

# ╔═╡ 500bc9c4-5713-4dad-a764-73de1ca186bb
# Can also use symbols as column names, either way the end result is the same
DataFrame(
	:flavor => ["Chocolate Chip", "Snicker Doodle"], 
	:calories => [200, 220]
)

# ╔═╡ 97a8cd72-2127-4da8-ab1c-17d9b66851f3
# Use dicts to create a dataframe
begin
	cookies = Dict(
		"flavor" => ["Chocolate Chip", "Snicker Doodle"],
		"calories" => [200, 220]
	)

	DataFrame(cookies)
end

# ╔═╡ 3aa837ba-74a4-4f83-8beb-5b0358817bb6
# Use vector of named tuples
shapes = [
	(type="circle", radius=10),
	(type="square", side=20)
]

# ╔═╡ ab76c1cd-058f-4873-b111-118a7eb0415c
# This will error out because all the fields are not present in all the elements
try
	DataFrame(shapes)
catch err
	@assert err isa FieldError
	print(err)
end

# ╔═╡ b781df2a-244a-49e3-92cb-d1154c2b8d00
# This will add `missing` values where needed
DataFrame(Tables.dictcolumntable(shapes))

# ╔═╡ 4ea514d9-ab96-4ba9-8535-efaa00a15293
D = rand(10, 3)

# ╔═╡ 124d8029-946e-43fb-bdcf-a96a4ec650b9
# Creating data from a matrix will automatically assign column names
DataFrame(D, :auto)

# ╔═╡ 72f53441-3d5e-4cc2-a3a0-009ba7fabc00
columnar_data = [
	D[:, 1],
	D[:, 2],
	D[:, 3]
]

# ╔═╡ b16cc143-9919-47f1-9076-d244917629fd
typeof(columnar_data)

# ╔═╡ 3b7867d8-9625-40d8-96c4-f0f8f8dc4fa3
header = [:speed, :velocity, :xdot]

# ╔═╡ 30a87a63-7be7-4600-9f0a-43927f2915d2
DataFrame(columnar_data, header)

# ╔═╡ Cell order:
# ╟─ee47b9c4-ec42-11f0-ba40-0d910dbae6fb
# ╠═3805d13d-a150-4847-86c8-fd09c2bac043
# ╠═39d3bd8f-fe28-431d-9da1-7ca124ced9eb
# ╟─815eff54-ef9d-4270-8fac-444ce9b3f642
# ╠═95d349c1-8aea-4647-9a6d-6fcfe0c11033
# ╠═500bc9c4-5713-4dad-a764-73de1ca186bb
# ╠═97a8cd72-2127-4da8-ab1c-17d9b66851f3
# ╠═3aa837ba-74a4-4f83-8beb-5b0358817bb6
# ╠═ab76c1cd-058f-4873-b111-118a7eb0415c
# ╠═b781df2a-244a-49e3-92cb-d1154c2b8d00
# ╠═4ea514d9-ab96-4ba9-8535-efaa00a15293
# ╠═124d8029-946e-43fb-bdcf-a96a4ec650b9
# ╠═72f53441-3d5e-4cc2-a3a0-009ba7fabc00
# ╠═b16cc143-9919-47f1-9076-d244917629fd
# ╠═3b7867d8-9625-40d8-96c4-f0f8f8dc4fa3
# ╠═30a87a63-7be7-4600-9f0a-43927f2915d2
