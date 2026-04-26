### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 3840985a-a3f3-4cc3-8164-83d6e5457252
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ 8b5716b0-5f4b-44ac-baee-8caf806ba5ac
begin
	using DataFrames
	using CSV
end

# ╔═╡ 3136375c-ec44-11f0-86c6-230501f2a587
md"""
# Learning DataFrames
"""

# ╔═╡ 5497d01e-d075-4a69-83e0-8642ba660ce3
md"""
## Loading Data

I can read a CSV file into any table "sink", the `DataFrame` constructor is one such sink. 

I'll notice that DataFrame uses special String objects like String3, these are similar to varchar(3) and are more efficient than a plain old String.
"""

# ╔═╡ d3d96892-580f-47cb-9c47-a4af67440ef1
tips = CSV.read("tips.csv", DataFrame)

# ╔═╡ 3da9493b-9a5d-427e-8c89-8888813a2502
typeof(tips)

# ╔═╡ 912eb2f3-3167-4c62-88d8-47ea94c6f11f
md"""
### Metadata

  * `names(df[, Type])` returns the colnames (with a specified data type) as strings
  * `propertynames(df)` returns the colnames as symbols, no way to specify the eltype
  * `size(df[, dim])` returns the shape `(nrows, ncols)`
  * `nrow(df)`
  * `ncol(df)`
  * `describe(df[, ;cols=<range of cols>])` reports the default stats
"""

# ╔═╡ f6a1cb98-f421-43a8-bd18-ad8d6fca37c5
# Gets all the columns
names(tips)

# ╔═╡ 1d8eab73-f627-4620-ace8-7d10074d103f
# Gets only the String columns
names(tips, AbstractString)

# ╔═╡ 7d943636-583c-4871-8a74-02ec315bccef
propertynames(tips)

# ╔═╡ 96bffa54-ea7f-43b7-9b8e-6b56cfb79904
size(tips)

# ╔═╡ 30f8aa54-a715-4036-bd0c-fc6ebfd67334
describe(tips)

# ╔═╡ 982e0cac-630f-43d0-8bad-ddf880c84d47
md"""
## Saving Data
"""

# ╔═╡ 38fcef51-94f5-4dce-a66a-d9344c4ad862
cookies = DataFrame(
	:flavor => [
		"Chocolate Chip",
		"Snicker Doodle",
		"Oatmeal Raisin",
		"Double Chocolate Chip",
		"Cranberries Almond",
		"Oatmeal Marshmallow",
		"Pink Sugar",
		"Brownie Batter"
	],
	:calories => [
		200,
		220,
		180,
		250,
		190,
		250,
		300,
		280
	],
	:rating => [
		4.9,
		4.1,
		3.8,
		4.8,
		4.3,
		3.9,
		4.75,
		2.3
	]
)

# ╔═╡ 78e12299-a307-423b-ba2f-11f525eec1eb
CSV.write("cookies.csv", cookies)

# ╔═╡ d2401168-e657-42ce-9d80-0e3117ab392e


# ╔═╡ Cell order:
# ╟─3136375c-ec44-11f0-86c6-230501f2a587
# ╠═3840985a-a3f3-4cc3-8164-83d6e5457252
# ╠═8b5716b0-5f4b-44ac-baee-8caf806ba5ac
# ╟─5497d01e-d075-4a69-83e0-8642ba660ce3
# ╠═d3d96892-580f-47cb-9c47-a4af67440ef1
# ╠═3da9493b-9a5d-427e-8c89-8888813a2502
# ╟─912eb2f3-3167-4c62-88d8-47ea94c6f11f
# ╠═f6a1cb98-f421-43a8-bd18-ad8d6fca37c5
# ╠═1d8eab73-f627-4620-ace8-7d10074d103f
# ╠═7d943636-583c-4871-8a74-02ec315bccef
# ╠═96bffa54-ea7f-43b7-9b8e-6b56cfb79904
# ╠═30f8aa54-a715-4036-bd0c-fc6ebfd67334
# ╟─982e0cac-630f-43d0-8bad-ddf880c84d47
# ╠═38fcef51-94f5-4dce-a66a-d9344c4ad862
# ╠═78e12299-a307-423b-ba2f-11f525eec1eb
# ╠═d2401168-e657-42ce-9d80-0e3117ab392e
