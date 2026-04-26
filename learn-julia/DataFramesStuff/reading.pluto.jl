### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ fbfa3f95-2337-4b66-a02f-5d86a27a4563
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ 79c59d1e-5777-4d13-a7be-01ad2273612d
begin
	using DataFrames
	using CSV
end

# ╔═╡ d9b9db5c-9b5f-402e-a543-f2bdcf148cd2
using Statistics

# ╔═╡ e4e3208a-ec44-11f0-b3d3-01f7e69767d0
md"""
# Learning DataFrames
"""

# ╔═╡ 33c696f1-fca1-4a52-be9a-6731d1ffda0c
tips = CSV.read("tips.csv", DataFrame)

# ╔═╡ 23930f86-6d71-4739-866d-9a924fca186f
md"""
## Reading Data

To get a single column -
  * Will give the underlying data without copying
    - `df.colname` will give the underlying data without copying,
    - `df[!, :colname]` will give the underlying data
    - `df[!, "colname"]`
  * Will give a copy of the underlying data
    - `df[:, :colname]`
    - `df[:, "colname"]`

I can iterate through each column with `eachcol(df)`.

Each column is simply an `AbstractArray` so all functions that work on arrays will continue to work on columns. E.g., I can apply `Statistics.mean(col)` and it will "just works".

To get a single row as a `DataFrameRow` object -
  * `df[idx, :]`
  * `first(df)` will get a single row, `first(df, n)` will get the first n rows in a new dataframe
  * `last(df)` and `last(df, n)`

`DataFrameRow` objects give the underlying data and therefore can mutate the original dataframe.

Indexing into the dataframe works exactly same as indexing into an array. The returned dataframe is a copy. And just like in arrays, if I want the actual data I can use `view()` or the `@view` macro. Using views and indexing is a lot more faster than only using indexing because there is no additional cost of copying, but it comes with the risk of accidentaly mutating the underlying data.

I can use boolean indexes to filter the dataframe like usual.
"""

# ╔═╡ a292ad39-86dd-428d-9d1a-e714693e577c
tips.total_bill

# ╔═╡ 4c7437f7-0439-4d38-a83f-7a4a824224a8
begin
	colname = "tip"
	tips[!, colname]
end

# ╔═╡ e7310aec-8ba6-4668-9b35-58c2c53fb30b
colname2 = :sex

# ╔═╡ c6a0d72f-4d71-44b4-84a5-d10f8b642663
# The returned column is **not** a copy!
sex = tips[!, colname2]

# ╔═╡ 2f5c9606-e806-433c-b6e3-cc705efbc63c
typeof(sex) <: AbstractArray

# ╔═╡ 3bf6292c-3ccc-49bb-ac28-3b699dfde0e8
sex[1] = "N.B"

# ╔═╡ 973724e2-cca8-47ba-a9b2-e0df6c1e304e
tips

# ╔═╡ cf89217e-4e75-450e-8993-7f0a0d31a0e4
# The returned column is a copy
sex_copy = tips[:, colname2]

# ╔═╡ 8c572444-365c-48f1-9993-4ba128a8f5ae
# Changing it will not change the df
sex_copy[1] = "Female"

# ╔═╡ 8e403d07-e356-4d66-af1c-cd223f0683fa
# sex of the first row is still N.B
tips

# ╔═╡ 4b539b79-6b93-4e8e-9cbb-9432e5264fab
head3 = first(tips, 3)

# ╔═╡ b899ac3a-4a6e-4815-ba1c-f1a624ffe9e1
typeof(head3)

# ╔═╡ cd4ca798-a831-4ccd-83a6-4669107104eb
# Similar to using index
tips[1:3, :]

# ╔═╡ b3664c72-269a-4c3d-9c26-df3e3868c05f
# This is mutable!
head = first(tips)

# ╔═╡ c4306a40-5e11-42be-b664-05849a0b240b
typeof(head)

# ╔═╡ 1659cc5e-b31d-4d25-8f72-0979a4076f97
# Changing it will change the df!
head.sex = "Female"

# ╔═╡ 71c04e9f-8492-447a-b390-d3b79265058c
# The sex column of the first row has been changed back to Female
tips

# ╔═╡ fb73627f-c1ce-426a-8fe3-ae227741dd23
# Again same as using index
tips[1, :] === head

# ╔═╡ d8c3a922-7c5c-4223-b664-1ceb447636e5
# No need for any special aggregating functions like in Pandas
# Using any function that acts on AbstractArrays will just works
mean(tips.tip)

# ╔═╡ e6bea8fc-67fa-4352-b877-17c2d22b2bf7
# I can even use the dot-function calls to filter rows
tips[tips.tip .> 6, :]

# ╔═╡ 06ec9b63-1202-4b7d-98e6-6c951245c710
tips[(6 .< tips.tip .< 9) .& (tips.sex .== "Male"), :]

# ╔═╡ bede8e8d-df85-4b07-822e-f090f054f3ed
tips[in(["Thur", "Fri"]).(tips.day), :]

# ╔═╡ 84299ca5-e15a-4ba9-a43a-f7dba855b648
unique(tips.day)

# ╔═╡ 9be3d2a2-0a52-4d21-a126-cb7900cc2cdc
@view tips[end:-1:end-10, [1, 3]]

# ╔═╡ e2aa41a0-b7ec-4a92-9ddb-e18574687489
md"""
### Joins

DataFrames supports a bunch of different joins out of which these are the ones I end up using the most -

  * `innerjoin()`
  * `outerjoin()`
  * `leftjoin()`
"""

# ╔═╡ 4e2c9abc-26e5-4f90-8064-e4ab320fb5f8
cookies = DataFrame(
	:id => 1:8,
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
	]
)

# ╔═╡ 0e553316-df99-4730-84f4-cdf19aa6b9af
reviews = DataFrame(
	:cookieid => rand(1:8, 20),
	:rating => round.(rand(20) .* (5.0 - 2.0) .+ 2.0, digits=2)
)

# ╔═╡ 3dd28fff-ad68-4fab-9210-5aa6cccf5ac4
innerjoin(cookies, reviews, on=:id => :cookieid)

# ╔═╡ 292fac7c-b82f-4158-bebe-ead5a5ac2a20
md"""
If the column name in both the dataframes had been the same say, `cookie_id`, then the call would look like `innerjoin(cookies, reviews, on=:cookie_id)`.
"""

# ╔═╡ Cell order:
# ╟─e4e3208a-ec44-11f0-b3d3-01f7e69767d0
# ╠═fbfa3f95-2337-4b66-a02f-5d86a27a4563
# ╠═79c59d1e-5777-4d13-a7be-01ad2273612d
# ╠═33c696f1-fca1-4a52-be9a-6731d1ffda0c
# ╟─23930f86-6d71-4739-866d-9a924fca186f
# ╠═a292ad39-86dd-428d-9d1a-e714693e577c
# ╠═4c7437f7-0439-4d38-a83f-7a4a824224a8
# ╠═e7310aec-8ba6-4668-9b35-58c2c53fb30b
# ╠═c6a0d72f-4d71-44b4-84a5-d10f8b642663
# ╠═2f5c9606-e806-433c-b6e3-cc705efbc63c
# ╠═3bf6292c-3ccc-49bb-ac28-3b699dfde0e8
# ╠═973724e2-cca8-47ba-a9b2-e0df6c1e304e
# ╠═cf89217e-4e75-450e-8993-7f0a0d31a0e4
# ╠═8c572444-365c-48f1-9993-4ba128a8f5ae
# ╠═8e403d07-e356-4d66-af1c-cd223f0683fa
# ╠═4b539b79-6b93-4e8e-9cbb-9432e5264fab
# ╠═b899ac3a-4a6e-4815-ba1c-f1a624ffe9e1
# ╠═cd4ca798-a831-4ccd-83a6-4669107104eb
# ╠═b3664c72-269a-4c3d-9c26-df3e3868c05f
# ╠═c4306a40-5e11-42be-b664-05849a0b240b
# ╠═1659cc5e-b31d-4d25-8f72-0979a4076f97
# ╠═71c04e9f-8492-447a-b390-d3b79265058c
# ╠═fb73627f-c1ce-426a-8fe3-ae227741dd23
# ╠═d9b9db5c-9b5f-402e-a543-f2bdcf148cd2
# ╠═d8c3a922-7c5c-4223-b664-1ceb447636e5
# ╠═e6bea8fc-67fa-4352-b877-17c2d22b2bf7
# ╠═06ec9b63-1202-4b7d-98e6-6c951245c710
# ╠═bede8e8d-df85-4b07-822e-f090f054f3ed
# ╠═84299ca5-e15a-4ba9-a43a-f7dba855b648
# ╠═9be3d2a2-0a52-4d21-a126-cb7900cc2cdc
# ╟─e2aa41a0-b7ec-4a92-9ddb-e18574687489
# ╠═4e2c9abc-26e5-4f90-8064-e4ab320fb5f8
# ╠═0e553316-df99-4730-84f4-cdf19aa6b9af
# ╠═3dd28fff-ad68-4fab-9210-5aa6cccf5ac4
# ╟─292fac7c-b82f-4158-bebe-ead5a5ac2a20
