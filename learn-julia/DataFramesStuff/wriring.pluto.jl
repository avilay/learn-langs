### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ dd149be1-1ac3-4ea8-ab8c-2431fc5a6ad9
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ ce2584d6-27e1-4602-b30f-ede9a9abfc2b
begin
	using DataFrames
	using CSV
end

# ╔═╡ 43a2fe0e-ec47-11f0-88a4-212133615d0d
md"""
# Learning DataFrames
"""

# ╔═╡ a4b350fc-6953-4296-bc1b-d23c7ab1d515
tips = CSV.read("tips.csv", DataFrame)

# ╔═╡ 32ffddbe-24a6-4de8-8a26-054269ecebd0
md"""
## Writing Data

⚠️ Even though `df[:, col]` gives a copy of the data when reading, when writing I can actually replace columns using `df[:, col] = [...new vals...]` as long as the shapes match! This works for any subset of data, e,g., `data[1:4, col] = 100:103` will replace the first four rows of the specified col with the new data.

I can change the entire column by `df.col = newvector` or `df[!, col] = newvector` as long as the shapes match. 
⚠️ The df starts pointing to the `newvector`. I can change the df by changing `newvector`.

As seen above, I can change a row as long as I have a `DataFrameRow` object that I can get with `first()`, `last()`, or `df[idx, :]`.

Broadcasting works like it does for arrays.

I can add new cols by -
  * `df.newcol = vals` as long as the shapes match
  * `insertcols!(df, colpos, :colname => val)` will pseudo-broadcast `val` to all the rows.
"""

# ╔═╡ a552dcca-2216-4fa4-abeb-8fa94401ed83
nonbinary = fill("N.B", nrow(tips))

# ╔═╡ a07c8655-a806-4eca-857a-eb9fdafbdde4
# This will mutate the df!
tips[:, :sex] = nonbinary

# ╔═╡ af6ce8da-6f19-4e2e-ad62-e4d9a7aba92e
tips

# ╔═╡ 4e097649-5442-4c50-b12e-a675ded99928
# The values are copied from the nonbinary vector into the df
tips.sex === nonbinary

# ╔═╡ d64eb007-fa12-41db-bbe2-2f5b73076538
sundays = fill("Sunday", nrow(tips))

# ╔═╡ cc087547-b816-426c-8c71-4b9131ae8d12
# This will also mutate the df but it is not so surprising because it returns the
# the actual data
tips.day = sundays

# ╔═╡ 37453658-e69e-4bab-a756-201e8c7d7b69
tips

# ╔═╡ fbe7785f-27a2-4b7b-bfea-b63b760400a7
# However, this did not copy the sundays vector
# the df col is actually pointing to it!
tips.day === sundays

# ╔═╡ 879f3a72-040b-41d1-9e2c-318e0f365823
# I can change the df by changing this vector!
sundays[1] = "MONDAY"

# ╔═╡ 9e2d4bf7-aacc-44e7-9e0e-78f88205a8d0
tips

# ╔═╡ 01db9c85-a42d-4da4-93ad-dabf18df1c27
head = first(tips)

# ╔═╡ f505bafb-d48c-4090-9833-472d9cb5a504
head.day = "Sunday"

# ╔═╡ 3c245264-681e-4d2c-a899-a969d54c59c6
tips

# ╔═╡ 65d12566-642e-4779-89ba-b48e6477945f
# No need to use fill every time, I can just use brodcast
tips[1:4, :day] .= "MONDAY"

# ╔═╡ d2b79f7c-77b5-4ebb-a36e-e183a9d2dd3f
tips

# ╔═╡ da4aebbd-9c4a-46cd-a03e-e1c315c94175
rows = rand(1:nrow(tips), 10)

# ╔═╡ d16ee639-ebb0-4f46-bc01-e1028da00323
tips_small = tips[rows, :]

# ╔═╡ 8663ec27-cb97-4cac-ae12-ab9aa48eb11b
tips_small.city = rand(["Seattle", "San Francisco"], 10)

# ╔═╡ bfcc9e58-a61f-4d29-902e-0381b0e86a2b
tips_small

# ╔═╡ Cell order:
# ╟─43a2fe0e-ec47-11f0-88a4-212133615d0d
# ╠═dd149be1-1ac3-4ea8-ab8c-2431fc5a6ad9
# ╠═ce2584d6-27e1-4602-b30f-ede9a9abfc2b
# ╠═a4b350fc-6953-4296-bc1b-d23c7ab1d515
# ╟─32ffddbe-24a6-4de8-8a26-054269ecebd0
# ╠═a552dcca-2216-4fa4-abeb-8fa94401ed83
# ╠═a07c8655-a806-4eca-857a-eb9fdafbdde4
# ╠═af6ce8da-6f19-4e2e-ad62-e4d9a7aba92e
# ╠═4e097649-5442-4c50-b12e-a675ded99928
# ╠═d64eb007-fa12-41db-bbe2-2f5b73076538
# ╠═cc087547-b816-426c-8c71-4b9131ae8d12
# ╠═37453658-e69e-4bab-a756-201e8c7d7b69
# ╠═fbe7785f-27a2-4b7b-bfea-b63b760400a7
# ╠═879f3a72-040b-41d1-9e2c-318e0f365823
# ╠═9e2d4bf7-aacc-44e7-9e0e-78f88205a8d0
# ╠═01db9c85-a42d-4da4-93ad-dabf18df1c27
# ╠═f505bafb-d48c-4090-9833-472d9cb5a504
# ╠═3c245264-681e-4d2c-a899-a969d54c59c6
# ╠═65d12566-642e-4779-89ba-b48e6477945f
# ╠═d2b79f7c-77b5-4ebb-a36e-e183a9d2dd3f
# ╠═da4aebbd-9c4a-46cd-a03e-e1c315c94175
# ╠═d16ee639-ebb0-4f46-bc01-e1028da00323
# ╠═8663ec27-cb97-4cac-ae12-ab9aa48eb11b
# ╠═bfcc9e58-a61f-4d29-902e-0381b0e86a2b
