### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ f46c50ac-ed58-11f0-b372-f3efe02f5e5b
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ 6c42467f-be59-42b1-8c33-d1c310f4a6c7
begin
	using DataFrames
	using CSV
end

# ╔═╡ e295047a-4627-4aa4-a1c6-f33fa40e3b2e
langsdf = CSV.read("programming_languages.csv", DataFrame)

# ╔═╡ d0b981d0-5df5-47bc-af20-749914b29adf
describe(langsdf)

# ╔═╡ 9d80c99a-0415-41e3-a1c2-7586e091b457
langsary = Array(langsdf[:, :])

# ╔═╡ 02ff94a5-b3ee-4278-ad02-9fa3d228e56c
md"""
Given a language, tell me when it was invented?
"""

# ╔═╡ 11e30ce2-9e4b-4cfb-9978-71f876f5d71d
lang = "Julia"

# ╔═╡ 5b5c858e-5e78-4af7-a9ff-902675db4701
begin
	maybe_year_1 = langsary[langsary[:, 2] .== lang, 1]
	length(maybe_year_1) > 0 ? maybe_year_1[1] : ""
end

# ╔═╡ 39d7b8e1-4bdf-4dc4-a68b-99519d47eabd
begin
	maybe_year = langsdf[langsdf.language .== lang, :year]
	length(maybe_year) > 0 ? maybe_year[1] : ""
end

# ╔═╡ ca8ee038-7a4c-4b9c-a989-4d1f63b46992
findfirst(langsary[:, 2] .== lang)

# ╔═╡ e3777b41-b1cf-404b-9cd5-b900d96ec87d
md"""
Given a year, tell me how many languages were created in that year.
"""

# ╔═╡ 745157a9-6384-46bc-84d3-ac4944f2a159
year = 2011

# ╔═╡ 3b90b6fe-70c9-4b1a-867c-b0b654c6d187
langsary[langsary[:, 1] .== year, 2]

# ╔═╡ 62bd0c75-67a7-4f08-9904-a7a19e5350e6
# The size of the boolean mask will be the same as the size of the array so I
# cannot get the length of that, I first have to extract the elements at those
# coordinates and then find the length of those.
length(langsary[langsary[:, 1] .== year, 2])

# ╔═╡ d40af1db-ee16-4f34-a914-50608c36d259
langsdf[langsdf.year .== year, 2]

# ╔═╡ aaf90efb-2da5-4972-aaff-85474266c93a
length(langsdf[langsdf.year .== year, 2])

# ╔═╡ cd8554c0-11b9-4b66-a6f0-1c6a56a98b75
# Easier to use findall here since I am only interested in the length and not 
# the actual values
length(findall(langsary[:, 1] .== year))

# ╔═╡ 093d0f72-375d-405b-9778-d5c1d3a355b9
length(findall(langsdf.year .== year))

# ╔═╡ 2f4fb2d8-e093-4b96-8f5c-01a65f00ed44
langsdict = Dict{Int, Vector{String}}()

# ╔═╡ 3b3e4fb1-08a9-4f8d-98b8-3d6db70f5e30
for i in axes(langsary, 1)
	year, lang = langsary[i, :]
	langs = get!(langsdict, year, String[])
	push!(langs, lang)
end

# ╔═╡ 0771a003-fa68-4dd3-bcb1-4714765fa682
langsdict

# ╔═╡ 6c5e3a94-8426-45d5-857a-6f8c3906561c
langsdict[2011]

# ╔═╡ 5907d639-edef-4a53-882f-9fe129234283
empty!(langsdict)

# ╔═╡ a6a24e0e-dc59-4674-9d8c-9cbe50cb5ad0
for row in eachrow(langsdf)
	langs = get!(langsdict, row.year, String[])
	push!(langs, row.language)
end

# ╔═╡ 3b5e80d4-d9cd-44df-898d-d6a5ded2918c
langsdict

# ╔═╡ 041cb690-e2e5-4e73-9966-188288e3012b
langsdict[2011]

# ╔═╡ 8ed3b1b8-4a84-46dc-9fb4-2e40a551ea62
length(unique(langsdf.year))

# ╔═╡ ac4b53ab-0b19-4ca7-a145-37a05a6e6ab9
length(keys(langsdict))

# ╔═╡ 36935b92-aeb2-4acf-88fe-24d18215ffbf
# when was a language invented?
for (year, langs) in langsdict
	if lang ∈ langs
		println(year)
		break
	end
end

# ╔═╡ c9deea88-a2dc-44dd-b3c9-6823fd871080
# how many languages were invented in a given year?
length(get(langsdict, year, []))

# ╔═╡ Cell order:
# ╠═f46c50ac-ed58-11f0-b372-f3efe02f5e5b
# ╠═6c42467f-be59-42b1-8c33-d1c310f4a6c7
# ╠═e295047a-4627-4aa4-a1c6-f33fa40e3b2e
# ╠═d0b981d0-5df5-47bc-af20-749914b29adf
# ╠═9d80c99a-0415-41e3-a1c2-7586e091b457
# ╠═02ff94a5-b3ee-4278-ad02-9fa3d228e56c
# ╠═11e30ce2-9e4b-4cfb-9978-71f876f5d71d
# ╠═5b5c858e-5e78-4af7-a9ff-902675db4701
# ╠═39d7b8e1-4bdf-4dc4-a68b-99519d47eabd
# ╠═ca8ee038-7a4c-4b9c-a989-4d1f63b46992
# ╠═e3777b41-b1cf-404b-9cd5-b900d96ec87d
# ╠═745157a9-6384-46bc-84d3-ac4944f2a159
# ╠═3b90b6fe-70c9-4b1a-867c-b0b654c6d187
# ╠═62bd0c75-67a7-4f08-9904-a7a19e5350e6
# ╠═d40af1db-ee16-4f34-a914-50608c36d259
# ╠═aaf90efb-2da5-4972-aaff-85474266c93a
# ╠═cd8554c0-11b9-4b66-a6f0-1c6a56a98b75
# ╠═093d0f72-375d-405b-9778-d5c1d3a355b9
# ╠═2f4fb2d8-e093-4b96-8f5c-01a65f00ed44
# ╠═3b3e4fb1-08a9-4f8d-98b8-3d6db70f5e30
# ╠═0771a003-fa68-4dd3-bcb1-4714765fa682
# ╠═6c5e3a94-8426-45d5-857a-6f8c3906561c
# ╠═5907d639-edef-4a53-882f-9fe129234283
# ╠═a6a24e0e-dc59-4674-9d8c-9cbe50cb5ad0
# ╠═3b5e80d4-d9cd-44df-898d-d6a5ded2918c
# ╠═041cb690-e2e5-4e73-9966-188288e3012b
# ╠═8ed3b1b8-4a84-46dc-9fb4-2e40a551ea62
# ╠═ac4b53ab-0b19-4ca7-a145-37a05a6e6ab9
# ╠═36935b92-aeb2-4acf-88fe-24d18215ffbf
# ╠═c9deea88-a2dc-44dd-b3c9-6823fd871080
