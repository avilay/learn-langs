### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 431f7df1-fe97-4862-8459-819f47eff2ef
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ e91c7f78-05a7-4287-b91c-b76422aba096
begin
	using DataFrames
	using CSV
end

# ╔═╡ bd4a8e26-8523-45b9-ab48-33ba5ecb42b4
using Statistics

# ╔═╡ ee0e8da8-ec70-11f0-a326-f5b21707512a
md"""
# Learning DataFrames
"""

# ╔═╡ b34f5560-a923-4cd3-8242-36ff85d5591b
iris = CSV.read("iris.csv", DataFrame)

# ╔═╡ 4f928e57-890b-4fea-9b07-2a27c320d5b2
md"""
## Splitting with `groupby()`

The workhorse function that splits the data is `groupby`. It will group the dataframe by whatever key field I provide. All the rows in a group will have the same value for the key field.

Conceptually the returned object is a hashmap with the key as the index and the entire group as the value at that index.

Iteration:
  * Basic iteration through the returned object will yield each group. 
  * If I also want the index/key, I should iterate using `pairs(grps)`. 
  * I can get just the keys with `keys(grps)`. This will give me a wrapped vector of keys, not a iterator.

Access: I can access individual contained groups in the following ways -
  * Using a single numerical index
  * Using the key object as the index
  * Using a named tuple as the index
  * Using a plain 1-tuple as the index
  * Using a 1 element dict as the index (why?)

I can get multiple contained groups by using multiple keys. When using 1-tuples as indices, take care to provide each key in a separate 1-tuple, and provide vector of such 1-tuples.
"""

# ╔═╡ 26f4407c-f377-4b47-adb0-3a3b42b0fef6
species_groups = groupby(iris, :species)

# ╔═╡ 5d6352aa-b77c-4319-8abe-23768eb15c04
# Plain iteration will yield the contained groups
for species in species_groups
	println(typeof(species), " ", size(species))
end

# ╔═╡ e7f82c0d-84ed-449b-a97b-f4fd40073395
# Using pairs will yield key and the contained group
for (key, species) in pairs(species_groups)
	println(key, " ", typeof(species))
end

# ╔═╡ 1d13bfe1-14c9-4977-ab62-d76404c50370
for (key, species) in pairs(species_groups)
	println("Number of data points for $(key.species) is $(nrow(species))")
end

# ╔═╡ 0b3ad5ea-cb45-4c57-b3f3-74d21fd372bd
keys(species_groups)

# ╔═╡ 1ebe5789-03f7-4f04-a1a1-f37966beb247
size(keys(species_groups))

# ╔═╡ d353ba26-c2ad-4451-b286-a77d50f49fa5
# Index with a single number
species_groups[1]

# ╔═╡ d1baeccb-2de0-4bfc-a469-5c6a7f0a7227
# Index with a key object
key = last(keys(species_groups))

# ╔═╡ f2de10f2-5fce-4c8c-8d68-e105652cf5b2
species_groups[key]

# ╔═╡ e406b60d-d0dc-4456-937b-dccd46e2567c
# Index using named tuples
setosas = species_groups[(species="setosa",)]

# ╔═╡ 9f4f47ed-4345-41ca-bab9-53a5b210fb62
typeof(setosas)

# ╔═╡ f417550e-94c8-453d-a2a5-93be38a3a4b4
# Or using plain tuples
versicolors = species_groups[("versicolor",)]

# ╔═╡ aefd836f-af70-4959-86c3-ce2602bc64fb
# I can also select multiple groups at once, but it is tricky. Each group index
# is still in a tuple of its own, and I pass in a vector of such 1-tuples as the
# index
species_groups[[("setosa",), ("versicolor",)]]

# ╔═╡ 3ad9ff41-64ea-453e-b4fc-3e146bdbcbc6
# Can also be indexed using a dict with a single key => value
# Why would I ever do this?
species_groups[Dict("species" => "setosa")]

# ╔═╡ a65870f5-cbe4-47ec-bd95-221c84c059f5
md"""
## Applying and Combining with `combine()`

Remember the basic syntax of `combine(df, :colname => agg, ...)`. Here `agg(vals)` is an aggregator. It is not a vectorized function that will act on individual elements of the column. Instead it takes the entire column as input and aggregates it in some way to produce a scalar output. The output will still be a dataframe, but it will have a single row with the aggregated value or values if multiple columns were given.

When I give a grouped dataframe to `combine()`, it will apply the aggregator function to each group. The output will have a row for each group.

Any standard aggregator that takes in an `AbstractArray` will work.
"""

# ╔═╡ d0ff53cc-25c8-4aef-96f6-c72c667795f0
names(iris)

# ╔═╡ aa0d2ab2-a981-4151-b223-00f9714d5654
# Most straightforward usage, the column name is automatically generated
combine(species_groups, :petal_length => mean)

# ╔═╡ c8686d85-130b-4706-b416-44671e15257a
# I can choose the column name as well
combine(species_groups, :petal_length => mean => :mean_length)

# ╔═╡ c4101d7e-af0d-4388-b74c-2574e2bcb21f
# Multiple aggregators, compund aggregators acting on multiple columns
combine(
	species_groups,
	[:petal_length, :sepal_length] => ( (p, s) -> mean(p)/mean(s) ) => :a,
	:petal_length => sum => :b
)

# ╔═╡ e6272f39-8e48-4287-b291-fe60b1e03c11
# I can "simplify" the above with a single aggregator function outputting two values
# but have to use the AsTable functionality. 
# 99% sure I'll never use this!
combine(
	species_groups,
	[:petal_length, :sepal_length] => 
		((p, s) -> (a=mean(p)/mean(s), b=sum(p))) => AsTable
)

# ╔═╡ ed47dd88-e692-4e72-aae1-ce0d2058ef0c
# Another way to use multiple aggregators in one
combine(
	species_groups,
	[:petal_length, :sepal_length] => 
		((p, s) -> (a=mean(p)/mean(s), b=sum(p))) => [:a, :b]
)

# ╔═╡ 7197ae15-7b74-4744-98ad-0b9a2c2915c0
# A better example of using multiple aggregators in one
combine(species_groups, :petal_length => (x -> [extrema(x)]) => [:min, :max])

# ╔═╡ 1c5b4c82-6a45-4cab-b41f-67fd774e8547
valuecols(species_groups)

# ╔═╡ 21d7e248-ebcb-41ad-af18-6c7707788783
# Aggregate all columns at once by using dot-function to generate :col => agg pairs
combine(species_groups, valuecols(species_groups) .=> mean)

# ╔═╡ 63536d0d-dc56-4b5c-a385-ba9c4a67af8b
md"""
## Column Independent Operations

There are certain operations that will give the same answer regardless of which column in the group they are applied to, e.g., counting the number of rows. For such operations I don't have to follow the usual `combine(grps, :col => agg)` calling pattern, I can just say `combine(grps, aggop)`. Here are five such ops that work this way. Some of them work only inside `combine()`, some work indepedently. I won't go into the nuances of each. See [the docs](https://dataframes.juliadata.org/stable/man/split_apply_combine/#Column-independent-operations) for that.

  * `nrow`: counting the number of rows
  * `proprow`: getting the proportion of rows in a group
  * `groupindices`: getting the index of the group
  * `eachindex`: getting the index of each row within a group
"""

# ╔═╡ 65a0218f-3aee-489b-9605-8ce08bdee406
combine(species_groups, nrow, proprow)

# ╔═╡ af57c951-0891-4b22-bd21-81b06626b5c8
combine(species_groups, groupindices)

# ╔═╡ 68d875c1-a73a-415a-87fb-7123eac4cb1c
md"""
Let me create a smaller dataset to demo some other stuff
"""

# ╔═╡ 3240688b-60cb-4682-93b3-fcc08235553f
quarks = DataFrame(
	:collisions => [2, 54, 70, 92, 77, missing, missing, 97, missing, 81],
	:quark => ["down", "down", "bottom", "down", "top", "top", "bottom", "strange", "charm", "charm"]
)

# ╔═╡ e1a6490a-a2bf-426a-b811-10db2fa0703e
grps = groupby(quarks, :quark)

# ╔═╡ 348f68fe-b553-4707-8228-038eae32ac14
for (quark, grp) in pairs(grps)
	println(quark.quark)
	println(grp)
	println("----")
end

# ╔═╡ 41b1c226-175c-421e-ba06-5b7b753c1f55
combine(grps, eachindex, groupindices)

# ╔═╡ 74887f2e-1427-4438-9d25-d413eb15545a
md"""
## Apply with `select()` and `transform()`

These will aggregate the columns inside each group, but then will "de-normalize" the aggregated value to each row in the entire dataframe. In the example below, each group's collisions are totaled, but then they show up in each row of the entire dataframe.
"""

# ╔═╡ 221e0d06-1f21-4968-8ec0-6609ab9482ee
select(grps, :collisions => (sum ∘ skipmissing))

# ╔═╡ 4f2b4f9f-0ab0-4975-96b2-48b13cc8871e
# ╠═╡ disabled = true
#=╠═╡
combine(x -> std(x.petal_length)/std(x.sepal_length), species)
combine(species, 1:2 => cor, nrow)
combine(species) do df
	(m = mean(iris.petal_length), s² = var(df.petal_length))
end
  ╠═╡ =#

# ╔═╡ 9100f8b8-9162-4ad1-ae56-89473c64325c
# ╠═╡ disabled = true
#=╠═╡
select(species, 1:2 => cor)
transform(species, :species => x -> chop.(x, head=5, tail=0))
  ╠═╡ =#

# ╔═╡ Cell order:
# ╟─ee0e8da8-ec70-11f0-a326-f5b21707512a
# ╠═431f7df1-fe97-4862-8459-819f47eff2ef
# ╠═e91c7f78-05a7-4287-b91c-b76422aba096
# ╠═b34f5560-a923-4cd3-8242-36ff85d5591b
# ╟─4f928e57-890b-4fea-9b07-2a27c320d5b2
# ╠═26f4407c-f377-4b47-adb0-3a3b42b0fef6
# ╠═5d6352aa-b77c-4319-8abe-23768eb15c04
# ╠═e7f82c0d-84ed-449b-a97b-f4fd40073395
# ╠═1d13bfe1-14c9-4977-ab62-d76404c50370
# ╠═0b3ad5ea-cb45-4c57-b3f3-74d21fd372bd
# ╠═1ebe5789-03f7-4f04-a1a1-f37966beb247
# ╠═d353ba26-c2ad-4451-b286-a77d50f49fa5
# ╠═d1baeccb-2de0-4bfc-a469-5c6a7f0a7227
# ╠═f2de10f2-5fce-4c8c-8d68-e105652cf5b2
# ╠═e406b60d-d0dc-4456-937b-dccd46e2567c
# ╠═9f4f47ed-4345-41ca-bab9-53a5b210fb62
# ╠═f417550e-94c8-453d-a2a5-93be38a3a4b4
# ╠═aefd836f-af70-4959-86c3-ce2602bc64fb
# ╠═3ad9ff41-64ea-453e-b4fc-3e146bdbcbc6
# ╟─a65870f5-cbe4-47ec-bd95-221c84c059f5
# ╠═bd4a8e26-8523-45b9-ab48-33ba5ecb42b4
# ╠═d0ff53cc-25c8-4aef-96f6-c72c667795f0
# ╠═aa0d2ab2-a981-4151-b223-00f9714d5654
# ╠═c8686d85-130b-4706-b416-44671e15257a
# ╠═c4101d7e-af0d-4388-b74c-2574e2bcb21f
# ╠═e6272f39-8e48-4287-b291-fe60b1e03c11
# ╠═ed47dd88-e692-4e72-aae1-ce0d2058ef0c
# ╠═7197ae15-7b74-4744-98ad-0b9a2c2915c0
# ╠═1c5b4c82-6a45-4cab-b41f-67fd774e8547
# ╠═21d7e248-ebcb-41ad-af18-6c7707788783
# ╟─63536d0d-dc56-4b5c-a385-ba9c4a67af8b
# ╠═65a0218f-3aee-489b-9605-8ce08bdee406
# ╠═af57c951-0891-4b22-bd21-81b06626b5c8
# ╟─68d875c1-a73a-415a-87fb-7123eac4cb1c
# ╠═3240688b-60cb-4682-93b3-fcc08235553f
# ╠═e1a6490a-a2bf-426a-b811-10db2fa0703e
# ╠═348f68fe-b553-4707-8228-038eae32ac14
# ╠═41b1c226-175c-421e-ba06-5b7b753c1f55
# ╟─74887f2e-1427-4438-9d25-d413eb15545a
# ╠═221e0d06-1f21-4968-8ec0-6609ab9482ee
# ╠═4f2b4f9f-0ab0-4975-96b2-48b13cc8871e
# ╠═9100f8b8-9162-4ad1-ae56-89473c64325c
