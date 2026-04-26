### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ c0ed7bdb-4d71-4e4a-8b51-74eaf90f4de5
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ 27827423-d9ff-43ce-833d-f04a00bc1555
begin
	using DataFrames
	using CSV
end

# ╔═╡ f2836d3c-a2ff-485b-922f-7a8136c7c29e
using Statistics

# ╔═╡ 02e5ff12-ec4a-11f0-bfef-31026ecf2b5d
md"""
# Learning DataFrames
"""

# ╔═╡ 50bfd5ae-3bac-43dd-acef-ed91a6cb724d
tips = CSV.read("tips.csv", DataFrame)

# ╔═╡ 95fc3b4c-1bb8-4d19-b9f7-d81cc32d81ac
quarks = DataFrame(
	:x => [2, 54, 70, 92, 77, missing, missing, 97, missing, 81],
	:y => ["down", "down", "bottom", "down", "top", "top", "bottom", "strange", "charm", "charm"]
)

# ╔═╡ b96601cb-db6a-4d1d-89b4-48f6329b84b4
md"""
## Manipulation Functions

  * `transform()` or `transform!()`
  * `select(df, op)` or `select!()`: Used to select/de-select columns, rename them, transform them, add new cols, etc.
  * `subset(df, :col1 => pred1, ...[, skipmissing=true|false])` or `subset!()`: This is very similar to standard boolean mask indexing except I can set `skipmissing=true`. All the predicates are `and`-ed together to filter the rows.
  * `combine()`

All these functions have the same signature - `func(df, op)`, where `op` can take one of the following forms -
  * `source_column_selector`
  * `source_column_selector => op_func => new_colnames`
  * `source_column_selector => new_colnames`
"""

# ╔═╡ 4c8b50e0-9c3a-4fe8-abbe-598bf02053e8
md"""
### Subsets

In the examples below, when filtering `:y` column, index masks are much more clear and concise. But when filtering `:x` column, using `subet` is lot less clunkier than index masks because of missing values.
"""

# ╔═╡ b0fe2c66-d614-4a2c-904e-7105d60cc0ce
strange_or_charm = in(["strange", "charm"])

# ╔═╡ b5a051a6-51dd-4ff6-b203-ef0c9ce83955
subset(quarks, :y => yvals -> strange_or_charm.(yvals))

# ╔═╡ 352b2de2-64eb-4e21-8277-629023581c78
quarks[strange_or_charm.(quarks.y), :]

# ╔═╡ badb5e9d-85c8-4339-bab4-68d6e9606f9a
try
	quarks[iseven.(quarks.x), :]
catch err
	@assert err isa ArgumentError
	print(err)
end

# ╔═╡ 16fe15e1-9d97-48aa-ac41-3567427fea35
# I can just filter out the missing values
quarks[(quarks.x .!== missing) .& (iseven.(quarks.x)), :]

# ╔═╡ 74334d10-9f05-4489-866c-b4cfbdbde455
# Of I can impute the missing values with something else using `coalesce`
quarks[coalesce.(iseven.(quarks.x), false), :]

# ╔═╡ ad15f49d-883f-49ca-9825-28fc8617424a
md"""
In this case skipping or coalescing result in the same result, but there are cases where for example I am computing averages, I might need to skip rather than coalesce the missing values with 0 (say).

If I want to skip the missing values, then using `subset` is a lot less clunkier.
"""

# ╔═╡ 7c50191e-a59d-421d-9203-bf5e221b543d
subset(quarks, :x => vals -> iseven.(vals), skipmissing=true)

# ╔═╡ 7415a1bd-ca0d-4f62-83f0-86cf775104a4
md"""
### Select

The main purpose of `select()` function is similar to the `SELECT` statement in SQL, it is to select some of the columns from the dataframe. As I select the columns I am interested in, I can rename them, transform them, and even add new derivative columns from one or more of the selected columns.

If I want to simply select two or more columns into a separate dataframe, the indexing syntax is more intuitive. However, if I want to select a single column in a new dataframe, indexing will not work because it will give me back a vector. `select()` on the other hand will always return a dataframe, even for single columns.

`select()` is also useful when I am renaming one or more of the selected columns. In fact, I don't know how I'd do this with indexing. I can transform and rename columns at the same time with `select()`. I can transform a column with indexing, but it will be done in place rather than in a copy.

I can pass the `copycols=false` flag if I want the returned df to point to the orig df's data.
"""

# ╔═╡ bfec98c0-4da3-44a6-bfda-135001a64aa1
# Selecting these two columns from the dataframe
select(tips, :total_bill, :tip)

# ╔═╡ b44e70b1-2cdc-45c3-b4ee-dd7a1b4319cc
# Easier to do with indexing
tips[:, [:total_bill, :tip]]

# ╔═╡ bce6d214-9ced-419f-83a3-51b34d6ebbd3
# However, this gives me back a vector
tips[:, :total_bill]

# ╔═╡ a376f5a8-3a13-4c38-a68f-57ca2ce439aa
# but I still get back a dataframe with this
select(tips, :total_bill)

# ╔═╡ 86003f27-27e5-40aa-9e9c-6051941e6d69
# Renaming a few cols, don't know how I'd do this with indexing
eatingout = select(
	tips, 
	:sex => :waitstaff_gender, 
	:time => :meal_type, 
	:size=>:n_people
)

# ╔═╡ ef73e6d9-7af3-483d-b479-7903234a22d1
# Transforming and renaming a column at the same time
select(
	tips, 
	:smoker => (vals -> vals .== "Yes") => :is_smoker, 
	:tip
)

# ╔═╡ 6420f348-c8f6-45b6-a48e-88ee037e4424
# Just transforming a column without select
begin
	eatingout2 = tips[:, [:sex, :time]]
	eatingout2.sex = lowercase.(eatingout2.sex)
	eatingout2
end

# ╔═╡ 1b9c36c4-b1b9-44db-8452-7a9c3a84cc22
# Just transforming a column with select gives it a bad default name
select(
	tips,
	:sex => (svals -> lowercase.(svals))
)

# ╔═╡ 3d0af170-e791-482c-a40f-2b5f420a2f48
# Adding a new composite col
select(
	tips, 
	:sex, 
	:smoker, 
	:day, 
	:time,
	[:tip, :total_bill] => ((amt, tot) -> 100 .* amt ./ tot) => :tip_pct
)

# ╔═╡ 9e7bf42e-c55c-4292-a433-8c8791a8e3a8
md"""
### Rename

There is a dedicated function for renaming columns.
"""

# ╔═╡ a277e4ff-08c7-4c4f-bda7-bf3d31dd16d3
rename(
	tips,
	:sex => :waitstaff_gender,
	:smoker => :is_smoker,
	:time => :meal_type,
	:size => :n_people
)

# ╔═╡ cd130d0b-2a73-4a7a-a221-82b84b4d5e31


# ╔═╡ e517e969-5bf2-4662-b264-7d76c567c0fb
md"""
### Transform

The main purpose of transform is to rename and transform the dataframe. Unlike `select()`, there is no concept of selecting specific columns, and then acting on them. Here, I simply transform the columns I want, the returned dataframe will have all the columns without my having to explicitly select them.

There is no easy way to rename columns using `transform()` because it expects the `op` to be `cols => func => newcols`. I can use the `identity` function for the func, but it is better to use the `rename()` function. Just giving `colname => newcolname` will create a **new** column called `newcolname`!
"""

# ╔═╡ 9ffb9fa6-619e-4d04-b102-1866dc9f976e
# Now there are two identical columns :sex and :waitstaff_gender
transform(tips, :sex => :waitstaff_gender)

# ╔═╡ f113442c-1ed5-401f-8191-9a6abf39c07e
# Transforming a few cols in the entire dataframe
transform(
	tips, 
	:smoker => (vals -> vals .== "Yes") => :smoker,
	[:tip, :total_bill] => ((amt, tot) -> 100 .* amt ./ tot) => :tip_pct
)

# ╔═╡ 8ebbf25b-a70a-4ecb-ae28-eed6c5c5a7d8
tips

# ╔═╡ c2aab20d-76bf-40df-a4a2-ba5620b551f6
md"""
Lets sanitize and modernize this CSV first.

  1. Rename :sex to :waitstaff_gender
  2. Make :smoker into a boolean field and rename it to :is_smoking_section
  3. Rename :time to :meal_type
  4. Add a :tip_pct col
  5. Days are "Sun", "Sat", "Thur", "Fri". Change all "Thur" to "Thu" for consistency.
  6. Rename :size to :n_people
  7. Rename :total_bill to :total
"""

# ╔═╡ 3c20cc7c-f49c-49aa-a954-7925902dfb31
fixday(day) = day == "Thur" ? "Thu" : day

# ╔═╡ 679d59bc-726c-446e-a054-cecb77b07cc4
temp1 = transform(
	tips,
	:smoker => (vals -> vals .== "Yes") => :smoker,
	[:tip, :total_bill] => ((amt, tot) -> 100 .* amt ./ tot) => :tip_pct,
	:day => (dvals -> fixday.(dvals)) => :day
)

# ╔═╡ 2beba575-c978-4529-909c-32b87297dcd9
tips2 = rename(
	temp1, 
	:total_bill => :total,
	:sex => :waitstaff, 
	:smoker => :is_smoking, 
	:time => :meal_type,
	:size => :n_people
)

# ╔═╡ 16548e74-e435-4f8d-ad33-c8afdef8c408
md"""
### Combine

Combine aggregates the entire column into a single value. 
"""

# ╔═╡ 866108f0-4de5-4063-850f-69e7dbc7f1df
stats = combine(tips2, :total => sum, :tip => mean, :tip_pct => maximum)

# ╔═╡ f2cfa3f2-1cc5-4a25-900e-bab00f8e34fa
typeof(stats)

# ╔═╡ 3ee64a2e-5c6b-4e4e-af68-d87c33b5af8c
combine(tips2, :total => sum => :revenue, :tip => :perks)

# ╔═╡ Cell order:
# ╟─02e5ff12-ec4a-11f0-bfef-31026ecf2b5d
# ╠═c0ed7bdb-4d71-4e4a-8b51-74eaf90f4de5
# ╠═27827423-d9ff-43ce-833d-f04a00bc1555
# ╠═50bfd5ae-3bac-43dd-acef-ed91a6cb724d
# ╠═95fc3b4c-1bb8-4d19-b9f7-d81cc32d81ac
# ╟─b96601cb-db6a-4d1d-89b4-48f6329b84b4
# ╟─4c8b50e0-9c3a-4fe8-abbe-598bf02053e8
# ╠═b0fe2c66-d614-4a2c-904e-7105d60cc0ce
# ╠═b5a051a6-51dd-4ff6-b203-ef0c9ce83955
# ╠═352b2de2-64eb-4e21-8277-629023581c78
# ╠═badb5e9d-85c8-4339-bab4-68d6e9606f9a
# ╠═16fe15e1-9d97-48aa-ac41-3567427fea35
# ╠═74334d10-9f05-4489-866c-b4cfbdbde455
# ╟─ad15f49d-883f-49ca-9825-28fc8617424a
# ╠═7c50191e-a59d-421d-9203-bf5e221b543d
# ╟─7415a1bd-ca0d-4f62-83f0-86cf775104a4
# ╠═bfec98c0-4da3-44a6-bfda-135001a64aa1
# ╠═b44e70b1-2cdc-45c3-b4ee-dd7a1b4319cc
# ╠═bce6d214-9ced-419f-83a3-51b34d6ebbd3
# ╠═a376f5a8-3a13-4c38-a68f-57ca2ce439aa
# ╠═86003f27-27e5-40aa-9e9c-6051941e6d69
# ╠═ef73e6d9-7af3-483d-b479-7903234a22d1
# ╠═6420f348-c8f6-45b6-a48e-88ee037e4424
# ╠═1b9c36c4-b1b9-44db-8452-7a9c3a84cc22
# ╠═3d0af170-e791-482c-a40f-2b5f420a2f48
# ╟─9e7bf42e-c55c-4292-a433-8c8791a8e3a8
# ╠═a277e4ff-08c7-4c4f-bda7-bf3d31dd16d3
# ╠═cd130d0b-2a73-4a7a-a221-82b84b4d5e31
# ╟─e517e969-5bf2-4662-b264-7d76c567c0fb
# ╠═9ffb9fa6-619e-4d04-b102-1866dc9f976e
# ╠═f113442c-1ed5-401f-8191-9a6abf39c07e
# ╠═8ebbf25b-a70a-4ecb-ae28-eed6c5c5a7d8
# ╟─c2aab20d-76bf-40df-a4a2-ba5620b551f6
# ╠═3c20cc7c-f49c-49aa-a954-7925902dfb31
# ╠═679d59bc-726c-446e-a054-cecb77b07cc4
# ╠═2beba575-c978-4529-909c-32b87297dcd9
# ╟─16548e74-e435-4f8d-ad33-c8afdef8c408
# ╠═f2836d3c-a2ff-485b-922f-7a8136c7c29e
# ╠═866108f0-4de5-4063-850f-69e7dbc7f1df
# ╠═f2cfa3f2-1cc5-4a25-900e-bab00f8e34fa
# ╠═3ee64a2e-5c6b-4e4e-af68-d87c33b5af8c
