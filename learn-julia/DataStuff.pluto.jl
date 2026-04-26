### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ ca17585a-eba1-11f0-971b-a5f3505ef09a
using Pkg

# ╔═╡ f57b68be-553f-414b-9ac9-e908f941de92
Pkg.activate("ai"; shared=true)

# ╔═╡ 5beebb10-970b-46d4-a7bc-6c3c768edc30
using DataFrames

# ╔═╡ 73528b10-728b-4a30-b16b-d02d3a869c3f
using CSV

# ╔═╡ cdd62849-38ce-40b8-aa6c-599239c5c933
using Statistics: mean

# ╔═╡ ce925d66-2c86-42ee-816e-92c3f4f24ae3
md"""
## Constructors

Different ways of creating a dataframe -
  * Use kwargs
  * Use pairs
  * Use dicts
  * Use named tuples (with dictcolumntable for missing values)
"""

# ╔═╡ 4fde745d-b152-4b6a-b57f-61d7ce50424c
DataFrame(
	flavor=["Chocoloate Chip", "Snicker Doodle"], 
	calories=[200, 220], 
	desert=true
)

# ╔═╡ a3bb719e-7c60-4a72-9e2b-213fdf143dff
DataFrame(
	:flavor => ["Chocolate Chip", "Snicker Doodle"], 
	:calories => [200, 220]
)

# ╔═╡ 959ef3dc-cfb1-4320-a2d5-e17179c963fb
cookies = Dict(
	"flavor" => ["Chocolate Chip", "Snicker Doodle"],
	"calories" => [200, 220]
)

# ╔═╡ 35176e26-e343-45bd-a46f-54be8a1af468
DataFrame(cookies)

# ╔═╡ b2ae1b2b-fafa-4276-9983-43759d39e6ea
shapes = [
	(type="circle", radius=10),
	(type="square", side=20)
]

# ╔═╡ 1d89e660-d3d4-4ac4-8b67-3cf6820d1fda
# This will error out because all the fields are not present in all the elements
DataFrame(shapes)

# ╔═╡ c44afd64-f5a4-47ec-98d3-77e6422a2995
# This will add `missing` values where needed
DataFrame(Tables.dictcolumntable(shapes))

# ╔═╡ 73cc349f-bf98-4932-b02c-b60d94e710ce
md"""
## Loading Data

I can read a CSV file into any table "sink", the `DataFrame` constructor is one such sink. 

I'll notice that DataFrame uses special String objects like String3, these are similar to varchar(3) and are more efficient than a plain old String.
"""

# ╔═╡ 794b6c7e-26e0-439c-b54e-45bf12891926
tips_orig = CSV.read("tips.csv", DataFrame)

# ╔═╡ e0b55e75-0d67-4019-9cb9-5e4ea3ada1c0
md"""
## Metadata

  * `names(df[, Type])` returns the colnames (with a specified data type) as strings
  * `propertynames(df)` returns the colnames as symbols, no way to specify the eltype
  * `size(df[, dim])` returns the shape `(nrows, ncols)`
  * `nrow(df)`
  * `ncol(df)`
  * `describe(df[, ;cols=<range of cols>])` reports the default stats
"""

# ╔═╡ ede08cb0-2118-4577-a928-c45cadc406ba


# ╔═╡ 509fef6c-7802-4aca-b931-b7289a5542ab
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
"""

# ╔═╡ 8450b9e5-d3f8-4241-bd48-6a09bb2f8a9b
colname = "tip"

# ╔═╡ 29301c5f-5d51-4f2e-9f2f-cb6bfb04feec
colname2 = :sex

# ╔═╡ ca65628b-9c1d-45ba-9b0f-dc4cd6ca413f
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

# ╔═╡ db8ded2e-4c70-42f6-a047-5a83c77aa202
#=╠═╡
typeof(tips)
  ╠═╡ =#

# ╔═╡ fec899ee-9c9a-4294-9a83-8953f263cdfa
#=╠═╡
# Gets all the columns
names(tips)
  ╠═╡ =#

# ╔═╡ 7445a92c-6acc-4def-8b84-088d24f5b85c
#=╠═╡
# Gets only the String columns
names(tips, AbstractString)
  ╠═╡ =#

# ╔═╡ 18f93b5a-61e9-4730-9d24-28de9354e181
#=╠═╡
propertynames(tips)
  ╠═╡ =#

# ╔═╡ af4c3a35-a3d8-4169-9fc9-ef76280b5244
#=╠═╡
size(tips)
  ╠═╡ =#

# ╔═╡ 3e3e612d-150d-47e0-91d9-21c9041f144f
#=╠═╡
describe(tips)
  ╠═╡ =#

# ╔═╡ acd06856-cbf3-45be-94c7-2fe2647b5fda
#=╠═╡
tips.total_bill
  ╠═╡ =#

# ╔═╡ 07413877-d33f-45d8-8ffe-8f6fa8b33762
#=╠═╡
tips[!, colname]
  ╠═╡ =#

# ╔═╡ e63c0e8f-26db-4d52-93f4-11ac3735daba
#=╠═╡
c2 = tips[!, colname2]
  ╠═╡ =#

# ╔═╡ c938c9b7-9064-417f-86c1-8e7f3876b25b
#=╠═╡
typeof(c2) <: AbstractArray
  ╠═╡ =#

# ╔═╡ 6494195a-5ca3-4986-b248-cc99bb511182
#=╠═╡
# The returned colname is **not** a copy, changing it will change the dataframe
c2[1] = "NB"
  ╠═╡ =#

# ╔═╡ 92588889-d881-4f77-9272-55bbf7ec205c
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ 02a2a447-5579-4f34-9250-43288bbc74ee
#=╠═╡
# The returned column is a copy, changing it will not change the df
c1 = tips[:, colname2]
  ╠═╡ =#

# ╔═╡ 0ebad347-32f2-46b7-9580-4759a2661360
#=╠═╡
c1[1] = "Female"
  ╠═╡ =#

# ╔═╡ 62fb1361-9d30-4cf6-9532-7e8cb8e0a423
#=╠═╡
c1
  ╠═╡ =#

# ╔═╡ 9105ff0d-1034-43ed-9fca-9c19494101ae
#=╠═╡
# df still has the first row sex as NB
tips
  ╠═╡ =#

# ╔═╡ 991e271a-4029-4676-8134-52aa3c4b8706
#=╠═╡
r1 = first(tips, 3)
  ╠═╡ =#

# ╔═╡ 330b766a-dd4e-4e40-9a86-636647aa5b86
#=╠═╡
typeof(r1)
  ╠═╡ =#

# ╔═╡ 7055035c-c383-46dd-89dc-7e595a430098
#=╠═╡
first(tips)
  ╠═╡ =#

# ╔═╡ 99e5f991-42b3-4970-b870-6072eefbb047
#=╠═╡
r2 = tips[1:4, :]
  ╠═╡ =#

# ╔═╡ 389e4ae4-f5a5-4877-9b63-9fce6a018192
#=╠═╡
typeof(r2)
  ╠═╡ =#

# ╔═╡ f3a070cd-360a-4f78-8d3d-b522ef5dcb8d
#=╠═╡
r3 = tips[1, :]
  ╠═╡ =#

# ╔═╡ ea2ecbec-1a18-4ded-8849-9be7e44c5366
#=╠═╡
r3.sex
  ╠═╡ =#

# ╔═╡ 85552bdb-64fd-4286-946f-5adbaae73f68
#=╠═╡
# This gives the underlying data, so changing it will change the df
r3.sex = "Female"
  ╠═╡ =#

# ╔═╡ e968bd4e-1353-45cd-897e-543f910cd80c
#=╠═╡
r3 === first(tips)
  ╠═╡ =#

# ╔═╡ be3d313f-4548-4aae-8bc9-46b1cb0f7ff2
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ e34e3c10-41b2-40d9-bf45-10524232869a
#=╠═╡
mean(tips.tip)
  ╠═╡ =#

# ╔═╡ c97cd44f-0eb7-459d-a91c-2137441a8d80
#=╠═╡
tips[tips.tip .> 3, :]
  ╠═╡ =#

# ╔═╡ 521dc2b1-beed-440d-b695-c8d32fd0c0ea
#=╠═╡
tips[(6 .< tips.tip .< 9) .& (tips.sex .== "Male"), :]
  ╠═╡ =#

# ╔═╡ 56af0049-f127-4a99-81cd-317efdc0acfd
#=╠═╡
tips[in(["Thur", "Fri"]).(tips.day), :]
  ╠═╡ =#

# ╔═╡ 7c2af536-ae29-40fd-bcc5-8539fbfed298
#=╠═╡
unique(tips.day)
  ╠═╡ =#

# ╔═╡ 4586d967-1c01-472e-a7a2-4e6d1133818a
#=╠═╡
@view tips[end:-1:end-10, [1, 3]]
  ╠═╡ =#

# ╔═╡ 2b0e45ec-43e4-4b10-acf8-37e03a54c362
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ c09f5d6c-5a80-4a2a-9477-2dc250a52fd3
#=╠═╡
nonbinary = fill("N.B", nrow(tips))
  ╠═╡ =#

# ╔═╡ c4c91f43-b3e6-4ba4-a1e0-9058b06c1388
#=╠═╡
# This will mutate the df!
tips[:, :sex] = nonbinary
  ╠═╡ =#

# ╔═╡ aeea997d-a859-49f9-843e-0addf499a276
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ b212b9a5-d22b-4c3d-9869-51a34968150b
#=╠═╡
# The values are copied from the nonbinary vector into the df
tips.sex === nonbinary
  ╠═╡ =#

# ╔═╡ 1f9775c1-dd92-46b1-8b8c-337d5675bdd1
#=╠═╡
sundays = fill("Sunday", nrow(tips))
  ╠═╡ =#

# ╔═╡ a1754868-e281-4721-b504-df2339340ce6
#=╠═╡
# This will also mutate the df but it is not so surprising because it returns the
# the actual data
tips.day = sundays
  ╠═╡ =#

# ╔═╡ 0cd7cfdd-fb22-4400-ac70-79f967da41bd
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ c7168cfb-4d71-461b-a451-a21588061d29
#=╠═╡
# However, this did not copy the sundays vector, the df col is simply pointing to it!
tips.day === sundays
  ╠═╡ =#

# ╔═╡ c9a6d830-16ec-441e-afb0-9848585e9912
#=╠═╡
# I can change the df by changing this vector!
sundays[1] = "MONDAY"
  ╠═╡ =#

# ╔═╡ fa0cd38e-05b9-466e-877f-46897dc028aa
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ 9c69e70c-fc8a-4192-9954-44081e0780ec
#=╠═╡
r4 = first(tips)
  ╠═╡ =#

# ╔═╡ ab0d0e0c-000c-41b1-a6c1-0cc8f4ba4977
#=╠═╡
r4.day = "Sunday"
  ╠═╡ =#

# ╔═╡ 008bedb0-6528-4ef2-9313-fffe83279556
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ 48d37d6b-2b6d-4d3c-ae02-df2ea4bd39c9
#=╠═╡
# No need to use fill every time, I can just use brodcast
tips[1:4, :day] .= "MONDAY"
  ╠═╡ =#

# ╔═╡ 10c73963-ac17-453e-85ec-3c175645a4c3
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ bccf18df-2000-41f4-89ca-383a43ed5c52
#=╠═╡
rows = rand(1:nrow(tips), 10)
  ╠═╡ =#

# ╔═╡ 9e4b88d6-b63e-41ef-975d-fa2628851b91
#=╠═╡
tips_small = tips_orig[rows, :]
  ╠═╡ =#

# ╔═╡ 92cf2f98-9950-485d-9b9c-1d4d8bff030a
#=╠═╡
tips_small.city = rand(["Seattle", "San Francisco"], 10)
  ╠═╡ =#

# ╔═╡ 5de88828-3d47-4d07-b2e4-b66f75c3c248
#=╠═╡
tips_small
  ╠═╡ =#

# ╔═╡ c0c52bec-6214-45b3-97d0-e3ad4aa74e91
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

# ╔═╡ a110b251-f45e-4973-80bf-874d6d421432
df1 = DataFrame(
	:x => [2, 54, 70, 92, 77, missing, missing, 97, missing, 81],
	:y => ["down", "down", "bottom", "down", "top", "top", "bottom", "strange", "charm", "charm"]
)

# ╔═╡ 6195bfcf-b50d-4b1c-95a1-f0c3391578c6
md"""
In the examples below, when filtering `:y` column, index masks are much more clear and concise. But when filtering `:x` column, using `subet` is lot less clunkier than index masks because of missing values.
"""

# ╔═╡ 9dbfab6e-c9a7-4c33-8864-8489273660ff
strange_or_charm = in(["strange", "charm"])

# ╔═╡ c41b17df-6d26-44ad-a2c3-38cad2eb5ad1
subset(df1, :y => yvals -> strange_or_charm.(yvals))

# ╔═╡ 564762ee-29f6-48b2-a4bd-567da4693300
df1[strange_or_charm.(df1.y), :]

# ╔═╡ cbc718e7-53a8-458d-8f98-83e886b21b02
try
	df1[iseven.(df1.x), :]
catch err
	@assert err isa ArgumentError
	print(err)
end

# ╔═╡ 451270cc-1cd7-4227-a151-55844f9d0901
df1[(df1.x .!== missing) .& (iseven.(df1.x)), :]

# ╔═╡ 89ec4f9a-b05c-4101-be84-ea9f4a8a08f0
# Of I can impute the missing values with something else using `coalesce`
df1[coalesce.(iseven.(df1.x), false), :]

# ╔═╡ e128eb10-e7b7-4050-9c79-33b415074b28
md"""
In this case skipping or coalescing result in the same result, but there are cases where for example I am computing averages, I might need to skip rather than coalesce the missing values with 0 (say).

If I want to skip the missing values, then using `subset` is a lot less clunkier.
"""

# ╔═╡ 5032e738-a136-4aef-9acd-be22f34eb585
subset(df1, :x => vals -> iseven.(vals), skipmissing=true)

# ╔═╡ 70cd7a8d-baf2-4613-84b4-005133525f24


# ╔═╡ 2f15ccf5-9afc-4882-bade-97bcb1c36214
#=╠═╡
select(tips, :total_bill, :tip)
  ╠═╡ =#

# ╔═╡ 01589adc-44e7-4379-b0ee-9d02ce11cb53
#=╠═╡
tips[:, [:total_bill, :tip]]
  ╠═╡ =#

# ╔═╡ 0bf02f29-3730-4a24-981d-4ffcbd640eb4
# Transforming and renaming a column at the same time
select(tips2, 
	   :total_bill, 
	   :tip, 
	   :gender, 
	   :smoker => (vals -> vals .== "Yes") => :is_smoker, 
	   :day, 
	   :meal_type, 
	   :n_people
)

# ╔═╡ 621bb956-eb35-4b96-a46d-4ebf817f4b2e
# ╠═╡ disabled = true
#=╠═╡
# Just transforming the single column can be done easily with indexing
tips.smoker = tips.smoker .== "Yes"
  ╠═╡ =#

# ╔═╡ 0b01c25a-6c1f-4608-87d4-26f902a824d8
#=╠═╡
tips
  ╠═╡ =#

# ╔═╡ f23d683b-9cd1-48c9-b3b1-04d761fe7f38


# ╔═╡ 945fed01-69da-4079-9f3a-e0ff3fc630eb


# ╔═╡ 7f8f7802-60eb-4e15-a2e7-2e52ca1ffb94


# ╔═╡ 901ba90c-1bcb-45ff-8fab-4b419e4890be


# ╔═╡ 40495b21-182b-4bc4-8e8c-225932e8a46f


# ╔═╡ c4784dac-5cbb-49d4-be46-9ee518a4e598


# ╔═╡ 8363280f-af4f-4ba3-88c7-af0f0eb3dc1e


# ╔═╡ b60dacc8-dce9-4698-908f-302328337f43
# ╠═╡ disabled = true
#=╠═╡
# Renaming a few cols, don't know how I'd do this with indexing
tips2 = select(tips, :total_bill, :tip, :sex => :gender, :smoker, :day, :time => :meal_type, :size=>:n_people)
  ╠═╡ =#

# ╔═╡ 4e5d19d6-8962-4878-b81c-a4cb0bf1bbac
# ╠═╡ disabled = true
#=╠═╡
tips = copy(tips_orig)
  ╠═╡ =#

# ╔═╡ c9cf824c-7965-4946-92ac-7311d14fee0b
# ╠═╡ disabled = true
#=╠═╡
tips = copy(tips_orig)
  ╠═╡ =#

# ╔═╡ 525bc47c-34c1-461c-b414-2ebff51fd34e
tips2 = copy(tips_orig)

# ╔═╡ Cell order:
# ╠═ca17585a-eba1-11f0-971b-a5f3505ef09a
# ╠═f57b68be-553f-414b-9ac9-e908f941de92
# ╠═5beebb10-970b-46d4-a7bc-6c3c768edc30
# ╠═ce925d66-2c86-42ee-816e-92c3f4f24ae3
# ╠═4fde745d-b152-4b6a-b57f-61d7ce50424c
# ╠═a3bb719e-7c60-4a72-9e2b-213fdf143dff
# ╠═959ef3dc-cfb1-4320-a2d5-e17179c963fb
# ╠═35176e26-e343-45bd-a46f-54be8a1af468
# ╠═b2ae1b2b-fafa-4276-9983-43759d39e6ea
# ╠═1d89e660-d3d4-4ac4-8b67-3cf6820d1fda
# ╠═c44afd64-f5a4-47ec-98d3-77e6422a2995
# ╠═73cc349f-bf98-4932-b02c-b60d94e710ce
# ╠═73528b10-728b-4a30-b16b-d02d3a869c3f
# ╠═794b6c7e-26e0-439c-b54e-45bf12891926
# ╠═4e5d19d6-8962-4878-b81c-a4cb0bf1bbac
# ╠═db8ded2e-4c70-42f6-a047-5a83c77aa202
# ╠═e0b55e75-0d67-4019-9cb9-5e4ea3ada1c0
# ╠═fec899ee-9c9a-4294-9a83-8953f263cdfa
# ╠═7445a92c-6acc-4def-8b84-088d24f5b85c
# ╠═18f93b5a-61e9-4730-9d24-28de9354e181
# ╠═af4c3a35-a3d8-4169-9fc9-ef76280b5244
# ╠═3e3e612d-150d-47e0-91d9-21c9041f144f
# ╠═ede08cb0-2118-4577-a928-c45cadc406ba
# ╠═509fef6c-7802-4aca-b931-b7289a5542ab
# ╠═acd06856-cbf3-45be-94c7-2fe2647b5fda
# ╠═8450b9e5-d3f8-4241-bd48-6a09bb2f8a9b
# ╠═07413877-d33f-45d8-8ffe-8f6fa8b33762
# ╠═29301c5f-5d51-4f2e-9f2f-cb6bfb04feec
# ╠═e63c0e8f-26db-4d52-93f4-11ac3735daba
# ╠═c938c9b7-9064-417f-86c1-8e7f3876b25b
# ╠═6494195a-5ca3-4986-b248-cc99bb511182
# ╠═92588889-d881-4f77-9272-55bbf7ec205c
# ╠═02a2a447-5579-4f34-9250-43288bbc74ee
# ╠═0ebad347-32f2-46b7-9580-4759a2661360
# ╠═62fb1361-9d30-4cf6-9532-7e8cb8e0a423
# ╠═9105ff0d-1034-43ed-9fca-9c19494101ae
# ╠═991e271a-4029-4676-8134-52aa3c4b8706
# ╠═330b766a-dd4e-4e40-9a86-636647aa5b86
# ╠═7055035c-c383-46dd-89dc-7e595a430098
# ╠═99e5f991-42b3-4970-b870-6072eefbb047
# ╠═389e4ae4-f5a5-4877-9b63-9fce6a018192
# ╠═f3a070cd-360a-4f78-8d3d-b522ef5dcb8d
# ╠═e968bd4e-1353-45cd-897e-543f910cd80c
# ╠═ea2ecbec-1a18-4ded-8849-9be7e44c5366
# ╠═85552bdb-64fd-4286-946f-5adbaae73f68
# ╠═be3d313f-4548-4aae-8bc9-46b1cb0f7ff2
# ╠═cdd62849-38ce-40b8-aa6c-599239c5c933
# ╠═e34e3c10-41b2-40d9-bf45-10524232869a
# ╠═c97cd44f-0eb7-459d-a91c-2137441a8d80
# ╠═521dc2b1-beed-440d-b695-c8d32fd0c0ea
# ╠═56af0049-f127-4a99-81cd-317efdc0acfd
# ╠═7c2af536-ae29-40fd-bcc5-8539fbfed298
# ╠═4586d967-1c01-472e-a7a2-4e6d1133818a
# ╠═2b0e45ec-43e4-4b10-acf8-37e03a54c362
# ╠═ca65628b-9c1d-45ba-9b0f-dc4cd6ca413f
# ╠═c9cf824c-7965-4946-92ac-7311d14fee0b
# ╠═c09f5d6c-5a80-4a2a-9477-2dc250a52fd3
# ╠═c4c91f43-b3e6-4ba4-a1e0-9058b06c1388
# ╠═aeea997d-a859-49f9-843e-0addf499a276
# ╠═b212b9a5-d22b-4c3d-9869-51a34968150b
# ╠═1f9775c1-dd92-46b1-8b8c-337d5675bdd1
# ╠═a1754868-e281-4721-b504-df2339340ce6
# ╠═0cd7cfdd-fb22-4400-ac70-79f967da41bd
# ╠═c7168cfb-4d71-461b-a451-a21588061d29
# ╠═c9a6d830-16ec-441e-afb0-9848585e9912
# ╠═fa0cd38e-05b9-466e-877f-46897dc028aa
# ╠═9c69e70c-fc8a-4192-9954-44081e0780ec
# ╠═ab0d0e0c-000c-41b1-a6c1-0cc8f4ba4977
# ╠═008bedb0-6528-4ef2-9313-fffe83279556
# ╠═48d37d6b-2b6d-4d3c-ae02-df2ea4bd39c9
# ╠═10c73963-ac17-453e-85ec-3c175645a4c3
# ╠═bccf18df-2000-41f4-89ca-383a43ed5c52
# ╠═9e4b88d6-b63e-41ef-975d-fa2628851b91
# ╠═92cf2f98-9950-485d-9b9c-1d4d8bff030a
# ╠═5de88828-3d47-4d07-b2e4-b66f75c3c248
# ╠═c0c52bec-6214-45b3-97d0-e3ad4aa74e91
# ╠═a110b251-f45e-4973-80bf-874d6d421432
# ╠═6195bfcf-b50d-4b1c-95a1-f0c3391578c6
# ╠═9dbfab6e-c9a7-4c33-8864-8489273660ff
# ╠═c41b17df-6d26-44ad-a2c3-38cad2eb5ad1
# ╠═564762ee-29f6-48b2-a4bd-567da4693300
# ╠═cbc718e7-53a8-458d-8f98-83e886b21b02
# ╠═451270cc-1cd7-4227-a151-55844f9d0901
# ╠═89ec4f9a-b05c-4101-be84-ea9f4a8a08f0
# ╠═e128eb10-e7b7-4050-9c79-33b415074b28
# ╠═5032e738-a136-4aef-9acd-be22f34eb585
# ╠═70cd7a8d-baf2-4613-84b4-005133525f24
# ╠═525bc47c-34c1-461c-b414-2ebff51fd34e
# ╠═2f15ccf5-9afc-4882-bade-97bcb1c36214
# ╠═01589adc-44e7-4379-b0ee-9d02ce11cb53
# ╠═b60dacc8-dce9-4698-908f-302328337f43
# ╠═0bf02f29-3730-4a24-981d-4ffcbd640eb4
# ╠═621bb956-eb35-4b96-a46d-4ebf817f4b2e
# ╠═0b01c25a-6c1f-4608-87d4-26f902a824d8
# ╠═f23d683b-9cd1-48c9-b3b1-04d761fe7f38
# ╠═945fed01-69da-4079-9f3a-e0ff3fc630eb
# ╠═7f8f7802-60eb-4e15-a2e7-2e52ca1ffb94
# ╠═901ba90c-1bcb-45ff-8fab-4b419e4890be
# ╠═40495b21-182b-4bc4-8e8c-225932e8a46f
# ╠═c4784dac-5cbb-49d4-be46-9ee518a4e598
# ╠═8363280f-af4f-4ba3-88c7-af0f0eb3dc1e
