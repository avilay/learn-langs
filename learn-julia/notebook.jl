### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 6e261e0b-02c4-4f52-b4d5-8f2960c99492
A = reshape(1:35, 5, 7)

# ╔═╡ 5b9290f0-4cb0-49f3-83ff-d793abb0f7cc
A[2, 4]

# ╔═╡ 17a5e9ab-7a56-4846-b98a-2968d6b256e4
B = reshape(1:3*4*2*3, 3, 4, 2, 3)

# ╔═╡ a3f54e02-96be-4648-b264-e349f2cee8cb
a = A[2, 2:4]

# ╔═╡ 7ed68ef3-3a94-440a-b71a-c4f472c32f83
print(a)

# ╔═╡ 7366500e-a66b-43bd-a908-ffabd6fce31d


# ╔═╡ 49d3e8fe-c4ad-4833-b0c8-6bb36e00cf48
A[begin:2:end, 2:4]

# ╔═╡ 08d71ab0-f82a-42f5-837a-a14e7f99df77
for d=9:-1:2
	@show d
	if 11 % d == 0
		print("Not prime")
		break
	end
end

# ╔═╡ 71da086b-b900-44e4-871b-3fd092d205b9


# ╔═╡ b69a2a36-6f34-4bd6-9fda-59188e4fd878


# ╔═╡ b7cfca35-b97e-4711-842d-4f11a09ab6af
function isprime(x)
	for d = x-1:-1:2
		if x % d == 0
			return false
		end
	end
	return true
end

# ╔═╡ 81f60620-d773-443d-813f-83fc10edc754
isprime(13)

# ╔═╡ 8b9bb71b-a8c2-43e8-a620-677472a0b644
isprime(25)

# ╔═╡ ab78160d-1c95-49e7-ba3e-b40b66119fbc
R = rand(1:100, 3, 3)

# ╔═╡ 395b8d8d-baea-4426-83e2-adb531b9366e
isprime.(R)

# ╔═╡ f90f0c9e-7e27-4652-aed8-8b8e73f24518


# ╔═╡ 61e165be-06e0-4c37-b363-a7237a6100bb


# ╔═╡ 3b9fe14e-c372-4318-b0a8-319657195551


# ╔═╡ 1506a085-0bf3-48ab-8d4e-82632b1221b8


# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.12.1"
manifest_format = "2.0"
project_hash = "71853c6197a6a7f222db0f1978c7cb232b87c5ee"

[deps]
"""

# ╔═╡ Cell order:
# ╠═6e261e0b-02c4-4f52-b4d5-8f2960c99492
# ╠═5b9290f0-4cb0-49f3-83ff-d793abb0f7cc
# ╠═17a5e9ab-7a56-4846-b98a-2968d6b256e4
# ╠═a3f54e02-96be-4648-b264-e349f2cee8cb
# ╠═7ed68ef3-3a94-440a-b71a-c4f472c32f83
# ╠═7366500e-a66b-43bd-a908-ffabd6fce31d
# ╠═49d3e8fe-c4ad-4833-b0c8-6bb36e00cf48
# ╠═08d71ab0-f82a-42f5-837a-a14e7f99df77
# ╠═71da086b-b900-44e4-871b-3fd092d205b9
# ╠═b69a2a36-6f34-4bd6-9fda-59188e4fd878
# ╠═b7cfca35-b97e-4711-842d-4f11a09ab6af
# ╠═81f60620-d773-443d-813f-83fc10edc754
# ╠═8b9bb71b-a8c2-43e8-a620-677472a0b644
# ╠═ab78160d-1c95-49e7-ba3e-b40b66119fbc
# ╠═395b8d8d-baea-4426-83e2-adb531b9366e
# ╠═f90f0c9e-7e27-4652-aed8-8b8e73f24518
# ╠═61e165be-06e0-4c37-b363-a7237a6100bb
# ╠═3b9fe14e-c372-4318-b0a8-319657195551
# ╠═1506a085-0bf3-48ab-8d4e-82632b1221b8
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
