### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 9dc1ad94-ed67-11f0-8abd-31b9cd116d32
begin
	using Pkg
	Pkg.activate("ai"; shared=true)
end

# ╔═╡ cd04f9dd-6b84-4cd3-985f-2c68c1d2949d
using LinearAlgebra

# ╔═╡ 70e21dc2-bf9b-4604-983d-daf9379600c3
md"""
## Equation Solving

The `\` operator solves equations of the type ``A \mathbf x = \mathbf b`` or ``AX = B``. It will exploit any symmetries in ``A`` if available to factorize ``A`` and solve these equations. If ``A`` is rectangular, there can be exactly ``1``, ``0``, or ``\infty`` solutions. The `\` operator will give a minimum norm least squares solution.
"""

# ╔═╡ 34b902aa-292b-4c75-b943-89f30351c8e7
A = round.(rand(3, 3), digits=2)

# ╔═╡ 14f20ad4-6dfa-417b-8384-f48e90c4435d
b = round.(rand(3), digits=2)

# ╔═╡ 45ccac6b-812e-4e34-8b6d-f0a7a98ecadd
x = A\b

# ╔═╡ bbe676c5-55a5-4f6e-9270-bbc6d805b196
A * x

# ╔═╡ bcf3720a-524d-4698-99b8-08eede485990
A*x ≈ b

# ╔═╡ 17002e1f-88d0-4856-a2bc-25247dd5016a
norm(A*x - b)

# ╔═╡ b907a12c-0885-4f99-8ae7-4eb4d91e2053
md"""
## Factorization

This section is just running notes of factorization. As I go deeper into Linear Algebra, I'll keep adding to it.

The first level of understanding factorization is that given a matrix ``A``, it will be factored into a bunch of other matrices ``F_1, F_2, \cdots, F_n`` s.t. ``A = F_1 F_2 \cdots F_n``. 

But certain factorization algorithms will output a permutation matrix in addition to the factors. The permutation matrix is composed of a bunch of ``1``s and ``0``s. If I multiply any matrix by a permutation matrix, the original LHS matrix's columns are shuffled.

```math
A P = A_p
```

``A_p`` is a matrix with the same columns as ``A``, but with its columns shuffled.

> 💣 ``AP \neq PA`` so if I multiply the permutation matrix by any matrix, the output is not going to be a shuffled matrix.


The above simple factorization will not hold true. The actual factorization will be the factors producing not ``A``, but some function of ``P`` multiplied by ``A``.

The tutorials compare the norm of both sides to check for equality, which beats eyeballing the output, but I am wondering if that is a strict enough check. Won't there exist multiple matrices that happen to have the same norm?

The [`factorize()`](https://docs.julialang.org/en/v1/stdlib/LinearAlgebra/#LinearAlgebra.factorize) function will choose the right factorization technique based on the properties of the input matrix. Here is the partial look at what it chooses -

| Input | Technique |
--------|------------|
| General Rectangular | QR |
| General Square | LU |
| Symmetric | Bunch-Kaufman |

Any kind of symmetric matrix, whether it is real-valued symmetric, or complex valued Hermitian, or a positive definite matrix, in all cases `factorize()` will use Bunch-Kaufman. Earlier versions used Cholesky for positive definite matrices.
"""

# ╔═╡ 21be2c78-dfa6-4003-8e9e-19703ba86b7c
md"""
### General Rectangular Matrix

``M`` is a general rectangular matrix, so `factorize()` will use QR factorization. 

#### QR Factorization
In this case it actually uses pivoted QR factorization that produces a permutation matrix. The factorization is simple, in that the product of the factors is a simple permutation of the input matrix itself.

In the cells below, I try this out with both a wide as well as a tall matrix.

```math
QR = MP
```
"""

# ╔═╡ 72e0eed9-291b-473b-8924-1202e8042df5
M = round.(rand(3, 4), digits=2)

# ╔═╡ 73bad247-b7b4-4ec4-a0b1-59df0875f452
factors_M = factorize(M)

# ╔═╡ 6aa1e04b-203f-47e6-ba57-1b5352bc4d9c
factors_M.Q * factors_M.R

# ╔═╡ 73c96da2-2f57-4691-bfd1-ffb4f7cd6627
M * factors_M.P

# ╔═╡ f196a800-c7d7-40b1-be53-a09844edc988
factors_M.Q * factors_M.R ≈ M * factors_M.P

# ╔═╡ 1972b211-0a47-438a-bdec-e2034ab2bc3e
N = round.(rand(4, 3), digits=2)

# ╔═╡ cd3fe461-3832-47fb-987f-3228402698a8
factors_N = factorize(N)

# ╔═╡ af0192e8-2261-4318-bdaf-a6f52029d69b
factors_N.Q * factors_N.R ≈ N * factors_N.P

# ╔═╡ 88cb2619-071b-4c86-bdca-3c75d8e0885a
md"""
### General Square Matrix
``A`` is a general square matrix, so `factorize()` will use LU factorization.

#### LU Factorization
```math
A = LU
```

With the permutation matrix, the actual factorization will be -
```math
LU = PA
```

Note, that the RHS is not a shuffled form of the input matrix which would be ``AP``.
"""

# ╔═╡ f2c58a31-29ec-4de7-a2a6-72f9f6e136b1
factors_A = factorize(A)

# ╔═╡ aa84a29e-9418-4a13-823c-24a9a6ea406a
factors_A.L * factors_A.U

# ╔═╡ 57423c78-b650-41c4-8b11-e384de10114d
# P is a permutation matrix, it will shuffle the columns of whatever matrix it is
# multiplied with it.
factors_A.P

# ╔═╡ 4d4df8ef-9f24-4c3b-9a11-aeecc6bd4607
A

# ╔═╡ 8d65eb2d-b90c-42ab-8d62-18ed5721a12b
A * factors_A.P

# ╔═╡ 6dd169d7-84af-4318-a48e-a8240a7a3994
# However, the factors are PA, not the shuffled A matrix
factors_A.P * A

# ╔═╡ a984e20f-7a53-4e7c-aa9a-082f358a8405
norm(factors_A.L * factors_A.U) ≈ norm(A * factors_A.P)

# ╔═╡ f9a6d474-df97-4968-8ab0-436112cdc86f
# I can directly call LU factorization if I want.
lu(A)

# ╔═╡ ab725fa1-1a18-44aa-941f-790042e9ba53


# ╔═╡ 30367568-6e50-4d23-a738-93bc319071f1
md"""
### Symmetric Matrix
A symmetric matrix is a square matrix that is equal to its transpose. This means that 
  * Its elements will mirror each other across the diagonal. 
    - ``s_{ij} = s_{ji}``
  * ``SS^T = S^TS = I``

Calling `Symmetric(A)` will generate a symmetric matrix from any matrix ``A`` by taking its upper triangle and mirroring it. I can also ask it to use the lower triangular to mirror.
"""

# ╔═╡ 0376b5dc-b85c-4b8e-90b4-1318a666cc93
S = Symmetric(A)

# ╔═╡ c2e65602-bf57-402f-a9a2-18ddba4519c9
S.uplo

# ╔═╡ c1a4ef68-ddd2-44bb-9b5c-8f84ae63db8f
md"""
#### Bunch Kaufmann factorization

A symmetric matrix is best factorized using the Bunch Kaufman method into ``D``, ``U`` or ``L`` (depending on whether the upper or lower part of ``A`` was used in generating the symmetic matrix), and the permutation matrix ``P``.

```math
U D U^T = P S P^T
```

or re-arranging after some algebraic manipulation -

```math
P^T U D U^T P = S
```
"""

# ╔═╡ 7b437c4b-25a4-4f64-8529-8cec7701ba52
factors_S = factorize(S)

# ╔═╡ a6399e17-6e80-4b0e-a9da-67977868bf0b
factors_S.P

# ╔═╡ e31db4ed-dd33-460c-acba-1b38af7b1fad
S

# ╔═╡ e2e35d30-c861-454d-9de5-adf75ef85de2
S * factors_S.P

# ╔═╡ fe6a6dfd-4c69-48d8-8ff6-a4d0c0d18506
factors_S.P * S * factors_S.P'

# ╔═╡ b63923d8-0feb-44ae-b4c1-679c211410db
factors_S.U * factors_S.D * factors_S.U'

# ╔═╡ 177c4f0f-73f5-4e5e-a3b8-3d1e7fde9249
factors_S.P' * factors_S.U * factors_S.D * factors_S.U' * factors_S.P ≈ S

# ╔═╡ 61de10e7-f655-483b-9171-6dd0dc9fd8e1
md"""
### Hermitian Matrix

A Hermitian matrix is a square matrix of complex numbers that is equal to its conjugate transpose. This means that
  * Mirror elements across the diagnoal will be conjugate transposes of each other.
    - ``h_{ij} = h^*_{ji}``
  * ``H H^{\dagger} = H^{\dagger} H = I``
  * The diagonal entries ``h_{ii}`` are real because they need to be equal to their conjugates.

If I have a complex matrix ``C``, a Hermitian matrix can be generated by taking its upper (or lower) triangular values by calling `Hermitian(C)`.
"""

# ╔═╡ c0c72e68-e421-4f11-8edb-549a87ac5870
C = reshape(
	[Complex(xx, yy) for (xx, yy) in zip(rand(1:10, 9), rand(1:10, 9))], 
	3, 3
)

# ╔═╡ d215f42a-c2bf-4ba1-884d-f63d6a818e12
H = Hermitian(C)

# ╔═╡ 1000d057-47e6-4813-a3fc-16ee83755f31
H.uplo

# ╔═╡ e708379b-208a-4929-8c01-fd469a3c90db
md"""
`factorize()` will again choose the Bunch Kaufman method to factorize into ``P``, ``D``, ``U`` or ``L``, s.t. 
```math
H = P^T U D U^T P
```
"""

# ╔═╡ 9225b16a-c891-406d-9113-3c30d2aeb1c5
factors_H = factorize(H)

# ╔═╡ 728f00b2-4497-4dd4-bfe6-b68698e6a227
factors_H.P' * factors_H.U * factors_H.D * factors_H.U' * factors_H.P ≈ H

# ╔═╡ 1063e6d6-9dd3-41d7-9207-02b731ce66ef
md"""
### Positive Definite Matrix
A positive definite matrix is a **symmetric** or a **Hermitian** matrix 
  * Eigen values are strictly positive ``\lambda_k > 0``.
  * This implies that its determinant, which is the product of all its eigen values, is also strictly positive.
  * This in turn implies that this kind of matrix is invertible.

A positive semi-definite matrix is one whose eigen values can be 0 or positive ``\lambda_k \geq 0``. An easy way to generate a positive semi-definite matrix is to multiply a square matrix by its transpose (note, there is no way to multiply a rectangular matrix to its transpose, only a square matrix can be multiplied to its transpose.). If I want it to be positive definite, I can bump up the diagnoal by a small value.

I can check if a matrix is positive definite by calling the `isposdef()` function on it.
"""

# ╔═╡ c54efc74-2673-45c1-a7f4-917704d00768
B = A' * A + 0.01I

# ╔═╡ ff20ea98-4f88-4ed9-bdf8-d1970eb97ba7
isposdef(B)

# ╔═╡ 67b969fb-4c7e-4a25-8e1c-0cacadcfecb6
eigvals(B)

# ╔═╡ 88f8b119-991e-47ca-8abd-5ca297beda11
md"""
Even though it is possible to factorize positive definite matrices with the Cholesky technique, `factorize()` still uses the Bunch Kaufman.

```math
U D U^T = P^T B P 
```
"""

# ╔═╡ 76fc4793-c1c8-457d-8647-e40d66e38005
factors_B = factorize(B)

# ╔═╡ 8e608a80-4f78-4510-bbc8-a956ddc622f6
factors_B.U * factors_B.D * factors_B.U' ≈ factors_B.P' * B * factors_B.P

# ╔═╡ 3aef8c8c-575f-4185-a9c4-bb8fd1d2aa96
factors1_B = cholesky(B)

# ╔═╡ b3eb6032-3082-4e5f-aafd-9aa941e0f0cc
factors1_B.U' * factors1_B.U ≈ B

# ╔═╡ d9b171b0-eb64-4819-9525-61c34eab4c77
factors1_B.L * factors1_B.L' ≈ B

# ╔═╡ d01598dc-1165-4659-8a31-4af4b7e8fb30


# ╔═╡ Cell order:
# ╠═9dc1ad94-ed67-11f0-8abd-31b9cd116d32
# ╠═cd04f9dd-6b84-4cd3-985f-2c68c1d2949d
# ╟─70e21dc2-bf9b-4604-983d-daf9379600c3
# ╠═34b902aa-292b-4c75-b943-89f30351c8e7
# ╠═14f20ad4-6dfa-417b-8384-f48e90c4435d
# ╠═45ccac6b-812e-4e34-8b6d-f0a7a98ecadd
# ╠═bbe676c5-55a5-4f6e-9270-bbc6d805b196
# ╠═bcf3720a-524d-4698-99b8-08eede485990
# ╠═17002e1f-88d0-4856-a2bc-25247dd5016a
# ╟─b907a12c-0885-4f99-8ae7-4eb4d91e2053
# ╟─21be2c78-dfa6-4003-8e9e-19703ba86b7c
# ╠═72e0eed9-291b-473b-8924-1202e8042df5
# ╠═73bad247-b7b4-4ec4-a0b1-59df0875f452
# ╠═6aa1e04b-203f-47e6-ba57-1b5352bc4d9c
# ╠═73c96da2-2f57-4691-bfd1-ffb4f7cd6627
# ╠═f196a800-c7d7-40b1-be53-a09844edc988
# ╠═1972b211-0a47-438a-bdec-e2034ab2bc3e
# ╠═cd3fe461-3832-47fb-987f-3228402698a8
# ╠═af0192e8-2261-4318-bdaf-a6f52029d69b
# ╟─88cb2619-071b-4c86-bdca-3c75d8e0885a
# ╠═f2c58a31-29ec-4de7-a2a6-72f9f6e136b1
# ╠═aa84a29e-9418-4a13-823c-24a9a6ea406a
# ╠═57423c78-b650-41c4-8b11-e384de10114d
# ╠═4d4df8ef-9f24-4c3b-9a11-aeecc6bd4607
# ╠═8d65eb2d-b90c-42ab-8d62-18ed5721a12b
# ╠═6dd169d7-84af-4318-a48e-a8240a7a3994
# ╠═a984e20f-7a53-4e7c-aa9a-082f358a8405
# ╠═f9a6d474-df97-4968-8ab0-436112cdc86f
# ╠═ab725fa1-1a18-44aa-941f-790042e9ba53
# ╟─30367568-6e50-4d23-a738-93bc319071f1
# ╠═0376b5dc-b85c-4b8e-90b4-1318a666cc93
# ╠═c2e65602-bf57-402f-a9a2-18ddba4519c9
# ╟─c1a4ef68-ddd2-44bb-9b5c-8f84ae63db8f
# ╠═7b437c4b-25a4-4f64-8529-8cec7701ba52
# ╠═a6399e17-6e80-4b0e-a9da-67977868bf0b
# ╠═e31db4ed-dd33-460c-acba-1b38af7b1fad
# ╠═e2e35d30-c861-454d-9de5-adf75ef85de2
# ╠═fe6a6dfd-4c69-48d8-8ff6-a4d0c0d18506
# ╠═b63923d8-0feb-44ae-b4c1-679c211410db
# ╠═177c4f0f-73f5-4e5e-a3b8-3d1e7fde9249
# ╟─61de10e7-f655-483b-9171-6dd0dc9fd8e1
# ╠═c0c72e68-e421-4f11-8edb-549a87ac5870
# ╠═d215f42a-c2bf-4ba1-884d-f63d6a818e12
# ╠═1000d057-47e6-4813-a3fc-16ee83755f31
# ╟─e708379b-208a-4929-8c01-fd469a3c90db
# ╠═9225b16a-c891-406d-9113-3c30d2aeb1c5
# ╠═728f00b2-4497-4dd4-bfe6-b68698e6a227
# ╟─1063e6d6-9dd3-41d7-9207-02b731ce66ef
# ╠═c54efc74-2673-45c1-a7f4-917704d00768
# ╠═ff20ea98-4f88-4ed9-bdf8-d1970eb97ba7
# ╠═67b969fb-4c7e-4a25-8e1c-0cacadcfecb6
# ╟─88f8b119-991e-47ca-8abd-5ca297beda11
# ╠═76fc4793-c1c8-457d-8647-e40d66e38005
# ╠═8e608a80-4f78-4510-bbc8-a956ddc622f6
# ╠═3aef8c8c-575f-4185-a9c4-bb8fd1d2aa96
# ╠═b3eb6032-3082-4e5f-aafd-9aa941e0f0cc
# ╠═d9b171b0-eb64-4819-9525-61c34eab4c77
# ╠═d01598dc-1165-4659-8a31-4af4b7e8fb30
