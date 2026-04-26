module Exercism

include("TwoBucket.jl")
include("BinarySearchTrees.jl")
include("ISBNs.jl")
include("TicTacToe.jl")
include("Robots.jl")

using .TwoBucket
using .BinarySearchTrees
using .ISBNs
using .TicTacToe
using .Robots

export twobucket, BinarySearchTree, nodedata, rightnode, leftnode, ISBN, gamestate, Robot, robotname, robotreset!


export count_nucleotides
function count_nucleotides(strand)
    invalid = filter(protien -> protien ∉ ('A', 'C', 'G', 'T'), strand)
    length(invalid) > 0 && throw(DomainError("Cannot have $invalid protiens!"))
    Dict(protien => count(==(protien), strand) for protien in "ACGT")
end


export scrabble_score
score = Dict(
    'A' => 1, 
    'E' => 1,
    'I' => 1,
    'O' => 1,
    'U' => 1,
    'L' => 1,
    'N' => 1,
    'R' => 1,
    'S' => 1,
    'T' => 1,
    'D' => 2,
    'G' => 2,
    'B' => 3,
    'C' => 3,
    'M' => 3, 
    'P' => 3,
    'F' => 4,
    'H' => 4, 
    'V' => 4, 
    'W' => 4, 
    'Y' => 4,
    'K' => 5,
    'J' => 8,
    'X' => 8,
    'Q' => 10,
    'Z' => 10
)
function scrabble_score(word)
    word = uppercase(word)
    reduce(+, [get(score, letter, 0) for letter in word], init=0)
end


export largest_product
function largest_product(str, span)
    length(str) < span && throw(ArgumentError("string cannot be less than span!"))
    span <= 0 && throw(ArgumentError("cannot have 0 span!"))
    prods = []
    for idx in span:lastindex(str)
        startidx = idx - span + 1
        window = str[startidx:idx]
        product = prod((parse(Int, digit) for digit in window))
        push!(prods, product)
    end
    maximum(prods)
end


export pythagorean_triplets
function pythagorean_triplets(n)
    triplets = []
    for a in 1:n
        for b in a+1:n
            c = n - a - b
            if b < c <= n && a^2 + b^2 == c^2
                push!(triplets, (a, b, c))
            end
        end
    end
    triplets
end

end # module Exercism
