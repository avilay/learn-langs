using Exercism
using Test

nucleotide_test_name = rpad("Nucleotide tests", 30, '.')
@testset "$nucleotide_test_name" begin
    @test count_nucleotides("") == Dict('A' => 0, 'C' => 0, 'G' => 0, 'T' => 0)
    @test count_nucleotides("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") == Dict('A' => 20, 'C' => 12, 'G' => 17, 'T' => 21)
    @test count_nucleotides("GGGGGGG") == Dict('A' => 0, 'C' => 0, 'G' => 7, 'T' => 0)
    @test_throws DomainError count_nucleotides("AGXXACT")
end

scrabble_test_name = rpad("Scrabble Score tests", 30, '.')
@testset "$scrabble_test_name" begin
    @test scrabble_score("cabbage") == 14
    @test scrabble_score("") == 0
    @test scrabble_score("pinata") == 8
    @test scrabble_score("piñata") == 7
end

largest_product_test_name = rpad("Largest Series Product", 30, '.')
@testset "$largest_product_test_name" begin
    @test largest_product("63915", 3) == 162
    @test largest_product("73167176531330624919225119674426574742355349194934", 6) == 23_520
    @test largest_product("0000", 2) == 0
    @test_throws ArgumentError largest_product("123", 4)
    @test_throws ArgumentError largest_product("123", -1)
    @test_throws ArgumentError largest_product("", 3)
end

isbn_verification_name = rpad("ISBN Verification", 30, '.')
@testset "$isbn_verification_name" begin
    @test ISBN("3-598-21508-8").value == "3-598-21508-8"
    @test ISBN("3-598-21507-X").value == "3-598-21507-X"
    @test_throws DomainError ISBN("")
    @test_throws DomainError ISBN("3-598-21507-Y")
    @test_throws DomainError ISBN("3-598-21508-9")
end

two_bucket_name = rpad("Two Bucket", 30, '.')
@testset "$two_bucket_name" begin
    @test twobucket(5, 3, 4, 1) == (6, 1, 3)
    @test_throws DomainError twobucket(6, 15, 5, 1)
    @test_throws DomainError twobucket(5, 7, 8, 1)
end

binary_search_tree_name = rpad("Binary Search Tree", 30, '.')
@testset "$binary_search_tree_name" begin
    @test BinarySearchTree() isa BinarySearchTree
    
    single = BinarySearchTree(4)
    @test nodedata(single) == 4
    @test isnothing(leftnode(single))
    @test isnothing(rightnode(single))

    ary = BinarySearchTree([4])
    @test nodedata(ary) == 4
    @test isnothing(leftnode(ary))
    @test isnothing(rightnode(ary))

    tree = BinarySearchTree([50, 30, 70, 20, 40, 60, 80])
    
    for val in [15, 25, 35, 45, 55, 65, 75, 85]
        @test val ∉ tree
    end

    for val in [20, 30, 40, 50, 60, 70, 80]
        @test val ∈ tree
    end

    @test sort(tree) == [20, 30, 40, 50, 60, 70, 80]

    dups = BinarySearchTree(4)
    push!(dups, 4)
    push!(dups, 5)
    @test nodedata(dups) == 4
    @test nodedata(leftnode(dups)) == 4
    @test nodedata(rightnode(dups)) == 5
end

triplets_name = rpad("Pythagorean Triplets", 30, '.')
@testset "$triplets_name" begin
    @test pythagorean_triplets(1000) == [(200, 375, 425)]
end

tictactoe_name = rpad("Tic Tac Toe Game State", 30, '.')
@testset "$tictactoe_name" begin
    @test gamestate(["OXO", " X ", " X "]) == "win"
    # @test gamestate(["O O", "XXX", " O "]) == "win"
    @test gamestate(["XOX", " XX", "OOO"]) == "win"
    @test gamestate(["XXO", "OXX", "XOO"]) == "draw"
    @test gamestate(["X  ", " XO", "OX "]) == "ongoing"
    @test_throws ErrorException gamestate(["XXX", "OOO", "   "])
    @test_throws ErrorException gamestate(["XX ", "   ", "   "])
end

robot_name = rpad("Robot Name", 30, '.')
@testset "$robot_name" begin
    r1 = Robot()
    @test occursin(r"^[A-Z]{2}[0-9]{3}$", robotname(r1))
    oldname = robotname(r1)
    robotreset!(r1)
    @test occursin(r"^[A-Z]{2}[0-9]{3}$", robotname(r1))
    @test oldname != robotname(r1)
end