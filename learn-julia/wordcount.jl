function (@main)(args)
    content = read(joinpath(@__DIR__, "duke_of_york.txt"), String)
    words = split(content)
    println("Word count: $(length(words))")
end
