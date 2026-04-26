import MyApp: filestats, countwords
# I am only importing filestats and countwords, but this time they are available directly in my
# namespace. The pkg itself is not. This means that I cannot use qualified functions and I cannot
# any of the other functions that are exported by this module.

function (@main)(args)
    filepath = joinpath(@__DIR__, "duke_of_york.txt")

    # Using FileUtils (which internally uses StringUtils)
    stats = filestats(filepath)
    println("File stats: $(stats.words) words, $(stats.chars) chars")

    # Using StringUtils directly
    text = "Julia is fun"
    println("'$text' has $(countwords(text)) words")

    # Using Stats
    # numbers = [10, 20, 30, 40]
    # println("Numbers $numbers: total=$(MyApp.total(numbers)), average=$(MyApp.average(numbers))")
end
