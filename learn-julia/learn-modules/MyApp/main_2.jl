using MyApp: filestats, countwords
# I only got filestats and countwords in my namespace. I did not get any of the other functions
# and nor did I get the package itself. This means that I can only use these two functions unqualified.
# If I do MyApp.countwords() I'll get an error.

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
    # println("Numbers $numbers: total=$(total(numbers)), average=$(MyApp.average(numbers))")
end
