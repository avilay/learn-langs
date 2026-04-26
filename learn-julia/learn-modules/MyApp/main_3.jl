import MyApp
# Import only gets the package itself in my namespace, not any of the functions it exports. This
# means that I'll have to qualify the function name as MyApp.<func> everywhere.

function (@main)(args)
    filepath = joinpath(@__DIR__, "duke_of_york.txt")

    # Using FileUtils (which internally uses StringUtils)
    stats = MyApp.filestats(filepath)
    println("File stats: $(stats.words) words, $(stats.chars) chars")

    # Using StringUtils directly
    text = "Julia is fun"
    println("'$text' has $(MyApp.countwords(text)) words")

    # Using Stats
    numbers = [10, 20, 30, 40]
    println("Numbers $numbers: total=$(MyApp.total(numbers)), average=$(MyApp.average(numbers))")
end
