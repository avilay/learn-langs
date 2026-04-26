using MyApp
# All the functions exported by MyApp along with MyApp itself are now in my namespace. I don't have 
# to qualify functions with the pkg name, I can use them directly. But because I also have the pkg
# itself in my namespace, I **can** qualify the functions with MyApp.<func> if I want for clarity.
# See the use of MyApp.average below.

function (@main)(args)
    filepath = joinpath(@__DIR__, "duke_of_york.txt")

    # Using FileUtils (which internally uses StringUtils)
    stats = filestats(filepath)
    println("File stats: $(stats.words) words, $(stats.chars) chars")

    # Using StringUtils directly
    text = "Julia is fun"
    println("'$text' has $(countwords(text)) words")

    # Using Stats
    numbers = [10, 20, 30, 40]
    println("Numbers $numbers: total=$(total(numbers)), average=$(MyApp.average(numbers))")
end
