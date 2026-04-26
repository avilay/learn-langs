module MyApp

include("utils/StringUtils.jl")
include("FileUtils.jl")
include("Stats.jl")

using .StringUtils
using .FileUtils
using .Stats

export countwords, countchars, filestats, average, total

end
