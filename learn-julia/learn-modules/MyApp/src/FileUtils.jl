#=
FileUtils - File analysis utilities

Inclusion context:
  This module must be included from MyApp.jl AFTER StringUtils.jl.
  It expects StringUtils to be a sibling module (both children of MyApp).

Dependencies:
  - StringUtils: countwords, countchars
=#
module FileUtils

using ..StringUtils

export filestats

function filestats(filepath::String)
    content = read(filepath, String)
    (words=countwords(content), chars=countchars(content))
end

end
