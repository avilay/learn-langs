module StringUtils

export countwords, countchars

countwords(text::String) = length(split(text))

countchars(text::String) = length(text)

end
