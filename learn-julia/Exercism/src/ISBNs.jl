module ISBNs

export ISBN

_select(pred) = payload -> filter(pred, payload)

struct ISBN
    value::String

    function ISBN(isbn::String)
        _filter = _select(!=('-'))
        reverse_isbn = (reverse ∘ _filter ∘ lowercase ∘ strip)(isbn)
        length(reverse_isbn) == 10 || throw(DomainError("must have exactly 10 digits!"))
        (isdigit(reverse_isbn[1]) || reverse_isbn[1] == 'x') || throw(DomainError("invalid check digit!"))
        initval = reverse_isbn[1] == 'x' ? 10 : parse(Int, reverse_isbn[1])
        hsh = reduce(enumerate(reverse_isbn[2:end]), init=initval) do acc, (idx, val)
            try
                digit = parse(Int, val)
                acc + ((idx + 1) * digit)
            catch err
                if err isa ArgumentError
                    rethrow(DomainError("$val is not a valid digit!"))
                else
                    rethrow()
                end
            end
        end
        hsh % 11 == 0 || throw(DomainError("verification check for hash value $hsh failed!"))
        new(isbn)
    end
end

function Base.:(==)(isbn1::ISBN, isbn2::ISBN)
    _filter = _select(!=('-'))  
    (_filter ∘ lowercase ∘ strip)(isbn1.value) == (_filter ∘ lowercase ∘ strip)(isbn2.value)
end

end