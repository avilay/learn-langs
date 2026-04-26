using Exercism

function (@main)(args)
    r1 = Robot()
    r2 = Robot()
    println("Name of first robot is $(robotname(r1))")
    println("Name of second robot is $(robotname(r2))")
    robotreset!(r1)
    println("New name of first robot is $(robotname(r1))")
end