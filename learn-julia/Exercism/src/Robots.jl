module Robots

export Robot, robotname, robotreset!

const used_names = Set{String}()
push!(used_names, "")

function gen_name()
	name = ""
	while name ∈ used_names
		name = join([
			Char(64 + rand(1:26)), 
			Char(64 + rand(1:26)), 
			rand(0:9), 
			rand(0:9), 
			rand(0:9)
		])
	end
	push!(used_names, name)
	name
end

mutable struct Robot
	name::String

	Robot() = new(gen_name())
end

robotname(robot::Robot) = robot.name

function robotreset!(robot::Robot)
	robot.name = gen_name()
end

end