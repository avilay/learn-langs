module RobotsSim

const NORTH = 0
const EAST = 1
const SOUTH = 2
const WEST = 3

struct Point
    x::Int
    y::Int
end

Point(xy::Tuple{Int,Int}) = Point(xy[1], xy[2])

Base.convert(::Type{Point}, xy::Tuple{Int,Int}) = Point(xy)

mutable struct Robot
    pos::Point
    dir::Int
end

position(robot::Robot) = robot.pos

heading(robot::Robot) = robot.dir

function turn_right!(robot)
	robot.dir = (robot.dir+1) % 4
    robot
end

function turn_left!(robot)
	robot.dir = (robot.dir + 3) % 4
    robot
end

function advance!(robot)
	if robot.dir == NORTH
		robot.pos = Point(robot.pos.x, robot.pos.y + 1)
	elseif robot.dir == EAST
		robot.pos = Point(robot.pos.x + 1, robot.pos.y)
	elseif robot.dir == WEST
		robot.pos = Point(robot.pos.x - 1, robot.pos.y)
	else  # robot.dir == south
		robot.pos = Point(robot.pos.x, robot.pos.y - 1)
	end
    robot
end

function move!(robot::Robot, cmds::String)
	for cmd in cmds
        if cmd == 'R'
            turn_right!(robot)
        elseif cmd == 'L'
            turn_left!(robot)
        elseif cmd == 'A'
            advance!(robot)
        else
            error("unknown command $cmd")
        end
    end
    robot
end

end