module TwoBucket

export twobucket

function pour_1_2(caps, vols)
    newvol2 = min(caps[2], vols[1] + vols[2])
    newvol1 = vols[1] - (newvol2 - vols[2])
    (newvol1, newvol2)
end

function pour_2_1(caps, vols)
    newvol1 = min(caps[1], vols[1] + vols[2])
    newvol2 = vols[2] - (newvol1 - vols[1])
    (newvol1, newvol2)
end

# Doing a bfs instead of my usual backtracking with dfs because I want to
# find the least number of steps it takes to reach goal state.
function twobucket(capacity1, capacity2, goal_vol, start_bucket)
    errmsg = "$start_bucket is invalid, it can only be either 1 or 2!"
    (start_bucket == 1 || start_bucket == 2) || throw(DomainError(errmsg))

    # I should not end up in a state where the start bucket is empty and other
    # bucket is full
    bad_state = start_bucket == 1 ? (0, capacity2) : (capacity1, 0)
    
    # Fill the start bucket
    init_vols = start_bucket == 1 ? (capacity1, 0) : (0, capacity2)

    # Return the number of steps, bucket with the goal volume, and the other bucket's volume
    init_vols[1] == goal_vol && return (1, 1, 0)
    init_vols[2] == goal_vol && return (1, 2, 0)

    # Mark the processed node
    marked = Set()
    push!(marked, init_vols)
    push!(marked, bad_state)

    # Queue of the current node and the number of steps
    queue = [(init_vols, 1)]

    while !isempty(queue)
        # Dequeue the processed node
        vols, nsteps = popfirst!(queue)

        all_newvols = [
            pour_1_2((capacity1, capacity2), vols),  # Pour bucket 1 into bucket 2
            pour_2_1((capacity1, capacity2), vols), # Pour bucket 2 into bucket 1
            (0, vols[2]),  # Empty bucket 1
            (vols[1], 0),  # Empty bucket 2
            (capacity1, vols[2]),  # Fill bucket 1
            (vols[1], capacity2),  # Fill bucket 2
        ]

        # Process, mark, and enqueue all unmarked neighbors
        for newvols in all_newvols
            if newvols ∉ marked
                # Process
                newvols[1] == goal_vol && return (nsteps+1, 1, newvols[2])
                newvols[2] == goal_vol && return (nsteps+1, 2, newvols[1])
                
                # Mark
                push!(marked, newvols)
                
                # Enqueue
                push!(queue, (newvols, nsteps+1))
            end
        end
    end

    throw(DomainError("no solution for this configuration!"))
    
end

end