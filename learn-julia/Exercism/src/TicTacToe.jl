module TicTacToe

export gamestate

function gamestate(boardstr)
	board = fromstr(boardstr)
	xwon = has_won(board, 1)
	owon = has_won(board, 0)
	num_xs = length(findall(==(1), board))
	num_os = length(findall(==(0), board))
	board_full = -1 ∉ board
	
	xwon && !owon && num_xs == num_os + 1 && return "win"
	owon && !xwon && num_xs == num_os && return "win"
	!xwon && !owon && board_full && num_xs == num_os + 1 && return "draw"
	!xwon && !owon && !board_full && (num_xs == num_os || num_xs == num_os + 1) && return "ongoing"
	error("invalid board!")
end

function fromstr(boardstr)
	board = fill(-1, 3, 3)
	for i in 1:3
		for j in 1:3
			if boardstr[i][j] == 'X'
				board[i, j] = 1
			elseif boardstr[i][j] == 'O'
				board[i, j] = 0
			end
		end
	end
	board
end

function has_won(board, v)
	any(board[i, :] == [v, v, v] for i in 1:3) && return true
	any(board[:, j] == [v, v, v] for j in 1:3) && return true
	[board[k, k] for k in 1:3] == [v, v, v] && return true
	[board[k, 3 - k + 1] for k in 1:3] == [v, v, v] && return true
	return false
end

end