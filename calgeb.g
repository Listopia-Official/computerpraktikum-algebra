valid_graphs := ['A', 'B', 'C', 'D', 'E', 'F', 'G']; # Valid graph letters
graph_min_n := [1, 2, 2, 4, 6, 4, 2]; # Min n for the graphs
graph_max_n := [-1, -1, -1, -1, 8, 4, 2]; # Max n for the graphs

# Returns a matrix representing the graph X_n
gmat :=function(X,n)
	local row, i, j, mat, min_n, max_n, graph_index;

	mat := [];

	if X in valid_graphs then
		
		# Input validation start

		graph_index := Position(valid_graphs, X); # Corresponds to the data entries of one graph at the arrays above

		min_n := graph_min_n[graph_index];
		max_n := graph_max_n[graph_index];

		if n < min_n then
			Print("n must be at least ", min_n);
			return;
		fi;

		if n > max_n and max_n > -1 then
			Print("n mustn't be greater than ", max_n);
			return;
		fi;

		# Input validation finished
	else
		Print("X must be in {A, B, C, D, E, F, G}");
	fi;

end;

_grel := function(X, n, i, j)
Print("WIP");
end;
