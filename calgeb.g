valid_graphs := ['A', 'B', 'C', 'D', 'E', 'F', 'G']; # Valid graph letters
graph_min_n := [1, 2, 2, 4, 6, 4, 2]; # Min n for the graphs
graph_max_n := [-1, -1, -1, -1, 8, 4, 2]; # Max n for the graphs

# Internal function, no input validation done
_grel := function(X, n, i, j)
	if X = 'A' then
		if 1 = AbsInt(i-j) then # If i/j are next to each other, they're connected, else not (multiple connections don't occur here)
			return -1;
		fi;
	elif X = 'E' then
		if (i = 1 and j = 3) or (i = 3 and j = 1) or (i = 2 and j = 4) or (i = 4 and j = 2) or (1 = AbsInt(i-j) and not ( (i = 1 and j = 2) or (i = 2 and j = 1) or (i = 2 and j = 3) or (i = 3 and j = 2))) then
			return -1;
		fi;
	fi;

	return 0; # No connection between j and j
end;

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

		# Generate matrix A(gamma)
		for i in [1..n] do
			row := [];
 
			for j in [1..n] do
				
				if i = j then
					row[j] := 2;
				else
					row[j] := _grel(X, n, i, j);				

				fi;

			od;
			
			mat[i] := row;
		od;

		return mat;
	else
		Print("X must be in {A, B, C, D, E, F, G}");
	fi;

end;
