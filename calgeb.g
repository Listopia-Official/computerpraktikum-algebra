valid_graphs := ['A', 'B', 'C', 'D', 'E', 'F', 'G']; # Valid graph letters
graph_min_n := [1, 2, 2, 4, 6, 4, 2]; # Min n for the graphs
graph_max_n := [-1, -1, -1, -1, 8, 4, 2]; # Max n for the graphs

# Internal function, no input validation done
_grel := function(X, n, i, j)
	if X = 'A' then
		if 1 = AbsInt(i-j) then # If i/j are next to each other, they're connected, else not (multiple connections don't occur here)
			return -1;
		fi;
	elif X = 'B' or X = 'C' then
		if (i = 1 and j = 2 and X = 'B') or (i = 2 and j = 1 and X = 'C') then
			return -2;
		elif 1 = AbsInt(i-j)  then
			return -1;
		fi;
	elif X = 'D' then
		if (i = 1 and j = 3) or (i = 3 and j = 1) or (1 = AbsInt(i-j) and not ( (i = 1 and j = 2) or (i = 2 and j = 1))) then
			return -1;
		fi;
	elif X = 'E' then
		if (i = 1 and j = 3) or (i = 3 and j = 1) or (i = 2 and j = 4) or (i = 4 and j = 2) or (1 = AbsInt(i-j) and not ( (i = 1 and j = 2) or (i = 2 and j = 1) or (i = 2 and j = 3) or (i = 3 and j = 2))) then
			return -1;
		fi;
	elif X = 'F' then # Only F_4 possible
		if i = 3 and j = 2 then
			return -2;
		elif AbsInt(i-j) = 1 then
			return -1;
		fi;
	elif X = 'G' then # Only G_2 is possible, so we can hardcode the matrix
		if i = 1 then
			return -1;
		else
			return -3;
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

# Computes the matrices of the w_i for the default base {e_1, ..., e_2} of Q^n
# Input: An n x n matrix representing a graph as above
glin := function(mat)
	local n, w_i, matrices, e_j, k, i, j, e_i, row;

	n := Length(mat); # Dimension of the quadratic matrix

	matrices := []; # Array containing the matrices for the w_i

	for i in [1..n] do
		w_i := []; # Matrix for linear function w_i

		for k in [1..n] do
			row := []; # Row of w_i matrix

			for j in [1..n] do
				if k = j then
					e_j := 1;
				else
					e_j := 0;
				fi;

				if k = i then
					e_i := 1;
				else
					e_i := 0;
				fi;

				row[j] := e_j - mat[i][j] * e_i;
			od;
			w_i[k] := row;
		od;

		matrices[i] := w_i;
	od;

	return matrices;

end;

# Computes the set phi as defined in the exercise
# Input: An array containing the matrices of the n linear functions w_i
gphi := function(matrices)
	local phi, e_i, i, n, j, last_count, ph;

	n := Length(matrices); # Count of linear functions

	phi := Set([]);

	# Initialize phi with e_i
	for i in [1..n] do
		e_i := [];

		for j in [1..n] do
			if i = j then
				e_i[j] := 1;
			else
				e_i[j] := 0;
			fi;		
		od;
		
		AddSet(phi, e_i);

	od;

	last_count := Length(phi); # Monitoring the count of phi

	while true do
		for ph in Iterator(phi) do
			for j in [1..n] do
				AddSet(phi, matrices[j] * ph);	
			od;
		od;

		if last_count = Length(phi) then
			break;
		else
			last_count := Length(phi);
		fi;
	od;

	return phi;

end;

# Computes the set w by multiplying every element with every element as long as that creates new elements
# Input: An array containing the matrices of the n linear functions w_i
gw := function(matrices)
	local w, w1, w2, old_length, cache, arr, prod;

	w:= Set(matrices);

	old_length := Length(w);

	cache := Set([]); # Stores the factors for the computation of the matrix product for performance reasons

	while true do

		for w1 in Iterator(w) do
			for w2 in Iterator(w) do
				arr := [w1, w2];

				if not arr in cache then
					prod := w1*w2;
					AddSet(w, prod);
					AddSet(cache, arr);		
				fi;
	
			od;
		od;

		if old_length = Length(w) then
			break;
		else
			old_length := Length(w);
		fi;

	od;

	return w;
end;