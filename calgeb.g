valid_graphs := ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'Z']; # Valid graph letters (Z_3 is the cyclic one!)
graph_min_n := [1, 2, 2, 4, 6, 4, 2, 3]; # Min n for the graphs
graph_max_n := [-1, -1, -1, -1, 8, 4, 2, 3]; # Max n for the graphs, -1 means no maximum is specified

# Internal function, no input validation is done
# The function returns the matrix element at the i-th/j-th positions for the graph matrices for i != j
_grel := function(X, n, i, j)
	if X = 'A' then
		if 1 = AbsInt(i-j) then # If i/j are next to each other, they're connected, else not (multiple connections don't occur here)
			return -1;
		fi;
	elif X = 'B' or X = 'C' then
		if (i = 1 and j = 2 and X = 'B') or (i = 2 and j = 1 and X = 'C') then # If no direction is specified, the graphs are connected in both directions, so we've to do symmetrical checks
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
	elif X = 'Z' then # The cyclic graph
		if (i = 1 and j = 2) or (i = 2 and j = 1) or (i = 1 and j = 3) or (i = 3 and j = 1) or (i = 2 and j = 3) or (i = 3 and j = 2) then
			return -1;
		fi;
	fi;

	return 0; # No connection between j and j
end;

# Returns a matrix representing the graph X_n, as specified
# X: The graph character, has to be in the list above of valid characters
# n: The number n, has to meet the conditions as specified in the exercise instructions
# The input will be validated, and in case of constraint violations an error message will be shown
gmat :=function(X,n)
	local row, i, j, mat, min_n, max_n, graph_index;

	mat := [];

	if X in valid_graphs then
		
		# Input validation start

		graph_index := Position(valid_graphs, X); # Corresponds to the data entries of one graph at the arrays above

		# Fetch bounds for n (-1 for maximum means no upper bound)
		min_n := graph_min_n[graph_index];
		max_n := graph_max_n[graph_index];

		if n < min_n then
			Print("n must be at least ", min_n);
			return;
		fi;

		if n > max_n and max_n > -1 then # Take -1 into account
			Print("n mustn't be greater than ", max_n);
			return;
		fi;

		# Input validation finished

		# Generate matrix A(gamma)
		for i in [1..n] do
			row := [];
 
			for j in [1..n] do
				
				if i = j then
					row[j] := 2; # Diagonal entries are 2
				else
					row[j] := _grel(X, n, i, j); # Otherwise delegate to internal function			

				fi;

			od;
			
			mat[i] := row; # Will the matrix row-wise
		od;

		return mat;
	else
		Print("X must be in {A, B, C, D, E, F, G}");
	fi;

end;

# Computes the matrices of the w_i for the default base {e_1, ..., e_2} of Q^n
# Input: An n x n matrix representing a graph as above
# Output: A list containing the matrices for w_1, ..., w_n (in that order)
glin := function(mat)
	local n, w_i, matrices, e_j, k, i, j, e_i, row;

	n := Length(mat); # Dimension of the quadratic matrix

	matrices := []; # Array containing the matrices for the w_i

	for i in [1..n] do
		w_i := []; # Matrix for linear function w_i

		for k in [1..n] do
			row := []; # Row of w_i matrix

			for j in [1..n] do
				if k = j then # Compute the unity vector e_j
					e_j := 1;
				else
					e_j := 0;
				fi;

				if k = i then # Compute the unity vector e_i
					e_i := 1;
				else
					e_i := 0;
				fi;

				row[j] := e_j - mat[i][j] * e_i; # Fill the matrix element-wise with the formula specified in the excersise
			od;
			w_i[k] := row;
		od;

		matrices[i] := w_i;
	od;

	return matrices;

end;

# Computes the set phi as defined in the exercise
# Input: An array containing the matrices of the n linear functions w_i, the order doesn't matter here
gphi := function(matrices)
	local phi, e_i, i, n, j, last_count, ph;

	n := Length(matrices); # Count of linear functions

	phi := Set([]);

	# Initialize phi with e_i
	for i in [1..n] do
		e_i := [];

		for j in [1..n] do
			if i = j then # Generate the e_i
				e_i[j] := 1;
			else
				e_i[j] := 0;
			fi;		
		od;
		
		AddSet(phi, e_i);

	od;

	last_count := Length(phi); # Monitoring the count of phi

	# Now we'll call phi on the elements (my multiplying the matrices) with each other until no new elements get added
	while true do
		for ph in Iterator(phi) do
			for j in [1..n] do
				AddSet(phi, matrices[j] * ph);	
			od;
		od;

		# Check whether new elements got added
		if last_count = Length(phi) then
			break; # No new elements, break here
		else
			last_count := Length(phi);
		fi;
	od;

	return phi;

end;

# Computes the set w by multiplying every new element of the last step with every element.
# Input: An array containing the matrices of the n linear functions w_i, the order doesn't matter here
gw := function(matrices)
	local w, w1, w2, prod, additions, newAdditions;

	w:= Set(matrices);

	additions := Immutable(w); # Elements that got added with the last iteration
	newAdditions := Set([]); # Monitor the new elements that got added

	while not Length(additions) = 0 do # Loop until no new elements got added

		for w1 in Iterator(additions) do
			for w2 in Iterator(w) do

				prod := w1*w2; # Multiply the new matrices to the current set w
                			
					if not prod in w then
					    AddSet(w, prod);
					    AddSet(newAdditions, prod); # If the matrix is not in w, save it here
					fi;
			od;
		od;

		additions := Immutable(newAdditions); # The additions of this iterations are the new additions
		newAdditions := Set([]); # Reset those
	od;

	return w;
end;
