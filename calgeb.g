# Returns a matrix representing the graph X_n
gmat :=function(X,n)
	local row, i, j, mat;

	mat := [];

	if X = 'A' then
		if validateN(n, 1) then
			for i in [1..n] do
				row:=[];
				for j in [1..n] do
					if i = j then
						row[j] := 2; # 
					elif AbsInt(i-j) = 1 then # i and j are connected
						row[j] := -1;
					else
						row[j] := 0;
					fi;
				od;
				mat[i] := row	;	
			od;
			return mat;
		fi;
	elif X = 'B' then
		if validateN(n, 2) then

		fi;
	elif X = 'C' then
		if validateN(n, 2) then

		fi;
	elif X = 'D' then
		if validateN(n, 4) then

		fi;
	elif X = 'E' then
		if validateN(n, 4) then

		fi;	
	elif X = 'F' then
		if validateN(n, 4) then

		fi;
	elif X = 'G' then
		if validateN(n, 2) then

		fi;
	else
		Print("X must be in {A, B, C, D, E, F, G}");
	fi;
end;

# Internal function used to ensure a number is at least as high as lower_bound
validateN:= function(n, lower_bound);
	if n < lower_bound then
		Print("n must be at least ",lower_bound);
		return false;
	else
		return true;
	fi;
end;
