gmat :=function(X,n);
	if X = 'A' then
		if validateN(n, 1) then

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

validateN:= function(n, lower_bound);
	if n < lower_bound then
		Print("n must be at least ",lower_bound);
		return false;
	else
		return true;
	fi;
end;