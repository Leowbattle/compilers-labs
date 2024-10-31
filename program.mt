let var n := 3;
    var x := 4;
	var z
in
begin
	n := 4;
	if x then
		x := 4
	else
	begin
		x := 3;
		n := 8
	end;

	while x + 1 do
	begin
		if x then
			getint(x)
		else
			printint(n + 5);

		n := 0
	end;

	z := n < 0 ? 1 :
	     n > 0 ? 3 : 4
end