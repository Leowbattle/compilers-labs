let var n;
    var x;
    var i
in
begin
	getint (n);
	if n then x := 0 else x := 1;
	i := 2;
	while i do
	begin
		x := x * i;
		i := i + 1
	end;
	printint (x)
end
