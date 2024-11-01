let var n;
    var x;
    var i
in
begin
	getint (n);
	if n < 0 then x := 0 else x := 1;
	i := 2;
	while i <= n do
	begin
		printint(x);
		x := x * i;
		i := i + 1
	end;
	printint (x)
end
