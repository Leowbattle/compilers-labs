let var a;
    var b;
	var x := 1
in
begin
	getint(a);
	getint(b);

	while b > 0 do
	begin
		x := x * a;
		b := b - 1
	end;

	printint(x)
end