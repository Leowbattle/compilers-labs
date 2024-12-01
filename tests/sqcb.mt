let
	fun square (x: Integer) : Integer = x*x;
	fun cube (x: Integer) : Integer = x*square(x);
	fun sqcb (b: Boolean, x:Integer) : Integer =
		b ? square(x) : cube(x)
in
begin
	printint(sqcb(true,3));
	printint(sqcb(false,3))
end