let
	var x: Integer;
	var i: Integer;
	fun fib(n: Integer): Integer = n < 2 ? n : fib(n - 1) + fib(n - 2)
in
begin
	getint(x);
	i := 0;
	while i < x do
	begin
		printint(fib(i));
		i := i + 1
	end
end