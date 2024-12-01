let
	fun fact (n:Integer) : Integer =
		n <= 0 ? 1 : n * fact (n-1);
	var x : Integer;
	var i : Integer := 0
in
begin
	getint(x);
	while i < x do
	begin
		printint(fact(i));
		i := i + 1
	end
end