let
	var x: Integer;
	fun func (a: Integer, b: Integer) : Integer = a + b
in
begin
	x := func(1+1, 3);
	printint(x)
end
