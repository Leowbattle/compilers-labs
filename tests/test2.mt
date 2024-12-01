let
	fun func1 (a: Integer, b: Integer, c: Integer) : Integer = a;
	fun func2 (a: Integer, b: Integer, c: Integer) : Integer = b;
	fun func3 (a: Integer, b: Integer, c: Integer) : Integer = c
in
begin
	printint(func1(1, 2, 3));
	printint(func2(1, 2, 3));
	printint(func3(1, 2, 3))
end
