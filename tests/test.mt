let
	var x : Integer := 4;
	fun func (a: Integer, b: Boolean) : Integer = x;
	var y : Boolean := true
in
begin
	x := 1;
	y := func(1, false) == 2
end
