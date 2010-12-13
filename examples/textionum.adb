with Ada.Text_IO;
procedure textionum is
	type I is range -100 .. 100;
	package IIO is new Ada.Text_IO.Integer_IO (I);
	type M is mod 100;
	package MIO is new Ada.Text_IO.Modular_IO (M);
	type F is digits 3;
	package FIO is new Ada.Text_IO.Float_IO (F);
	type O is delta 0.1 range 0.1 .. 1.0;
	package OIO is new Ada.Text_IO.Fixed_IO (O);
	type D1 is delta 0.1 digits 3;
	package D1IO is new Ada.Text_IO.Decimal_IO (D1);
	type D2 is delta 10.0 digits 3;
	package D2IO is new Ada.Text_IO.Decimal_IO (D2);
begin
	D1IO.Put (10.0, Fore => 5); Ada.Text_IO.New_Line;
	D1IO.Put (-12.3, Aft => 3); Ada.Text_IO.New_Line;
	D2IO.Put (10.0); Ada.Text_IO.New_Line;
	D2IO.Put (-1230.0); Ada.Text_IO.New_Line;
end textionum;
