with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;
-- all Integer_Text_IO
with Ada.Short_Short_Integer_Text_IO;
with Ada.Short_Integer_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;
-- all Float_Text_IO
with Ada.Short_Float_Text_IO;
with Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Long_Long_Float_Text_IO;
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
	S : String (1 .. 12);
begin
	D1IO.Put (10.0, Fore => 5); Ada.Text_IO.New_Line;
	D1IO.Put (-12.3, Aft => 3); Ada.Text_IO.New_Line;
	D2IO.Put (10.0); Ada.Text_IO.New_Line;
	D2IO.Put (-1230.0); Ada.Text_IO.New_Line;
	pragma Assert (Integer (Float'(5490.0)) = 5490);
	Ada.Float_Text_IO.Put (5490.0); Ada.Text_IO.New_Line;
	Ada.Float_Text_IO.Put (S, 5490.0);
	Ada.Debug.Put (S);
	pragma Assert (S = " 5.49000E+03");
	Test_Enumeration_IO : declare
		package Boolean_IO is new Ada.Text_IO.Enumeration_IO (Boolean);
		Boolean_Item : Boolean;
		package Character_IO is new Ada.Text_IO.Enumeration_IO (Character);
		Character_Item : Character;
		package Wide_Character_IO is new Ada.Text_IO.Enumeration_IO (Wide_Character);
		Wide_Character_Item : Wide_Character;
		package Wide_Wide_Character_IO is new Ada.Text_IO.Enumeration_IO (Wide_Wide_Character);
		Wide_Wide_Character_Item : Wide_Wide_Character;
		type E is (A, B, C);
		package E_IO is new Ada.Text_IO.Enumeration_IO (E);
		E_Item : E;
		package Integer_IO is new Ada.Text_IO.Enumeration_IO (Integer);
		Integer_Item : Integer;
		package M_IO is new Ada.Text_IO.Enumeration_IO (M);
		M_Item : M;
		Last : Natural;
	begin
		Boolean_IO.Get ("True", Boolean_Item, Last);
		pragma Assert (Boolean_Item and then Last = 4);
		begin
			Boolean_IO.Get ("null", Boolean_Item, Last);
			raise Program_Error;
		exception
			when Ada.Text_IO.Data_Error => null;
		end;
		Character_IO.Get ("Hex_FF", Character_Item, Last);
		pragma Assert (Character_Item = Character'Val (16#FF#) and then Last = 6);
		begin
			Character_IO.Get ("Hex_100", Character_Item, Last);
			raise Program_Error;
		exception
			when Ada.Text_IO.Data_Error => null;
		end;
		Wide_Character_IO.Get ("Hex_FFFF", Wide_Character_Item, Last);
		pragma Assert (Wide_Character_Item = Wide_Character'Val (16#FFFF#) and then Last = 8);
		begin
			Wide_Character_IO.Get ("Hex_10000", Wide_Character_Item, Last);
			raise Program_Error;
		exception
			when Ada.Text_IO.Data_Error => null;
		end;
		Wide_Wide_Character_IO.Get ("Hex_7FFFFFFF", Wide_Wide_Character_Item, Last);
		pragma Assert (Wide_Wide_Character_Item = Wide_Wide_Character'Val (16#7FFFFFFF#) and then Last = 12);
		begin
			Wide_Wide_Character_IO.Get ("Hex_80000000", Wide_Wide_Character_Item, Last);
			raise Program_Error;
		exception
			when Ada.Text_IO.Data_Error => null;
		end;
		E_IO.Get ("A", E_Item, Last);
		pragma Assert (E_Item = A and then Last = 1);
		begin
			E_IO.Get ("D", E_Item, Last);
			raise Program_Error;
		exception
			when Ada.Text_IO.Data_Error => null;
		end;
		Integer_IO.Get ("10", Integer_Item, Last);
		pragma Assert (Integer_Item = 10 and then Last = 2);
		begin
			Integer_IO.Get ("1A", Integer_Item, Last);
			raise Program_Error;
		exception
			when Ada.Text_IO.Data_Error => null;
		end;
		M_IO.Get ("10", M_Item, Last);
		pragma Assert (M_Item = 10 and then Last = 2);
		begin
			M_IO.Get ("1A", M_Item, Last);
			raise Program_Error;
		exception
			when Ada.Text_IO.Data_Error => null;
		end;
	end Test_Enumeration_IO;
	declare
		package Integer_Wide_Text_IO is new Ada.Wide_Text_IO.Integer_IO (Integer);
		package Unsigned_Wide_Text_IO is new Ada.Wide_Text_IO.Modular_IO (M);
		package Enumeration_Wide_Text_IO is new Ada.Wide_Text_IO.Enumeration_IO (Boolean);
		package Float_Wide_Text_IO is new Ada.Wide_Text_IO.Float_IO (F);
		package Fixed_Wide_Text_IO is new Ada.Wide_Text_IO.Fixed_IO (O);
		package Decimal_Wide_Text_IO is new Ada.Wide_Text_IO.Decimal_IO (D1);
	begin
		null;
	end;
	declare
		package Integer_Wide_Wide_Text_IO is new Ada.Wide_Wide_Text_IO.Integer_IO (Integer);
		package Unsigned_Wide_Wide_Text_IO is new Ada.Wide_Wide_Text_IO.Modular_IO (M);
		package Enumeration_Wide_Wide_Text_IO is new Ada.Wide_Wide_Text_IO.Enumeration_IO (Boolean);
		package Float_Wide_Wide_Text_IO is new Ada.Wide_Wide_Text_IO.Float_IO (F);
		package Fixed_Wide_Wide_Text_IO is new Ada.Wide_Wide_Text_IO.Fixed_IO (O);
		package Decimal_Wide_Wide_Text_IO is new Ada.Wide_Wide_Text_IO.Decimal_IO (D1);
	begin
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end textionum;
