with Ada.Containers.Access_Holders;
with Ada.Containers.Access_Holders_Derivational_Conversions;
with Ada.Unchecked_Deallocation;
procedure smartptr is
	type Integer_Access is access Integer;
	procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
	type Base is tagged record
		Value : Integer;
	end record;
	type Base_Access is access all Base'Class;
	procedure Free is new Ada.Unchecked_Deallocation (Base'Class, Base_Access);
	type Derived is new Base with null record;
	type Derived_Access is access all Derived'Class;
	procedure Free is new Ada.Unchecked_Deallocation (Derived'Class, Derived_Access);
begin
	declare
		package S is new Ada.Containers.Access_Holders (Integer_Access);
		use type S.Holder;
		S1 : S.Holder := +new Integer'(99);
		S2 : S.Holder := S.Null_Holder;
	begin
		S.Move (S2, S1);
		pragma Assert (S2.Element.all = 99);
		-- weak
		declare
			use type S.Weak.Weak_Holder;
			S3 : S.Weak.Weak_Holder := +S2;
		begin
			declare
				S4 : S.Weak.Weak_Holder := +S2;
			begin
				pragma Assert (S.Weak.To_Holder (S4).Element.all = 99);
				S.Weak.Clear (S4);
				pragma Assert (S.Weak.Is_Null (S4));
			end;
			pragma Assert (not S.Weak.Is_Null (S3));
			pragma Assert (S.Weak.To_Holder (S3).Element.all = 99);
			pragma Assert (not S.Is_Null (S2));
			pragma Assert (S2.Element.all = 99);
			S.Clear (S2);
			pragma Assert (S.Weak.Is_Null (S3));
		end;
	end;
	-- derived to base
	declare
		package SB is new Ada.Containers.Access_Holders (Base_Access);
		use type SB.Holder;
		package SD is new Ada.Containers.Access_Holders (Derived_Access);
		use type SD.Holder;
		package Conv is new Ada.Containers.Access_Holders_Derivational_Conversions (
			Base, Base_Access, SB,
			Derived, Derived_Access, SD);
		S1 : SD.Holder := +new Derived'(Value => 999);
		S2 : SB.Holder := SB.Null_Holder;
	begin
		Conv.Move (S2, S1);
		pragma Assert (S2.Element.Value = 999);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end smartptr;
