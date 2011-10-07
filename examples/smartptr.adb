with Ada.Containers.Scoped_Access_Holders;
with Ada.Containers.Counted_Access_Holders;
with Ada.Containers.Intrusive_Counted_Access_Holders;
with Ada.Containers.Counted_Access_Holders_Derivational_Conversions;
with Ada.Unchecked_Deallocation;
procedure smartptr is
	type Integer_Access is access Integer;
	procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
	procedure Retain (X : Integer_Access) is
	begin
		X.all := X.all + 1;
	end Retain;
	procedure Release (X : in out Integer_Access) is
	begin
		X.all := X.all - 1;
		if X.all = 0 then
			Free (X);
		end if;
	end Release;
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
		package U is new Ada.Containers.Scoped_Access_Holders (Integer_Access);
		use type U.Holder;
		S1 : U.Holder := +new Integer'(123);
		S2 : U.Holder := U.Null_Holder;
	begin
		U.Move (S2, S1);
		pragma Assert (S2.Element.all = 123);
	end;
	declare
		package S is new Ada.Containers.Counted_Access_Holders (Integer_Access);
		use type S.Holder;
		S1 : S.Holder := +new Integer'(99);
		S2 : S.Holder := S.Null_Holder;
	begin
		S.Move (S2, S1);
		pragma Assert (S2.Element.all = 99);
	end;
	declare
		package I is new Ada.Containers.Intrusive_Counted_Access_Holders (Integer_Access);
		use type I.Holder;
		function "+" is new I.Generic_To_Holder (Retain => False);
		S1 : I.Holder := +new Integer'(1);
		S2 : I.Holder := I.Null_Holder;
	begin
		I.Move (S2, S1);
		pragma Assert (S2.Element.all = 1);
	end;
	--  derived to base
	declare
		package SB is new Ada.Containers.Counted_Access_Holders (Base_Access);
		use type SB.Holder;
		package SD is new Ada.Containers.Counted_Access_Holders (Derived_Access);
		use type SD.Holder;
		package Conv is new Ada.Containers.Counted_Access_Holders_Derivational_Conversions (
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
