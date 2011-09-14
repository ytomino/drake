with Ada.Containers.Scoped_Access_Holders;
with Ada.Containers.Counted_Access_Holders;
with Ada.Containers.Intrusive_Counted_Access_Holders;
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
	pragma Debug (Ada.Debug.Put ("OK"));
end smartptr;
