with Ada.Finalization;
with System.Initialization;
procedure init is
	use type System.Address;
begin
	-- controlled type
	declare
		type Phase_Type is (Uninitialized, Initialized, Finalized);
		Phase : Phase_Type := Uninitialized;
		The_Address : System.Address;
		Adjust_Count : Natural := 0;
		In_Another : Boolean := False; -- handling Another_Object
		
		type Dummy is new Ada.Finalization.Controlled with null record;
		overriding procedure Initialize (Object : in out Dummy);
		overriding procedure Adjust (Object : in out Dummy);
		overriding procedure Finalize (Object : in out Dummy);
		
		overriding procedure Initialize (Object : in out Dummy) is
		begin
			pragma Assert (Object'Address = The_Address);
			pragma Assert (Phase = Uninitialized);
			Phase := Initialized;
		end Initialize;
		overriding procedure Adjust (Object : in out Dummy) is
		begin
			pragma Assert (Object'Address = The_Address);
			pragma Assert (Phase = Finalized);
			Phase := Initialized;
			Adjust_Count := Adjust_Count + 1;
		end Adjust;
		overriding procedure Finalize (Object : in out Dummy) is
		begin
			if not In_Another then
				pragma Assert (Object'Address = The_Address);
				pragma Assert (Phase = Initialized);
				Phase := Finalized;
			end if;
		end Finalize;
		
		Another_Object : Dummy := (Ada.Finalization.Controlled with null record);
		
		package I is new System.Initialization (Dummy);
		Storage : aliased I.Object_Storage;
		P : access Dummy;
	begin
		The_Address := Storage'Address;
		P := I.New_Object (Storage'Access);
		pragma Assert (Phase = Initialized);
		pragma Assert (Adjust_Count = 0);
		P.all := Another_Object;
		pragma Assert (Phase = Initialized);
		pragma Assert (Adjust_Count = 1);
		I.Dispose_Object (Storage'Access);
		pragma Assert (Phase = Finalized);
		
		In_Another := True; -- for finalizing Anotehr_Object
	end;
	-- default value
	declare
		type T is record
			F : Integer := 123;
		end record;
		package J is new System.Initialization (T);
		Storage : aliased J.Object_Storage;
		P : access T;
	begin
		P := J.New_Object (Storage'Access);
		pragma Assert (P.all = (F => 123));
		P := J.New_Object (Storage'Access, (F => 456));
		pragma Assert (P.all = (F => 456));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end init;
