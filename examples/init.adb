with Ada.Finalization;
with System.Initialization;
with System.Storage_Elements.Formatting;
procedure init is
	package SSEF renames System.Storage_Elements.Formatting;
	package Dummy is
		type Dummy is new Ada.Finalization.Controlled with null record;
		overriding procedure Initialize (Object : in out Dummy);
		overriding procedure Adjust (Object : in out Dummy);
		overriding procedure Finalize (Object : in out Dummy);
	end Dummy;
	package body Dummy is
		overriding procedure Initialize (Object : in out Dummy) is
		begin
			Ada.Debug.Put ("at " & SSEF.Image (Object'Address));
		end Initialize;
		overriding procedure Adjust (Object : in out Dummy) is
		begin
			Ada.Debug.Put ("at " & SSEF.Image (Object'Address));
		end Adjust;
		overriding procedure Finalize (Object : in out Dummy) is
		begin
			Ada.Debug.Put ("at " & SSEF.Image (Object'Address));
		end Finalize;
	end Dummy;
	package I is new System.Initialization (Dummy.Dummy);
	Storage : aliased I.Object_Storage;
	P : access Dummy.Dummy;
begin
	Ada.Debug.Put ("at " & SSEF.Image (Storage'Address));
	Ada.Debug.Put ("to init");
	P := I.New_Object (Storage'Access);
	Ada.Debug.Put ("to done");
	I.Dispose_Object (Storage'Access);
	Ada.Debug.Put ("end");
end init;
