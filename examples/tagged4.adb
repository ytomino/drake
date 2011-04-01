with Ada.Strings.Unbounded;
with Ada.Tags.Delegating;
procedure tagged4 is
	package I is
		type Intf is limited interface;
		procedure Method (Object : Intf) is abstract;
	end I;
	package A is
		type Aggregated_Object (Message : access String) is new I.Intf with null record;
		procedure Dummy (Object : Aggregated_Object);
		overriding procedure Method (Object : Aggregated_Object);
	end A;
	package body A is
		procedure Dummy (Object : Aggregated_Object) is
		begin
			null;
		end Dummy;
		overriding procedure Method (Object : Aggregated_Object) is
		begin
			Ada.Debug.Put (Object.Message.all);
		end Method;
	end A;
	package C is
		type Container (Message : access String) is tagged limited record
			Field : aliased A.Aggregated_Object (Message);
		end record;
		function Get (Object : not null access Container'Class) return access I.Intf'Class;
	end C;
	package body C is
		function Get (Object : not null access Container'Class) return access I.Intf'Class is
		begin
			return I.Intf'Class (Object.Field)'Access;
		end Get;
		procedure Impl is new Ada.Tags.Delegating.Implements (Container, I.Intf, Get);
	begin
		Impl;
	end C;
	package D is
		type Container (Additional : access String) is
			new C.Container (Additional) with
		record
			Controlled_Field : Ada.Strings.Unbounded.Unbounded_String;
		end record;
	end D;
begin
	declare
		Obj : aliased C.Container (new String'("Hello."));
		X : Boolean := C.Container'Class (Obj) in I.Intf'Class;
		Intf : access I.Intf'Class := I.Intf'Class (C.Container'Class (Obj))'Access;
	begin
		if X then
			Ada.Debug.Put ("membership test is ok.");
		end if;
		Ada.Debug.Put (Ada.Tags.Expanded_Name (Intf'Tag));
		I.Method (Intf.all);
	end;
	declare
		Obj : aliased D.Container (new String'("Hello, derived."));
		X : Boolean := D.Container'Class (Obj) in I.Intf'Class;
		Intf : access I.Intf'Class := I.Intf'Class (D.Container'Class (Obj))'Access;
	begin
		if X then
			Ada.Debug.Put ("membership test for derived type is ok.");
		end if;
		Ada.Debug.Put (Ada.Tags.Expanded_Name (Intf'Tag));
		I.Method (Intf.all);
	end;
end tagged4;
