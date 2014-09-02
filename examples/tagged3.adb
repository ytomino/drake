with Ada.Tags.Generic_Dispatching_Constructor;
procedure tagged3 is
	package Root is
		type T is abstract tagged limited null record;
		function Create (Params : not null access Integer) return T is abstract;
	end Root;
	package Derived is
		type T is new Root.T with null record;
		overriding function Create (Params : not null access Integer) return T;
	end Derived;
	package body Derived is
		overriding function Create (Params : not null access Integer) return T is
		begin
			Ada.Debug.Put ("create derived");
			return (Root.T with null record);
		end Create;
	end Derived;
	function Virtual_Create is new Ada.Tags.Generic_Dispatching_Constructor (
		Root.T,
		Integer,
		Root.Create);
	Params : aliased Integer := 10;
	Obj : Root.T'Class := Virtual_Create (Derived.T'Tag, Params'Access);
begin
	Ada.Debug.Put (Ada.Tags.Expanded_Name (Obj'Tag));
end tagged3;
