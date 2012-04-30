pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
private with Ada.Finalization;
generic
   type Element_Type is limited private;
   type File_Type (<>) is limited private;
   with function End_Of_File (File : File_Type) return Boolean is <>;
   with procedure Get (File : in out File_Type; Item : out Element_Type) is <>;
package Ada.Containers.Forward_Iterators is
   --  This package makes iterators from End_Of_*/Get_* style functions.
   pragma Preelaborate;

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is limited private;

   function Constant_Reference (
      Container : File_Type;
      Position : Cursor)
      return Constant_Reference_Type;

   type Iterator is tagged limited private;

   function Iterate (Container : not null access File_Type) return Iterator;
   function First (Object : Iterator'Class) return Cursor;
   function Next (Object : Iterator'Class; Position : Cursor) return Cursor;

   Status_Error : exception
      renames IO_Exceptions.Status_Error;

private

   type Queue;
   type Queue_Access is access Queue;

   type Node;
   type Node_Access is access Node;

   type Node is limited record
      Reference_Count : Integer;
      Next : Node_Access;
      Element : aliased Element_Type;
   end record;

   type Cursor is new Finalization.Controlled with record
      Node : Node_Access;
   end record;

   overriding procedure Adjust (Object : in out Cursor);
   overriding procedure Finalize (Object : in out Cursor);

   type Queue is limited record
      File : not null access File_Type;
      Last : Node_Access;
      Next_Called : Boolean;
   end record;

   type Iterator is new Finalization.Limited_Controlled with record
      Queue : Queue_Access;
   end record;

   overriding procedure Finalize (Object : in out Iterator);

   No_Element : constant Cursor := (Finalization.Controlled with Node => null);

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

end Ada.Containers.Forward_Iterators;
