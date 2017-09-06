pragma License (Unrestricted);
with Ada.Iterator_Interfaces;
private with Ada.Finalization;
private with System.Native_Environment_Variables;
package Ada.Environment_Variables is
   pragma Preelaborate;

   function Value (Name : String) return String;
   function Value (Name : String; Default : String) return String;
   pragma Inline (Value); -- renamed

   function Exists (Name : String) return Boolean;
   pragma Inline (Exists); -- renamed

   procedure Set (Name : String; Value : String);
   pragma Inline (Set); -- renamed

   procedure Clear (Name : String);
   procedure Clear;
   pragma Inline (Clear); -- renamed

   procedure Iterate (
      Process : not null access procedure (Name, Value : String));

   --  extended from here
   --  There is a simple iterator:

   type Cursor is private; -- moved from below
   pragma Preelaborable_Initialization (Cursor);

   function Has_Element (Position : Cursor) return Boolean;
   pragma Inline (Has_Element);

   function Name (Position : Cursor) return String;
   function Value (Position : Cursor) return String;

   pragma Inline (Name);
   pragma Inline (Value);

   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate return Iterator_Interfaces.Forward_Iterator'Class;

   --  to here

--  package Name_Value_Pairs is

--    type Name_Value_Pair_Type is tagged limited private;

      --  Operations on Name_Value_Pairs

--    function Name (Name_Value_Pair : Name_Value_Pair_Type) return String;

--    function Value (Name_Value_Pair : Name_Value_Pair_Type) return String;

      --  Note: If it follows AI12-0009-1/06 strictly, these is no way to make
      --    a value of Name_Value_Pair_Type, because it is declared as private
      --    and in the nested package Name_Value_Pairs.
      --  Perhaps, Current_Variable should be moved here.

--  private
      --  not specified by the languages
--  end Name_Value_Pairs;

--  use Name_Value_Pairs;

--  type Environment_Variable_Listing is tagged limited private
--    with
--       Constant_Indexing => Current_Variable,
--       Default_Iterator => Iterate,
--       Iterator_Element => Name_Value_Pairs.Name_Value_Pair_Type;
--  pragma Preelaborable_Initialization (Environment_Variable_Listing);

--  function Environment return Environment_Variable_Listing;

--  type Cursor is private; -- declared in above
--  pragma Preelaborable_Initialization (Cursor);

--  function Has_Environment_Variable (Position : Cursor) return Boolean
--    renames Has_Element;

--  package Environment_Variable_Iterators
--    renames Iterator_Interfaces;

--  function Iterate (Listing : Environment_Variable_Listing)
--    return Environment_Variable_Iterators.Forward_Iterator'Class;

--  function Current_Variable (
--    Variables : Environment_Variable_Listing;
--    Position : Cursor)
--    return Name_Value_Pairs.Name_Value_Pair_Type;
--    with Pre => Has_Environment_Variable (Position);

   --  Note: These "renames" should be unified when the AI have been decided.

private

   function Value (Name : String) return String
      renames System.Native_Environment_Variables.Value;

   function Value (Name : String; Default : String) return String
      renames System.Native_Environment_Variables.Value;

   function Exists (Name : String) return Boolean
      renames System.Native_Environment_Variables.Exists;

   procedure Set (Name : String; Value : String)
      renames System.Native_Environment_Variables.Set;

   procedure Clear (Name : String)
      renames System.Native_Environment_Variables.Clear;

   procedure Clear
      renames System.Native_Environment_Variables.Clear;

   type Cursor is new System.Native_Environment_Variables.Cursor;

   type Iterator is limited new Finalization.Limited_Controlled
      and Iterator_Interfaces.Forward_Iterator with
   record
      Block : System.Address := System.Null_Address;
   end record;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Environment_Variables;
