pragma License (Unrestricted);
--  extended unit
generic
   type Index_Type is range <>;
   type Element_Type is (<>); -- scalar only
   type Array_Type is array (Index_Type range <>) of Element_Type;
   type Name is access Array_Type;
procedure Ada.Unchecked_Reallocation (
   X : in out Name;
   First_Index : Index_Type;
   Last_Index : Index_Type'Base);
--  Change the size of the allocated array object.
pragma Preelaborate (Ada.Unchecked_Reallocation);
