pragma License (Unrestricted);
--  extended unit
generic
   type Index_Type is range <>;
   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;
   type Array_Access is access Array_Type;
   with procedure Free (X : in out Array_Access) is <>;
package Ada.Containers.Generic_Arrays is
   --  There are utility functions like Vectors for access-to-array types.
   pragma Preelaborate;

   type New_Array (<>) is limited private;

   subtype Extended_Index is Index_Type'Base range
      Index_Type'Base'Pred (Index_Type'First) ..
      Index_Type'Last;

   function "&" (Left : Array_Access; Right : Element_Type) return New_Array;

   function Length (Container : Array_Access) return Count_Type;

   procedure Set_Length (Container : in out Array_Access; Length : Count_Type);

   procedure Clear (Container : in out Array_Access)
      renames Free;

   procedure Move (Target : in out Array_Access; Source : in out Array_Access);

   procedure Assign (Target : in out Array_Access; Source : Array_Access);
   procedure Assign (Target : in out Array_Access; Source : New_Array);

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Array_Type);
   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Array_Access);
   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Element_Type;
      Count : Count_Type := 1);
   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      Count : Count_Type := 1);

   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Array_Type);
   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Array_Access);
   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Element_Type;
      Count : Count_Type := 1);
   procedure Prepend (
      Container : in out Array_Access;
      Count : Count_Type := 1);

   procedure Append (
      Container : in out Array_Access;
      New_Item : Array_Type);
   procedure Append (
      Container : in out Array_Access;
      New_Item : Array_Access);
   procedure Append (
      Container : in out Array_Access;
      New_Item : Element_Type;
      Count : Count_Type := 1);
   procedure Append (
      Container : in out Array_Access;
      Count : Count_Type := 1);

   procedure Delete (
      Container : in out Array_Access;
      Index : Extended_Index;
      Count : Count_Type := 1);
   procedure Delete_First (
      Container : in out Array_Access;
      Count : Count_Type := 1);
   procedure Delete_Last (
      Container : in out Array_Access;
      Count : Count_Type := 1);

   procedure Reverse_Elements (Container : in out Array_Access);
   procedure Swap (Container : in out Array_Access; I, J : Index_Type);

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is
      function Is_Sorted (Container : Array_Access) return Boolean;
      procedure Sort (Container : in out Array_Access);
      procedure Merge (
         Target : in out Array_Access;
         Source : in out Array_Access);
   end Generic_Sorting;

private

   type New_Array is new Array_Access;

end Ada.Containers.Generic_Arrays;
