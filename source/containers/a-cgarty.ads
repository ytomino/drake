pragma License (Unrestricted);
--  extended unit
generic
   type Index_Type is range <>;
   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;
--  diff (Array_Access)
--  diff (Free)
package Ada.Containers.Generic_Array_Types is
   --  Ada.Containers.Vectors-like utilities for array types.
   pragma Preelaborate;

   subtype Extended_Index is
      Index_Type'Base range
         Index_Type'Base'Pred (Index_Type'First) ..
         Index_Type'Last;

   function Length (Container : Array_Type) return Count_Type;

--  diff (Set_Length)
--
--

--  diff (Clear)
--

   procedure Assign (Target : in out Array_Type; Source : Array_Type);

   procedure Move (
      Target : in out Array_Type;
      Source : in out Array_Type);

--  diff (Insert)
--
--
--
--  diff (Insert)
--
--
--
--  diff (Insert)
--
--
--
--
--  diff (Insert)
--
--
--

--  diff (Prepend)
--
--
--  diff (Prepend)
--
--
--  diff (Prepend)
--
--
--
--  diff (Prepend)
--
--

--  diff (Append)
--
--
--  diff (Append)
--
--
--  diff (Append)
--
--
--
--  diff (Append)
--
--

--  diff (Delete)
--
--
--
--  diff (Delete_First)
--
--
--  diff (Delete_Last)
--
--

   procedure Swap (Container : in out Array_Type; I, J : Index_Type);

   function First_Index (Container : Array_Type) return Index_Type;
   function Last_Index (Container : Array_Type) return Extended_Index;

   generic
      with procedure Swap (
         Container : in out Array_Type;
         I, J : Index_Type) is Generic_Array_Types.Swap;
   package Generic_Reversing is

      procedure Reverse_Elements (Container : in out Array_Type);

      procedure Reverse_Rotate_Elements (
         Container : in out Array_Type;
         Before : Extended_Index);
      procedure Juggling_Rotate_Elements (
         Container : in out Array_Type;
         Before : Extended_Index);
      procedure Rotate_Elements (
         Container : in out Array_Type;
         Before : Extended_Index)
         renames Juggling_Rotate_Elements;

   end Generic_Reversing;

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
      with procedure Swap (
         Container : in out Array_Type;
         I, J : Index_Type) is Generic_Array_Types.Swap;
   package Generic_Sorting is

      function Is_Sorted (Container : Array_Type) return Boolean;

      procedure Insertion_Sort (Container : in out Array_Type);
      procedure Merge_Sort (Container : in out Array_Type);
      procedure Sort (Container : in out Array_Type)
         renames Merge_Sort;

--  diff (Merge)
--
--

   end Generic_Sorting;

end Ada.Containers.Generic_Array_Types;
