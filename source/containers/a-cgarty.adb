with Ada.Containers.Array_Sorting;
with System.Long_Long_Integer_Types;
package body Ada.Containers.Generic_Array_Types is

   subtype Word_Integer is System.Long_Long_Integer_Types.Word_Integer;

   --  implementation

   function Length (Container : Array_Type) return Count_Type is
   begin
--  diff
--  diff
--  diff
      return Container'Length;
--  diff
   end Length;

--  diff (Set_Length)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

   procedure Assign (Target : in out Array_Type; Source : Array_Type) is
   begin
      Target := Source;
--  diff
--  diff
--  diff
--  diff
--  diff
   end Assign;

   procedure Move (
      Target : in out Array_Type;
      Source : in out Array_Type)
   is
      pragma Unmodified (Source);
   begin
      Assign (Target, Source);
--  diff
--  diff
   end Move;

--  diff (Insert)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Insert)
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Insert)
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Insert)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Prepend)
--
--
--
--
--

--  diff (Prepend)
--
--
--
--
--

--  diff (Prepend)
--
--
--
--
--
--

--  diff (Prepend)
--
--
--
--
--

--  diff (Append)
--
--
--
--
--

--  diff (Append)
--
--
--
--
--

--  diff (Append)
--
--
--
--
--
--

--  diff (Append)
--
--
--
--
--

--  diff (Delete)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Delete_First)
--
--
--
--
--

--  diff (Delete_Last)
--
--
--
--
--

   procedure Swap (Container : in out Array_Type; I, J : Index_Type) is
      pragma Check (Pre,
         Check =>
            (I in First_Index (Container) .. Last_Index (Container)
               and then J in First_Index (Container) .. Last_Index (Container))
            or else raise Constraint_Error);
--  diff
   begin
      if I /= J then
         declare
            Temp : constant Element_Type := Container (I);
         begin
            Container (I) := Container (J);
            Container (J) := Temp;
         end;
      end if;
   end Swap;

   function First_Index (Container : Array_Type) return Index_Type is
   begin
--  diff
--  diff
--  diff
      return Container'First;
--  diff
   end First_Index;

   function Last_Index (Container : Array_Type) return Extended_Index is
   begin
--  diff
--  diff
--  diff
      return Container'Last;
--  diff
   end Last_Index;

   package body Generic_Reversing is

      type Context_Type is limited record
         Container : not null access Array_Type;
      end record;
      pragma Suppress_Initialization (Context_Type);

      procedure Swap (I, J : Word_Integer; Params : System.Address);
      procedure Swap (I, J : Word_Integer; Params : System.Address) is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         Swap (Context.Container.all, Index_Type'Val (I), Index_Type'Val (J));
      end Swap;

      --  implementation

      procedure Reverse_Elements (Container : in out Array_Type) is
         pragma Unmodified (Container);
         Context : aliased Context_Type :=
            (Container => Container'Unrestricted_Access);
      begin
--  diff
         Array_Sorting.In_Place_Reverse (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Container'Last),
            Context'Address,
            Swap => Swap'Access);
--  diff
      end Reverse_Elements;

      procedure Reverse_Rotate_Elements (
         Container : in out Array_Type;
         Before : Extended_Index)
      is
         pragma Check (Pre,
            Check =>
               Before in First_Index (Container) .. Last_Index (Container) + 1
               or else raise Constraint_Error);
         pragma Unmodified (Container);
         Context : aliased Context_Type :=
            (Container => Container'Unrestricted_Access);
      begin
--  diff
         Array_Sorting.Reverse_Rotate (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Before),
            Index_Type'Pos (Container'Last),
            Context'Address,
            Swap => Swap'Access);
--  diff
      end Reverse_Rotate_Elements;

      procedure Juggling_Rotate_Elements (
         Container : in out Array_Type;
         Before : Extended_Index)
      is
         pragma Check (Pre,
            Check =>
               Before in First_Index (Container) .. Last_Index (Container) + 1
               or else raise Constraint_Error);
         pragma Unmodified (Container);
         Context : aliased Context_Type :=
            (Container => Container'Unrestricted_Access);
      begin
--  diff
         Array_Sorting.Juggling_Rotate (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Before),
            Index_Type'Pos (Container'Last),
            Context'Address,
            Swap => Swap'Access);
--  diff
      end Juggling_Rotate_Elements;

   end Generic_Reversing;

   package body Generic_Sorting is

      type Context_Type is limited record
         Container : not null access Array_Type;
      end record;
      pragma Suppress_Initialization (Context_Type);

      function LT (Left, Right : Word_Integer; Params : System.Address)
         return Boolean;
      function LT (Left, Right : Word_Integer; Params : System.Address)
         return Boolean
      is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         return Context.Container (Index_Type'Val (Left)) <
            Context.Container (Index_Type'Val (Right));
      end LT;

      procedure Swap (I, J : Word_Integer; Params : System.Address);
      procedure Swap (I, J : Word_Integer; Params : System.Address) is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         Swap (Context.Container.all, Index_Type'Val (I), Index_Type'Val (J));
      end Swap;

      --  implementation

      function Is_Sorted (Container : Array_Type) return Boolean is
         Context : aliased Context_Type :=
            (Container => Container'Unrestricted_Access);
      begin
--  diff
         return Array_Sorting.Is_Sorted (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Container'Last),
            Context'Address,
            LT => LT'Access);
      end Is_Sorted;

      procedure Insertion_Sort (Container : in out Array_Type) is
         pragma Unmodified (Container);
         Context : aliased Context_Type :=
            (Container => Container'Unrestricted_Access);
      begin
--  diff
         Array_Sorting.Insertion_Sort (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Container'Last),
            Context'Address,
            LT => LT'Access,
            Swap => Swap'Access);
--  diff
      end Insertion_Sort;

      procedure Merge_Sort (Container : in out Array_Type) is
         pragma Unmodified (Container);
         Context : aliased Context_Type :=
            (Container => Container'Unrestricted_Access);
      begin
--  diff
         Array_Sorting.In_Place_Merge_Sort (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Container'Last),
            Context'Address,
            LT => LT'Access,
            Swap => Swap'Access);
--  diff
      end Merge_Sort;

   end Generic_Sorting;

end Ada.Containers.Generic_Array_Types;
