pragma License (Unrestricted);
--  implementation unit
with System.Storage_Elements;
package System.Reference_Counting is
   pragma Pure;

   type Counter is mod 2 ** 32;
   for Counter'Size use 32;
   pragma Atomic (Counter);

   Static : constant := 2 ** 32 - 1;
      --  This should be untyped.
      --  A typed atomic constant disables static elaboration.

   type Data_Access is access all Counter;
   for Data_Access'Storage_Size use 0;

   function Shared (Data : not null Data_Access) return Boolean;

   subtype Container is
      not null Data_Access; -- should be initialized with a sentinel

   procedure Adjust (
      Target : not null access Container);

   procedure Assign (
      Target : not null access Container;
      Source : not null access constant Container;
      Free : not null access procedure (Object : in out Data_Access));

   procedure Clear (
      Target : not null access Container;
      Free : not null access procedure (Object : in out Data_Access));

   procedure Move (
      Target : not null access Container;
      Source : not null access Container;
      Sentinel : not null Data_Access;
      Free : not null access procedure (Object : in out Data_Access));

   subtype Length_Type is Storage_Elements.Storage_Count;

   procedure Unique (
      Target : not null access Container;
      Target_Length : Length_Type;
      Target_Capacity : Length_Type;
      Max_Length : Length_Type;
      Capacity : Length_Type;
      Sentinel : not null Data_Access;
      Reallocate : not null access procedure (
         Target : aliased in out not null Data_Access;
         Length : Length_Type; -- copying length
         Max_Length : Length_Type; -- new length
         Capacity : Length_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Length_Type; -- copying length
         Max_Length : Length_Type; -- new length
         Capacity : Length_Type);
      Free : not null access procedure (Object : in out Data_Access));

   procedure Set_Length (
      Target : not null access Container;
      Target_Length : Length_Type;
      Target_Max_Length : aliased in out Length_Type;
      Target_Capacity : Length_Type;
      New_Length : Length_Type;
      Sentinel : not null Data_Access;
      Reallocate : not null access procedure (
         Target : aliased in out not null Data_Access;
         Length : Length_Type; -- copying length
         Max_Length : Length_Type; -- new length
         Capacity : Length_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Length_Type; -- copying length
         Max_Length : Length_Type; -- new length
         Capacity : Length_Type);
      Free : not null access procedure (Object : in out Data_Access));

end System.Reference_Counting;
