pragma License (Unrestricted);
--  implementation unit for Ada.Containers.Counted_Access_Holders
with System.Reference_Counting;
package Ada.Containers.Inside.Weak_Access_Holders is
   pragma Preelaborate;

   type Data;
   type Data_Access is access all Data;

   type Weak_Holder;
   type Weak_Holder_Access is access all Weak_Holder;

   type Data is limited record
      Reference_Count : aliased System.Reference_Counting.Counter;
      Weak_List : Weak_Holder_Access;
   end record;
   pragma Suppress_Initialization (Data);

   type Weak_Holder is record
      Data : not null Data_Access;
      Previous : Weak_Holder_Access;
      Next : Weak_Holder_Access;
   end record;
   pragma Suppress_Initialization (Weak_Holder);

   procedure Add_Weak (Item : Weak_Holder_Access);
   procedure Remove_Weak (Item : Weak_Holder_Access);
   procedure Clear_Weaks (
      List : in out Data;
      Null_Data : not null Data_Access);

   Data_Size : constant :=
      System.Reference_Counting.Counter'Size
      + Standard'Address_Size;

   pragma Compile_Time_Error (Data'Size /= Data_Size, "bad Data_Size");

end Ada.Containers.Inside.Weak_Access_Holders;
