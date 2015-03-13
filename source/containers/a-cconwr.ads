pragma License (Unrestricted);
--  implementation unit
private package Ada.Containers.Copy_On_Write is
   pragma Preelaborate;

   type Container;

   Data_Size : constant := Standard'Address_Size;

   type Data is limited record
      Follower : access Container; -- first container is owner
      pragma Atomic (Follower);
   end record;

   for Data'Size use Data_Size;

   type Data_Access is access Data;

   function Shared (Data : not null Data_Access) return Boolean;

   type Container is record
      Data : Data_Access;
      pragma Atomic (Data);
      Next_Follower : access Container;
      pragma Atomic (Next_Follower);
   end record;

   procedure Adjust (
      Target : not null access Container);

   procedure Assign (
      Target : not null access Container;
      Source : not null access constant Container;
      Free : not null access procedure (Object : in out Data_Access));

   procedure Clear (
      Target : not null access Container;
      Free : not null access procedure (Object : in out Data_Access));

   function Copy (
      Source : not null access constant Container;
      Length : Count_Type;
      Capacity : Count_Type; -- always used
      Allocate : not null access procedure (
         Target : out Data_Access;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type))
      return Container;

   procedure Move (
      Target : not null access Container;
      Source : not null access Container;
      Free : not null access procedure (Object : in out Data_Access));

   procedure Unique (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type; -- current value
      Capacity : Count_Type; -- new value
      To_Update : Boolean;
      Allocate : not null access procedure (
         Target : out Data_Access;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access));

   --  Copy and Reserve_Capacity also make it unique.

   Integer_A : constant := Integer'Alignment * Standard'Storage_Unit;

   Data_Ex_Size : constant := (Data_Size
      + (Count_Type'Size + Integer_A - 1) / Integer_A * Integer_A
      + (Boolean'Size + Integer_A - 1) / Integer_A * Integer_A);

   type Data_Ex is limited record
      Super : aliased Data;
      Max_Length : aliased Count_Type := 0;
      Is_Aliased : aliased Boolean := False;
   end record;

   for Data_Ex'Size use Data_Ex_Size;

   type Data_Ex_Access is access Data_Ex;

   procedure Set_Length (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type;
      New_Length : Count_Type;
      Allocate : not null access procedure (
         Target : out Data_Access;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access));

end Ada.Containers.Copy_On_Write;
