pragma License (Unrestricted);
--  implementation unit
private package Ada.Containers.Copy_On_Write is
   pragma Preelaborate;

   type Container;

   type Container_Access is access all Container;
   for Container_Access'Storage_Size use 0;

   Data_Size : constant := Standard'Address_Size;

   type Data is limited record
      Follower : Container_Access; -- first container is owner
      pragma Atomic (Follower);
   end record;

   for Data'Size use Data_Size;

   type Data_Access is access Data;

   function Shared (Data : Data_Access) return Boolean;

   type Container is record
      Data : Data_Access;
      pragma Atomic (Data);
      Next_Follower : Container_Access;
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

   procedure Copy (
      Target : not null access Container;
      Source : not null access constant Container;
      Length : Count_Type;
      New_Capacity : Count_Type;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         Max_Length : Count_Type; -- new length
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         Max_Length : Count_Type; -- new length
         Capacity : Count_Type));

   procedure Move (
      Target : not null access Container;
      Source : not null access Container;
      Free : not null access procedure (Object : in out Data_Access));

   procedure Unique (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type;
      New_Length : Count_Type;
      New_Capacity : Count_Type;
      To_Update : Boolean;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         Max_Length : Count_Type; -- new length
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         Max_Length : Count_Type; -- new length
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         Max_Length : Count_Type; -- new length
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access));

   --  Copy and Reserve_Capacity also make it unique.

   procedure In_Place_Set_Length (
      Target_Data : Data_Access;
      Target_Length : Count_Type;
      Target_Max_Length : aliased in out Count_Type; -- may be updated
      Target_Capacity : Count_Type;
      New_Length : Count_Type;
      Failure : out Boolean); -- reallocation is needed

end Ada.Containers.Copy_On_Write;
