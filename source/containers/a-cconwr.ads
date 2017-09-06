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
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         New_Length : Count_Type;
         Capacity : Count_Type));

   procedure Copy (
      Target : not null access Container;
      Source : not null access constant Container;
      Length : Count_Type;
      New_Capacity : Count_Type;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         New_Length : Count_Type;
         Capacity : Count_Type));

   procedure Move (
      Target : not null access Container;
      Source : not null access Container;
      Free : not null access procedure (Object : in out Data_Access));

   procedure Unique (
      Target : not null access Container;
      To_Update : Boolean;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         New_Length : Count_Type;
         Capacity : Count_Type);
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
         New_Length : Count_Type;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type; -- copying length
         New_Length : Count_Type;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access);
      Max_Length : not null access function (Data : not null Data_Access)
         return not null access Count_Type);

   --  Note: Copy and Reserve_Capacity also make a container unique.

   procedure In_Place_Set_Length (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type;
      New_Length : Count_Type;
      Failure : out Boolean; -- reallocation is needed
      Max_Length : not null access function (Data : not null Data_Access)
         return not null access Count_Type);

   function Zero (Data : not null Data_Access)
      return not null access Count_Type;

end Ada.Containers.Copy_On_Write;
