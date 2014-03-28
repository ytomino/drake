pragma License (Unrestricted);
--  implementation unit
package System.Reference_Counting is
   pragma Pure;

   type Counter is mod 2 ** 32;
   for Counter'Size use 32;

   Static : constant Counter := Counter'Last;

   function Shared (Count : Counter) return Boolean;

   procedure Adjust (
      Reference_Count : aliased in out Counter);

   procedure Assign (
      Target : not null access Address;
      Target_Reference_Count : aliased in out Counter;
      Source : not null access constant Address;
      Source_Reference_Count : aliased in out Counter;
      Free : not null access procedure (Object : Address));

   procedure Clear (
      Target : not null access Address;
      Reference_Count : aliased in out Counter;
      Free : not null access procedure (Object : Address));

   procedure Move (
      Target : not null access Address;
      Target_Reference_Count : aliased in out Counter;
      Source : not null access Address;
      Sentinel : Address;
      Free : not null access procedure (Object : Address));

   procedure Unique (
      Target : not null access Address;
      Target_Reference_Count : aliased in out Counter;
      Target_Length : Natural;
      Target_Capacity : Natural;
      Max_Length : Natural;
      Capacity : Natural;
      Sentinel : Address;
      Copy : not null access procedure (
         Target : out Address;
         Source : Address;
         Length : Natural;
         Max_Length : Natural;
         Capacity : Natural);
      Free : not null access procedure (Object : Address));

   procedure Set_Length (
      Target : not null access Address;
      Target_Reference_Count : aliased in out Counter;
      Target_Length : Natural;
      Target_Max_Length : aliased in out Natural;
      Target_Capacity : Natural;
      New_Length : Natural;
      Sentinel : Address;
      Copy : not null access procedure (
         Target : out Address;
         Source : Address;
         Length : Natural;
         Max_Length : Natural;
         Capacity : Natural);
      Free : not null access procedure (Object : Address));

end System.Reference_Counting;
