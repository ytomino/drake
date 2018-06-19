--  reference:
--  http://wiki.freepascal.org/User_Changes_3.0.4
--  http://www.kmonos.net/wlog/111.html
with Ada.Exceptions.Finally;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.System_Allocators.Allocated_Size;
package body System.Growth is
   use type Storage_Elements.Storage_Offset;

   procedure unreachable
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_unreachable";
   pragma No_Return (unreachable);

   --  implementation

   function Fast_Grow (Capacity : Count_Type) return Count_Type is
   begin
      return 2 * Capacity;
   end Fast_Grow;

   function Good_Grow (Capacity : Count_Type) return Count_Type is
      Result : Count_Type;
   begin
      if Capacity >=
         8 * 1024 * 1024 * Standard'Storage_Unit
            / Count_Type'Base (Component_Size) -- 8MB
      then
         if Capacity >=
            128 * 1024 * 1024 * Standard'Storage_Unit
               / Count_Type'Base (Component_Size) -- 128MB
         then
            Result :=
               Capacity
               + 32 * 1024 * 1024 * Standard'Storage_Unit
                  / Count_Type'Base (Component_Size); -- 32MB
         else
            Result := 3 * Capacity / 2; -- 1.5 < golden ratio (1.618...)
         end if;
      else
         Result := 2 * Capacity;
      end if;
      return Result;
   end Good_Grow;

   package body Scoped_Holder is

      procedure Finally (X : in out Address);
      procedure Finally (X : in out Address) is
      begin
         Standard_Allocators.Free (X);
      end Finally;

      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (Address, Finally);

      Buffer : aliased Address := Null_Address;
      Buffer_Capacity : Count_Type'Base := 0;

      --  implementation

      function Capacity return Count_Type is
      begin
         return Buffer_Capacity;
      end Capacity;

      procedure Reserve_Capacity (Capacity : Count_Type) is
         SO_Component_Size : constant Storage_Elements.Storage_Count :=
            Storage_Elements.Storage_Offset (Component_Size);
         Requested_Size : Storage_Elements.Storage_Count;
         Allocated_Size : Storage_Elements.Storage_Count;
      begin
         --  requested size
         if Component_Size rem Standard'Storage_Unit = 0 then
            --  optimized
            Requested_Size :=
               Storage_Elements.Storage_Offset (Capacity)
                  * (SO_Component_Size / Standard'Storage_Unit);
         else
            Requested_Size :=
               (Storage_Elements.Storage_Offset (Capacity) * SO_Component_Size
                     + (Standard'Storage_Unit - 1))
                  / Standard'Storage_Unit;
         end if;
         --  (re)allocate
         Buffer := Standard_Allocators.Reallocate (Buffer, Requested_Size);
         Holder.Assign (Buffer);
         --  allocated size
         Allocated_Size := System_Allocators.Allocated_Size (Buffer);
         if Allocated_Size <= 0 then
            unreachable;
         end if;
         if Component_Size rem Standard'Storage_Unit = 0 then
            --  optimized
            Buffer_Capacity :=
               Count_Type'Base (
                  Allocated_Size
                     / (SO_Component_Size / Standard'Storage_Unit));
         else
            Buffer_Capacity :=
               Count_Type'Base (
                  Allocated_Size * Standard'Storage_Unit / SO_Component_Size);
         end if;
      end Reserve_Capacity;

      function Storage_Address return Address is
      begin
         return Buffer;
      end Storage_Address;

   end Scoped_Holder;

end System.Growth;
