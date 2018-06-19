pragma License (Unrestricted);
--  implementation unit
package System.Growth is
   pragma Preelaborate;

   generic
      type Count_Type is range <>;
   function Fast_Grow (Capacity : Count_Type) return Count_Type;

   generic
      type Count_Type is range <>;
      Component_Size : Positive;
   function Good_Grow (Capacity : Count_Type) return Count_Type;

   generic
      type Count_Type is range <>;
      Component_Size : Positive;
   package Scoped_Holder is

      function Capacity return Count_Type;
      procedure Reserve_Capacity (Capacity : Count_Type);

      function Storage_Address return Address;

   end Scoped_Holder;

end System.Growth;
