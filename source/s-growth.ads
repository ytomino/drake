pragma License (Unrestricted);
--  implementation unit
package System.Growth is
   pragma Pure;

   generic
      type Count_Type is range <>;
   function Fast_Grow (Capacity : Count_Type) return Count_Type;

   generic
      type Count_Type is range <>;
      Component_Size : Positive;
   function Good_Grow (Capacity : Count_Type) return Count_Type;

end System.Growth;
