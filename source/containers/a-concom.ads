pragma License (Unrestricted);
--  extended package
package Ada.Containers.Comparators is
   pragma Pure;

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function LE (Left, Right : Element_Type) return Boolean;

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function GT (Left, Right : Element_Type) return Boolean;

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function GE (Left, Right : Element_Type) return Boolean;

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Generic_Compare (Left, Right : Element_Type) return Integer;

end Ada.Containers.Comparators;
