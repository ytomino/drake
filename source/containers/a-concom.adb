package body Ada.Containers.Comparators is

   function LE (Left, Right : Element_Type) return Boolean is
   begin
      return not (Right < Left);
   end LE;

   function GT (Left, Right : Element_Type) return Boolean is
   begin
      return Right < Left;
   end GT;

   function GE (Left, Right : Element_Type) return Boolean is
   begin
      return not (Left < Right);
   end GE;

   function Generic_Compare (Left, Right : Element_Type) return Integer is
   begin
      if Left < Right then
         return -1;
      elsif Right < Left then
         return 1;
      else
         return 0;
      end if;
   end Generic_Compare;

end Ada.Containers.Comparators;
