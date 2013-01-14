package body Ada.Strings.Maps.Inside is

   function To_Set return T is
   begin
      return T (Character_Set'(Create (
         Set_Data_Access'(Source.all'Unchecked_Access))));
   end To_Set;

   function To_Mapping return T is
   begin
      return T (Character_Mapping'(Create (
         Map_Data_Access'(Source.all'Unchecked_Access))));
   end To_Mapping;

end Ada.Strings.Maps.Inside;
