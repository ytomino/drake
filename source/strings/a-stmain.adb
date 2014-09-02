package body Ada.Strings.Maps.Inside is

   function To_Set return Character_Set is
   begin
      return Create (
         Set_Data_Access'(Source.all'Unchecked_Access));
   end To_Set;

   function To_Mapping return Character_Mapping is
   begin
      return Create (
         Map_Data_Access'(Source.all'Unchecked_Access));
   end To_Mapping;

end Ada.Strings.Maps.Inside;
