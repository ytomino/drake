package body Ada.Strings.Maps.Inside is

   function To_Set return T is
   begin
      return T (Character_Set'(Finalization.Controlled with
         Data => Set_Data_Access'(Source.all'Unchecked_Access)));
   end To_Set;

   function To_Mapping return T is
   begin
      return T (Character_Mapping'(Finalization.Controlled with
         Data => Map_Data_Access'(Source.all'Unchecked_Access)));
   end To_Mapping;

end Ada.Strings.Maps.Inside;
