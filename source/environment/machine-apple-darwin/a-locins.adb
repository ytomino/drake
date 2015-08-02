with System.Address_To_Named_Access_Conversions;
with C.stdlib;
package body Ada.Locales.Inside is
   use type C.char;
   use type C.char_array;
   use type C.char_ptr;
   use type C.size_t;

   --  should it use CFLocaleGetValue in OSX ???

   LANG : constant C.char_array := "LANG" & C.char'Val (0);

   --  implementation

   function Language return ISO_639_Alpha_2 is
      package Conv is
         new System.Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      P : constant C.char_ptr := C.stdlib.getenv (LANG (0)'Access);
   begin
      if P /= null then
         declare
            Value : C.char_array (C.size_t);
            for Value'Address use Conv.To_Address (P);
            Len : C.size_t := 0;
         begin
            while Value (Len) in 'a' .. 'z' loop
               Len := Len + 1;
            end loop;
            if Len = 2 then
               return (1 => Character (Value (0)), 2 => Character (Value (1)));
            end if;
         end;
      end if;
      return ISO_639_Alpha_2_Unknown;
   end Language;

   function Language return ISO_639_Alpha_3 is
   begin
      return To_Alpha_3 (Language);
   end Language;

   function Country return ISO_3166_1_Alpha_2 is
      package Conv is
         new System.Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      P : constant C.char_ptr := C.stdlib.getenv (LANG (0)'Access);
   begin
      if P /= null then
         declare
            Value : C.char_array (C.size_t);
            for Value'Address use Conv.To_Address (P);
            I : C.size_t := 0;
         begin
            while Value (I) /= C.char'Val (0)
               and then Value (I) /= '.' -- codeset
               and then Value (I) /= '@' -- modifier
            loop
               I := I + 1;
            end loop;
            if I >= 2
               and then Value (I - 1) in 'A' .. 'Z'
               and then Value (I - 2) in 'A' .. 'Z'
               and then (I = 2 or else Value (I - 3) = '_')
            then
               return (
                  1 => Character (Value (I - 2)),
                  2 => Character (Value (I - 1)));
            end if;
         end;
      end if;
      return ISO_3166_1_Alpha_2_Unknown;
   end Country;

end Ada.Locales.Inside;
