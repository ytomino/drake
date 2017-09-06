with Ada.Unchecked_Conversion;
with C.stdlib;
package body System.Native_Locales is
   use type C.char;
   use type C.char_array;
   use type C.char_ptr;
   use type C.size_t;

   subtype Fixed_char_array is C.char_array (C.size_t);
   type char_array_const_ptr is access constant Fixed_char_array
      with Convention => C;
   function To_char_array_const_ptr is
      new Ada.Unchecked_Conversion (C.char_ptr, char_array_const_ptr);

   --  should it use CFLocaleGetValue in OSX ???

   LANG : constant C.char_array := "LANG" & C.char'Val (0);

   --  implementation

   function Language return ISO_639_Alpha_2 is
      P : constant C.char_ptr := C.stdlib.getenv (LANG (0)'Access);
   begin
      if P /= null then
         declare
            Value : constant char_array_const_ptr :=
               To_char_array_const_ptr (P);
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
      return Ada.Locales.ISO_639_Alpha_2_Unknown;
   end Language;

   function Language return ISO_639_Alpha_3 is
   begin
      return Ada.Locales.To_Alpha_3 (Language);
   end Language;

   function Country return ISO_3166_1_Alpha_2 is
      P : constant C.char_ptr := C.stdlib.getenv (LANG (0)'Access);
   begin
      if P /= null then
         declare
            Value : constant char_array_const_ptr :=
               To_char_array_const_ptr (P);
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
      return Ada.Locales.ISO_3166_1_Alpha_2_Unknown;
   end Country;

end System.Native_Locales;
