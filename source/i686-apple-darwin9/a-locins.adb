--  with System.Zero_Terminated_Strings;
with C.stdlib;
package body Ada.Locales.Inside is
   pragma Suppress (All_Checks);
   use type C.char_array;
   use type C.char_ptr;

   --  should it use CFLocaleGetValue in OSX ???

   LANG : constant C.char_array := "LANG" & C.char'Val (0);

   --  implementation

   function Language return ISO_639_Alpha_2 is
      P : constant C.char_ptr := C.stdlib.getenv (LANG (0)'Access);
   begin
      if P /= null then
         declare
            subtype Fixed_String is String (Positive);
            Value : Fixed_String;
            for Value'Address use P.all'Address;
            Len : Natural := 0;
         begin
            while Value (Len + 1) in 'a' .. 'z' loop
               Len := Len + 1;
            end loop;
            if Len = 2 then
               return (1 => Value (1), 2 => Value (2));
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
      P : constant C.char_ptr := C.stdlib.getenv (LANG (0)'Access);
   begin
      if P /= null then
         declare
            subtype Fixed_String is String (Positive);
            Value : Fixed_String;
            for Value'Address use P.all'Address;
            Last : Natural := 0;
         begin
            while Value (Last + 1) /= Character'Val (0)
               and then Value (Last + 1) /= '.' -- codeset
               and then Value (Last + 1) /= '@' -- modifier
            loop
               Last := Last + 1;
            end loop;
            if Last >= 2
               and then Value (Last) in 'A' .. 'Z'
               and then Value (Last - 1) in 'A' .. 'Z'
               and then (Last = 2 or else Value (Last - 2) = '_')
            then
               return (1 => Value (Last - 1), 2 => Value (Last));
            end if;
         end;
      end if;
      return ISO_3166_1_Alpha_2_Unknown;
   end Country;

end Ada.Locales.Inside;
