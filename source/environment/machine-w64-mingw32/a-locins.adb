with C.winnls;
with C.winnt;
package body Ada.Locales.Inside is
   pragma Suppress (All_Checks);
   use type C.signed_int;
   use type C.winnt.WCHAR;

   --  implementation

   function Language return ISO_639_Alpha_2 is
      Buffer : C.winnt.WCHAR_array (0 .. 15);
      R : C.signed_int;
   begin
      R := C.winnls.GetLocaleInfo (
         C.winnt.LOCALE_USER_DEFAULT,
         C.winnls.LOCALE_SISO639LANGNAME,
         Buffer (0)'Access,
         Buffer'Length);
      if R /= 3 then
         return ISO_639_Alpha_2_Unknown;
      else
         return (
            Character'Val (C.winnt.WCHAR'Pos (Buffer (0))),
            Character'Val (C.winnt.WCHAR'Pos (Buffer (1))));
      end if;
   end Language;

   function Language return ISO_639_Alpha_3 is
   begin
      return To_Alpha_3 (Language);
   end Language;

   function Country return ISO_3166_1_Alpha_2 is
      Buffer : C.winnt.WCHAR_array (0 .. 15);
      R : C.signed_int;
   begin
      R := C.winnls.GetLocaleInfo (
         C.winnt.LOCALE_USER_DEFAULT,
         C.winnls.LOCALE_SISO3166CTRYNAME,
         Buffer (0)'Access,
         Buffer'Length);
      if R /= 3 then
         return ISO_3166_1_Alpha_2_Unknown;
      else
         return (
            Character'Val (C.winnt.WCHAR'Pos (Buffer (0))),
            Character'Val (C.winnt.WCHAR'Pos (Buffer (1))));
      end if;
   end Country;

end Ada.Locales.Inside;
