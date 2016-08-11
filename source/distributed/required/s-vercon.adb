with System.Formatting;
package body System.Version_Control is

   function Get_Version_String (V : Unsigned_Types.Unsigned)
      return Version_String
   is
      Last : Natural;
      Error : Boolean;
   begin
      return Result : Version_String do
         Formatting.Image (
            Formatting.Unsigned (V),
            Result,
            Last,
            Base => 16,
            Width => Version_String'Length,
            Error => Error);
         pragma Assert (not Error and then Last = Result'Last);
      end return;
   end Get_Version_String;

end System.Version_Control;
