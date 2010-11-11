package body Ada.Text_IO.Integer_IO is

   procedure Put (
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base) is
   begin
      raise Program_Error;
   end Put;

end Ada.Text_IO.Integer_IO;
