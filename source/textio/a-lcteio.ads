pragma License (Unrestricted);
with Ada.Numerics.Long_Complex_Types;
with Ada.Text_IO.Complex_IO;
package Ada.Long_Complex_Text_IO is
   new Text_IO.Complex_IO (Numerics.Long_Complex_Types);
