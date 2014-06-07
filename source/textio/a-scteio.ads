pragma License (Unrestricted);
with Ada.Numerics.Short_Complex_Types;
with Ada.Text_IO.Complex_IO;
package Ada.Short_Complex_Text_IO is
   new Text_IO.Complex_IO (Numerics.Short_Complex_Types);
