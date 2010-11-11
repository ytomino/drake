pragma License (Unrestricted);
with Ada.Numerics.Complex_Types;
with Ada.Text_IO.Complex_IO;
package Ada.Complex_Text_IO is new Text_IO.Complex_IO (Numerics.Complex_Types);
