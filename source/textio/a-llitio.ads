pragma License (Unrestricted);
with Ada.Text_IO;
package Ada.Long_Long_Integer_Text_IO is
   new Text_IO.Integer_IO (Long_Long_Integer);
