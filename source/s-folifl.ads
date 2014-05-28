pragma License (Unrestricted);
--  implementation unit
package System.Formatting.Literals.Float is
   pragma Pure;

   --  parsing Ada-form literals of real types

   procedure Get_Literal (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Float;
      Error : out Boolean);

end System.Formatting.Literals.Float;
