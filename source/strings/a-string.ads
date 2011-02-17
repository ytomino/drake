pragma License (Unrestricted);
package Ada.Strings is
   pragma Pure;

   Space : constant Character := ' ';
   Wide_Space : constant Wide_Character := ' ';
   Wide_Wide_Space : constant Wide_Wide_Character := ' ';

   Length_Error, Pattern_Error, Index_Error, Translation_Error : exception;

   type Alignment is (Left, Right, Center);
   type Truncation is (Left, Right, Error);
   type Membership is (Inside, Outside);
   type Direction is (Forward, Backward);
   type Trim_End is (Left, Right, Both);

end Ada.Strings;
