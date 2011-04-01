with System.Storage_Elements;
package body System.Bit_Ops is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;

   function Bit_Eq (
      Left : Address;
      Llen : Natural;
      Right : Address;
      Rlen : Natural)
      return Boolean is
   begin
      if Llen /= Rlen then
         return False;
      else
         declare
            Quotient : constant Storage_Elements.Storage_Count :=
               Storage_Elements.Storage_Count (Llen) / Standard'Storage_Unit;
            Remainder : constant Natural := Llen mod Standard'Storage_Unit;
            type Fixed_Unit_Array is array (1 .. Quotient) of
               Storage_Elements.Storage_Element;
            L_Units : Fixed_Unit_Array;
            for L_Units'Address use Left;
            R_Units : Fixed_Unit_Array;
            for R_Units'Address use Right;
         begin
            if L_Units /= R_Units then -- compiler will use memcmp
               return False;
            elsif Remainder /= 0 then
               declare
                  L_Rem : Storage_Elements.Storage_Element;
                  for L_Rem'Address use Left + Quotient;
                  R_Rem : Storage_Elements.Storage_Element;
                  for R_Rem'Address use Right + Quotient;
               begin
                  case Default_Bit_Order is
                     when High_Order_First =>
                        declare
                           Mask : constant Storage_Elements.Storage_Element :=
                              not (Storage_Elements.
                                 Shift_Left (1, 8 - Remainder) - 1);
                        begin
                           return (L_Rem and Mask) = (R_Rem and Mask);
                        end;
                     when Low_Order_First =>
                        declare
                           Mask : constant Storage_Elements.Storage_Element :=
                              Storage_Elements.Shift_Left (1, Remainder) - 1;
                        begin
                           return (L_Rem and Mask) = (R_Rem and Mask);
                        end;
                  end case;
               end;
            else
               return True;
            end if;
         end;
      end if;
   end Bit_Eq;

end System.Bit_Ops;
