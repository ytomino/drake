pragma License (Unrestricted);
--  implementation unit
package System.Value_Errors is
   pragma Pure;

   procedure Raise_Value_Failure (T : String; S : String);

   pragma No_Return (Raise_Value_Failure);

end System.Value_Errors;
