pragma License (Unrestricted);
--  implementation unit
package System.Value_Errors is
   pragma Pure;

   procedure Raise_Value_Failure (T : String; S : String);
   pragma No_Return (Raise_Value_Failure);

   procedure Raise_Discrete_Value_Failure (T : String; S : String);
   --  it returns if the flag of System.Runtime_Context is set.

end System.Value_Errors;
