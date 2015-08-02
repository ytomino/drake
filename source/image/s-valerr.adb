with System.Runtime_Context;
pragma Warnings (Off, System.Runtime_Context); -- break "pure" rule
package body System.Value_Errors is

   procedure Raise_Value_Failure (T : String; S : String) is
      Message : String (1 .. T'Length + S'Length * 2 + 11);
      Last : Natural;
   begin
      Last := T'Length;
      Message (1 .. Last) := T;
      Last := Last + 9;
      Message (Last - 8 .. Last) := "'Value (""";
      for I in S'Range loop
         if S (I) = '"' then
            Last := Last + 1;
            Message (Last) := '"';
         end if;
         Last := Last + 1;
         Message (Last) := S (I);
      end loop;
      Last := Last + 1;
      Message (Last) := '"';
      Last := Last + 1;
      Message (Last) := ')';
      raise Constraint_Error with Message (1 .. Last);
   end Raise_Value_Failure;

   procedure Raise_Discrete_Value_Failure (T : String; S : String) is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      if TLS.No_Discrete_Value_Failure_Propagation then
         TLS.Discrete_Value_Failure := True;
      else
         Raise_Value_Failure (T, S);
      end if;
   end Raise_Discrete_Value_Failure;

end System.Value_Errors;
