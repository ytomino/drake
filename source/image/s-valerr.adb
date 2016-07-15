with System.Runtime_Context;
pragma Warnings (Off, System.Runtime_Context); -- break "pure" rule
package body System.Value_Errors is

   procedure Raise_Value_Failure (T : String; S : String) is
      T_Length : constant Natural := T'Length;
      Message : String (1 .. T_Length + S'Length * 2 + 11);
      Last : Natural;
   begin
      Last := T_Length;
      Message (1 .. Last) := T;
      Last := Last + 9;
      Message (Last - 8 .. Last) := "'Value (""";
      for I in S'Range loop
         declare
            E : Character renames S (I);
         begin
            if E = '"' then
               Last := Last + 1;
               Message (Last) := '"';
            end if;
            Last := Last + 1;
            Message (Last) := E;
         end;
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
