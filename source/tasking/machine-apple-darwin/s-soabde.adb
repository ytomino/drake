with System.Native_Tasks;
with System.Native_Time;
with System.Tasks;
package body System.Synchronous_Objects.Abortable.Delays is

   procedure Delay_For (D : Duration) is
      Aborted : Boolean;
   begin
      Tasks.Enable_Abort;
      declare
         Attr : constant access Native_Tasks.Task_Attribute_Of_Abort :=
            Tasks.Abort_Attribute;
      begin
         if Attr /= null then
            declare
               Value : Boolean;
            begin
               Wait (
                  Event (Attr.all),
                  Timeout => D,
                  Value => Value);
               Aborted := Value or else Tasks.Is_Aborted;
            end;
         else
            Native_Time.Simple_Delay_For (D);
            Aborted := Tasks.Is_Aborted;
         end if;
      end;
      Tasks.Disable_Abort (Aborted);
   end Delay_For;

   procedure Register_Delays is
   begin
      Native_Time.Delay_For_Hook := Delay_For'Access;
   end Register_Delays;

   procedure Unregister_Delays is
   begin
      Native_Time.Delay_For_Hook := Native_Time.Simple_Delay_For'Access;
   end Unregister_Delays;

end System.Synchronous_Objects.Abortable.Delays;
