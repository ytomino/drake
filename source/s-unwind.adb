with System.Formatting;
package body System.Unwind is
   pragma Suppress (All_Checks);

   procedure Exception_Information (X : Exception_Occurrence) is
      subtype Fixed_String is String (Positive);
      Full_Name : Fixed_String;
      for Full_Name'Address use X.Id.Full_Name;
   begin
      Put ("Exception name: ");
      Put (Full_Name (1 .. X.Id.Name_Length));
      New_Line;
      Put ("Message: ");
      Put (X.Msg (1 .. X.Msg_Length));
      New_Line;
      if X.Pid /= 0 then
         --  output X.Pid is unimplemented
         null;
      end if;
      if X.Num_Tracebacks > 0 then
         Put ("Call stack traceback locations:");
         New_Line;
         for I in 1 .. X.Num_Tracebacks loop
            declare
               Use_Longest : constant Boolean :=
                  Standard'Address_Size > Formatting.Unsigned'Size;
               Item : constant Address := X.Tracebacks (I);
               Width : constant Natural := (Standard'Address_Size + 3) / 4;
               S : String (1 .. Width);
               Last : Natural;
               Error : Boolean;
            begin
               Put ("0x");
               if Use_Longest then
                  Formatting.Image (
                     Formatting.Longest_Unsigned (Item),
                     S,
                     Last,
                     Base => 16,
                     Casing => Formatting.Lower,
                     Width => Width,
                     Padding => '0',
                     Error => Error);
               else
                  Formatting.Image (
                     Formatting.Unsigned (Item),
                     S,
                     Last,
                     Base => 16,
                     Casing => Formatting.Lower,
                     Width => Width,
                     Padding => '0',
                     Error => Error);
               end if;
               Put (S (1 .. Last));
            end;
            if I < X.Num_Tracebacks then
               Put (" ");
            end if;
         end loop;
         New_Line;
      end if;
   end Exception_Information;

   procedure Save_Occurrence_No_Private (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence) is
   begin
      Target.Id := Source.Id;
      Target.Msg_Length := Source.Msg_Length;
      Target.Num_Tracebacks := Source.Num_Tracebacks;
      Target.Pid := Source.Pid;
      Target.Cleanup_Flag := Source.Cleanup_Flag;
      Target.Msg (1 .. Target.Msg_Length) :=
         Source.Msg (1 .. Target.Msg_Length);
      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
         Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence_No_Private;

end System.Unwind;
