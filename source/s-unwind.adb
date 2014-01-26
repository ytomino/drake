with System.Formatting.Address_Image;
package body System.Unwind is
   pragma Suppress (All_Checks);

   procedure Exception_Information (X : Exception_Occurrence) is
      subtype Fixed_String is String (Positive);
      Full_Name : Fixed_String;
      for Full_Name'Address use X.Id.Full_Name;
   begin
      Put ("Exception name: ");
      Put (Full_Name (1 .. X.Id.Name_Length - 1));
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
            Put ("0x");
            declare
               Item : constant Address := X.Tracebacks (I);
               Width : constant Natural := (Standard'Address_Size + 3) / 4;
               S : String (1 .. Width);
               Last : Natural;
            begin
               Formatting.Address_Image (
                  Item,
                  S,
                  Last,
                  Set => Formatting.Lower_Case);
               Put (S);
            end;
            if I < X.Num_Tracebacks then
               Put (" ");
            end if;
         end loop;
         New_Line;
      end if;
   end Exception_Information;

   procedure Save_Occurrence (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence) is
   begin
      Target.Id := Source.Id;
      Target.Msg_Length := Source.Msg_Length;
      Target.Num_Tracebacks := Source.Num_Tracebacks;
      Target.Pid := Source.Pid;
      Target.Msg (1 .. Target.Msg_Length) :=
         Source.Msg (1 .. Target.Msg_Length);
      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
         Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence;

end System.Unwind;
