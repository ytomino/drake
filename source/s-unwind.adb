package body System.Unwind is
   pragma Suppress (All_Checks);

   --  weak reference for System.Unwind.Backtrace
   procedure Backtrace_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address))
      with Import, -- weak linking
         Convention => Ada, External_Name => "__drake_backtrace_information";

   pragma Weak_External (Backtrace_Information);

   procedure Exception_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address))
   is
      subtype Fixed_String is String (Positive);
      Full_Name : Fixed_String;
      for Full_Name'Address use X.Id.Full_Name;
   begin
      Put ("Exception name: ", Params);
      Put (Full_Name (1 .. X.Id.Name_Length - 1), Params);
      New_Line (Params);
      Put ("Message: ", Params);
      Put (X.Msg (1 .. X.Msg_Length), Params);
      New_Line (Params);
      if X.Pid /= 0 then
         --  output X.Pid is unimplemented
         null;
      end if;
      if X.Num_Tracebacks > 0
         and then Backtrace_Information'Address /= Null_Address
      then
         Backtrace_Information (
            X,
            Params,
            Put => Put,
            New_Line => New_Line);
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
