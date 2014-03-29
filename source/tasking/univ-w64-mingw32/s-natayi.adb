with C.winbase;
procedure System.Native_Tasks.Yield is
   pragma Suppress (All_Checks);
begin
   C.winbase.Sleep (0);
end System.Native_Tasks.Yield;
