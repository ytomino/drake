with C.winbase;
procedure System.Tasking.Yield is
   pragma Suppress (All_Checks);
begin
   C.winbase.Sleep (0);
end System.Tasking.Yield;
