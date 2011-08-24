with C.pthread;
procedure System.Tasking.Yield is
   pragma Suppress (All_Checks);
begin
   C.pthread.pthread_yield;
end System.Tasking.Yield;
