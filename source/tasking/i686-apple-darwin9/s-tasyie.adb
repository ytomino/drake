with C.pthread;
procedure System.Tasking.Yield is
   pragma Suppress (All_Checks);
begin
   C.pthread.pthread_yield_np;
end System.Tasking.Yield;
