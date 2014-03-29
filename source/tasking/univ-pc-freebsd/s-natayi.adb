with C.pthread;
procedure System.Native_Tasks.Yield is
   pragma Suppress (All_Checks);
begin
   C.pthread.pthread_yield;
end System.Native_Tasks.Yield;
