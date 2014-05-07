with C.pthread;
procedure System.Native_Tasks.Yield is
begin
   C.pthread.pthread_yield;
end System.Native_Tasks.Yield;
