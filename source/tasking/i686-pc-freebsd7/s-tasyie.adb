with C.pthread;
procedure System.Tasking.Yield is
begin
   C.pthread.pthread_yield;
end System.Tasking.Yield;
