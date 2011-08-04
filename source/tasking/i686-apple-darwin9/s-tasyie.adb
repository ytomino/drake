with C.pthread;
procedure System.Tasking.Yield is
begin
   C.pthread.pthread_yield_np;
end System.Tasking.Yield;
