pragma License (Unrestricted);
--  implementation unit specialized for Windows
procedure System.Tasking.Yield;
--  Yield forces the running task to relinquish the processor for an instant.
pragma Preelaborate (System.Tasking.Yield);
