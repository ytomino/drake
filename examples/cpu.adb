with Ada;
with System.Multiprocessors;
procedure cpu is
begin
	Ada.Debug.Put (System.Multiprocessors.CPU'Image (System.Multiprocessors.Number_Of_CPUs));
end cpu;
