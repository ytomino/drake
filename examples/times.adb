with Ada.Execution_Time;
with Ada.Real_Time;
with Ada.Text_IO;
procedure times is
	package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);
	use Ada.Text_IO, Duration_IO;
	use type Ada.Execution_Time.CPU_Time;
	use type Ada.Real_Time.Time;
	First_RT : Ada.Real_Time.Time := Ada.Real_Time.Clock;
	First_CT : Ada.Execution_Time.CPU_Time := Ada.Execution_Time.Clock;
	Now_RT : Ada.Real_Time.Time;
	Now_CT : Ada.Execution_Time.CPU_Time;
begin
	delay 1.0;
	Now_RT := Ada.Real_Time.Clock;
	Now_CT := Ada.Execution_Time.Clock;
	Put ("Real Time: "); Put (Ada.Real_Time.To_Duration (Now_RT - First_RT)); New_Line;
	Put ("CPU Time:  "); PUt (Ada.Real_Time.To_Duration (Now_CT - First_CT)); New_Line;
end times;
