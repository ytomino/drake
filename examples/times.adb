with Ada.Execution_Time;
with Ada.Real_Time;
procedure times is
	use type Ada.Execution_Time.CPU_Time;
	use type Ada.Real_Time.Time;
	First_RT : Ada.Real_Time.Time := Ada.Real_Time.Clock;
	First_CT : Ada.Execution_Time.CPU_Time := Ada.Execution_Time.Clock;
	Now_RT : Ada.Real_Time.Time;
	Now_CT : Ada.Execution_Time.CPU_Time;
	procedure printf (f : String; v : Long_Float);
	pragma Import (C, printf);
begin
	delay 1.0;
	Now_RT := Ada.Real_Time.Clock;
	Now_CT := Ada.Execution_Time.Clock;
	printf ("Real Time: %F" & ASCII.LF & ASCII.NUL,
		Long_Float (Ada.Real_Time.To_Duration (Now_RT - First_RT)));
	printf ("CPU Time: %F" & ASCII.LF & ASCII.NUL,
		Long_Float (Ada.Real_Time.To_Duration (Now_CT - First_CT)));
end times;
