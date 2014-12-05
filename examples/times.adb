with Ada.Execution_Time;
with Ada.Real_Time;
with Ada.Text_IO;
procedure times is
	package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);
	use Ada.Text_IO, Duration_IO;
	use type Ada.Execution_Time.CPU_Time;
	use type Ada.Real_Time.Time;
	use type Ada.Real_Time.Time_Span;
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
	-- Time_Span
	pragma Assert (Ada.Real_Time.To_Time_Span (1.0) + Ada.Real_Time.To_Time_Span (2.0) = Ada.Real_Time.To_Time_Span (3.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (3.0) - Ada.Real_Time.To_Time_Span (2.0) = Ada.Real_Time.To_Time_Span (1.0));
	pragma Assert (-Ada.Real_Time.To_Time_Span (1.0) = Ada.Real_Time.To_Time_Span (-1.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (2.0) * 3 = Ada.Real_Time.To_Time_Span (6.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (6.0) / Ada.Real_Time.To_Time_Span (2.0) = 3);
	pragma Assert (Ada.Real_Time.To_Time_Span (6.0) / 2 = Ada.Real_Time.To_Time_Span (3.0));
	pragma Assert (abs Ada.Real_Time.To_Time_Span (1.0) = Ada.Real_Time.To_Time_Span (1.0));
	pragma Assert (abs Ada.Real_Time.To_Time_Span (-1.0) = Ada.Real_Time.To_Time_Span (1.0));
	-- Time and Time_Span
	pragma Assert (Now_RT + Ada.Real_Time.To_Time_Span (2.0) - Now_RT = Ada.Real_Time.To_Time_Span (2.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (2.0) + Now_RT = Now_RT + Ada.Real_Time.To_Time_Span (2.0));
	pragma Assert (Now_RT - Ada.Real_Time.To_Time_Span (2.0) = Now_RT + Ada.Real_Time.To_Time_Span (-2.0));
	pragma Debug (Ada.Debug.Put ("OK"));
end times;
