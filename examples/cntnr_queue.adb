with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Unbounded_Priority_Queues;
procedure cntnr_queue is
	package Integer_Queue_Interfaces is
		new Ada.Containers.Synchronized_Queue_Interfaces (Character);
	procedure Test (
		X : in out Integer_Queue_Interfaces.Queue'Class;
		Input : String;
		Output : String)
	is
		Buf : String (Input'Range);
		J : Positive := Buf'First;
	begin
		for I in Input'Range loop
			Integer_Queue_Interfaces.Enqueue (X, Input (I));
		end loop;
		while Integer_Queue_Interfaces.Current_Use (X) > 0 loop
			Integer_Queue_Interfaces.Dequeue (X, Buf (J));
			J := J + 1;
		end loop;
		pragma Assert (J = Buf'Last + 1);
		pragma Assert (Buf = Output);
		pragma Assert (Integer_Queue_Interfaces.Peak_Use (X) = Input'Length);
	end Test;
	package Integer_Queues is
		new Ada.Containers.Unbounded_Synchronized_Queues (Integer_Queue_Interfaces);
	type Character_Priority is mod 256;
	function Get_Priority (C : Character) return Character_Priority is
	begin
		return Character'Pos (C);
	end Get_Priority;
	package Integer_Priority_Queues is
		new Ada.Containers.Unbounded_Priority_Queues (
			Integer_Queue_Interfaces,
			Queue_Priority => Character_Priority,
			Get_Priority => Get_Priority,
			Before => ">");
begin
	declare
		X : Integer_Queues.Queue;
	begin
		Test (X, "ABC", "ABC");
	end;
	declare
		X : Integer_Priority_Queues.Queue;
	begin
		Test (X, "ABC", "CBA");
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end cntnr_queue;
