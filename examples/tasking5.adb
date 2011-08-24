with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Task_Identification;
with System.Tasking.Inside;
procedure tasking5 is
	function Cast is new Ada.Unchecked_Conversion (
		Ada.Task_Identification.Task_Id,
		System.Tasking.Inside.Task_Id);
	function Cast is new Ada.Unchecked_Conversion (
		System.Tasking.Inside.Task_Id,
		Ada.Task_Identification.Task_Id);
	Joined : Boolean;
	pragma Volatile (Joined);
begin
	Ada.Debug.Put ("*** test for simple task syntax ***");
	declare
		task T1 is
		end T1;
		task body T1 is
		begin
			delay 0.2; -- long processing...
			Ada.Debug.Put (Ada.Task_Identification.Image (
				Ada.Task_Identification.Current_Task));
			Joined := True;
		end T1;
		Id : Ada.Task_Identification.Task_Id := T1'Identity;
	begin
		Joined := False;
		Ada.Debug.Put (Ada.Task_Identification.Image (Id));
		-- wait end of T1 here
	end;
	pragma Assert (Joined);
	Ada.Debug.Put ("*** test for nesting ***");
	declare
		task type Nested_Task;
		task body Nested_Task is
		begin
			delay 0.2; -- long processing...
			Ada.Debug.Put ("nested: "
				& Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task));
			Ada.Debug.Put ("parent of nested: "
				& Ada.Task_Identification.Image (
					Cast (System.Tasking.Inside.Parent (Cast (Ada.Task_Identification.Current_Task)))));
			Ada.Debug.Put ("master level of nested: "
				& Integer'Image (System.Tasking.Inside.Master_Level_Of (Cast (Ada.Task_Identification.Current_Task))));
			Joined := True;
		end Nested_Task;
		task T2 is
		end T2;
		task body T2 is
		begin
			Ada.Debug.Put ("T2: "
				& Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task));
			Ada.Debug.Put ("parent of T2: "
				& Ada.Task_Identification.Image (
					Cast (System.Tasking.Inside.Parent (Cast (Ada.Task_Identification.Current_Task)))));
			Ada.Debug.Put ("master level of T2: "
				& Integer'Image (System.Tasking.Inside.Master_Level_Of (Cast (Ada.Task_Identification.Current_Task))));
			declare
				A : Nested_Task;
			begin
				null;
			end;
			pragma Assert (Joined);
		end T2;
	begin
		Joined := False;
		Ada.Debug.Put ("main: "
			& Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task));
		Ada.Debug.Put ("master level of main: "
			& Integer'Image (System.Tasking.Inside.Master_Level_Of (Cast (Ada.Task_Identification.Current_Task))));
	end;
	pragma Assert (Joined);
	Ada.Debug.Put ("*** test for dynamic allocation ***");
	declare
		task type Dynamic_Task;
		task body Dynamic_Task is
		begin
			delay 0.2; -- long processing...
			Ada.Debug.Put ("dynamic: "
				& Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task));
			Ada.Debug.Put ("parent of dynamic: "
				& Ada.Task_Identification.Image (
					Cast (System.Tasking.Inside.Parent (Cast (Ada.Task_Identification.Current_Task)))));
			Ada.Debug.Put ("master level of dynamic: "
				& Integer'Image (System.Tasking.Inside.Master_Level_Of (Cast (Ada.Task_Identification.Current_Task))));
			Joined := True;
		end Dynamic_Task;
		type DA is access Dynamic_Task;
		D : DA := null;
		task T3 is
		end T3;
		task body T3 is
		begin
			Ada.Debug.Put ("T3: "
				& Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task));
			Ada.Debug.Put ("parent of T3: "
				& Ada.Task_Identification.Image (
					Cast (System.Tasking.Inside.Parent (Cast (Ada.Task_Identification.Current_Task)))));
			Ada.Debug.Put ("master level of T3: "
				& Integer'Image (System.Tasking.Inside.Master_Level_Of (Cast (Ada.Task_Identification.Current_Task))));
			D := new Dynamic_Task;
			pragma Assert (not Joined);
		end T3;
	begin
		Joined := False;
		Ada.Debug.Put ("main: "
			& Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task));
		Ada.Debug.Put ("master level of main: "
			& Integer'Image (System.Tasking.Inside.Master_Level_Of (Cast (Ada.Task_Identification.Current_Task))));
		-- wait end of T3 and Dynamic_Task here
		-- memory of Dynamic_Task be leak !!!
	end;
	pragma Assert (Joined);
	Ada.Debug.Put ("*** test for dynamic allocation/deallocation ***");
	declare
		task type Dynamic_Task;
		task body Dynamic_Task is
		begin
			Ada.Debug.Put (Ada.Task_Identification.Image (
				Ada.Task_Identification.Current_Task));
		end Dynamic_Task;
		type DA is access Dynamic_Task;
		procedure Free is new Ada.Unchecked_Deallocation (Dynamic_Task, DA);
		D : DA := new Dynamic_Task;
	begin
		Free (D);
	end;
	Ada.Debug.Put ("*** test for build-in-place ***");
	Joined := False;
	declare
		task type Limited_Task;
		task body Limited_Task is
		begin
			Ada.Debug.Put (Ada.Task_Identification.Image (
				Ada.Task_Identification.Current_Task));
			Joined := True;
		end Limited_Task;
		function Create return Limited_Task is
		begin
			return Result : Limited_Task do
				delay 0.2;
				-- not started here
				pragma Assert (not Joined);
			end return;
		end Create;
		T : Limited_Task := Create;
		-- start here
	begin
		null;
		-- wait here
	end;
	Ada.Debug.Put ("*** test for no activation ***");
	begin
		declare
			task NAT;
			task body NAT is
			begin
				raise Program_Error;
			end NAT;
			A : Integer := Integer'Value ("###"); -- raise Constraint_Error
		begin
			raise Program_Error;
		end;
	exception
		when Constraint_Error => null;
	end;
	pragma Assert (Joined);
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking5;
