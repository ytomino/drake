pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Shared_Locking;
package body Ada.Containers.Copy_On_Write is

   procedure Follow (
      Target : not null access Container;
      Data : not null Data_Access);
   procedure Follow (
      Target : not null access Container;
      Data : not null Data_Access)
   is
      pragma Suppress (Accessibility_Check);
      --  if Target points local variable, accessibility check may failed.
   begin
      pragma Check (Trace, Debug.Put ("enter"));
      Target.Data := Data;
      if Data.Follower = null then
         pragma Assert (Target.Next_Follower = null);
         Data.Follower := Target;
      else
         --  keep first follower
         Target.Next_Follower := Data.Follower.Next_Follower;
         Data.Follower.Next_Follower := Target;
      end if;
      pragma Check (Trace, Debug.Put ("leave"));
   end Follow;

   procedure Unfollow (
      Target : not null access Container;
      To_Free : out Data_Access);
   procedure Unfollow (
      Target : not null access Container;
      To_Free : out Data_Access)
   is
      Data : constant Data_Access := Target.Data;
   begin
      pragma Check (Trace, Debug.Put ("enter"));
      To_Free := null;
      if Data.Follower = Target then
         Data.Follower := Target.Next_Follower;
         if Data.Follower = null then
            To_Free := Data;
         end if;
      else
         declare
            I : access Container := Data.Follower;
         begin
            while I.Next_Follower /= Target loop
               I := I.Next_Follower;
            end loop;
            I.Next_Follower := Target.Next_Follower;
         end;
      end if;
      Target.Next_Follower := null;
      Target.Data := null;
      pragma Check (Trace, Debug.Put ("leave"));
   end Unfollow;

   --  implementation

   function Shared (Data : not null Data_Access) return Boolean is
   begin
      return Data.Follower /= null
         and then Data.Follower.Next_Follower /= null;
   end Shared;

   procedure Adjust (
      Target : not null access Container)
   is
      Data : constant Data_Access := Target.Data;
   begin
      if Data /= null then
         System.Shared_Locking.Enter;
         Follow (Target, Data);
         System.Shared_Locking.Leave;
      end if;
   end Adjust;

   procedure Assign (
      Target : not null access Container;
      Source : not null access constant Container;
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Target.Data /= Source.Data then
         Clear (Target, Free);
         if Source.Data = null then
            Target.Data := null;
            pragma Assert (Target.Next_Follower = null);
         else
            System.Shared_Locking.Enter;
            Follow (Target, Source.Data);
            System.Shared_Locking.Leave;
         end if;
      end if;
   end Assign;

   procedure Clear (
      Target : not null access Container;
      Free : not null access procedure (Object : in out Data_Access))
   is
      Data : Data_Access := Target.Data;
   begin
      if Data /= null then
         declare
            To_Free : Data_Access;
         begin
            System.Shared_Locking.Enter;
            Unfollow (Target, To_Free);
            System.Shared_Locking.Leave;
            if To_Free /= null then
               Free (Data);
            end if;
         end;
      end if;
   end Clear;

   function Copy (
      Source : not null access constant Container;
      Length : Count_Type;
      Capacity : Count_Type;
      Allocate : not null access procedure (
         Target : out Data_Access;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type))
      return Container is
   begin
      return Result : Container := (null, null) do
         if Source.Data /= null then
            declare
               New_Data : Data_Access;
            begin
               Copy (New_Data, Source.Data, Length, Capacity);
               Follow (Result'Unrestricted_Access, New_Data); -- no sync
            end;
         elsif Capacity > 0 then
            declare
               New_Data : Data_Access;
            begin
               Allocate (New_Data, Capacity);
               Follow (Result'Unrestricted_Access, New_Data); -- no sync
            end;
         end if;
      end return;
   end Copy;

   procedure Move (
      Target : not null access Container;
      Source : not null access Container;
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Target /= Source then
         if Source.Data = null then
            Clear (Target, Free);
         else
            if Target.Data /= Source.Data then
               Clear (Target, Free);
               System.Shared_Locking.Enter;
               Follow (Target, Source.Data);
               System.Shared_Locking.Leave;
            end if;
            declare
               Dummy : Data_Access;
            begin
               System.Shared_Locking.Enter;
               Unfollow (Source, Dummy);
               System.Shared_Locking.Leave;
               pragma Assert (Dummy = null);
            end;
         end if;
      end if;
   end Move;

   procedure Unique (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type;
      Capacity : Count_Type;
      To_Update : Boolean;
      Allocate : not null access procedure (
         Target : out Data_Access;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Target.Data /= null then
         if Target.Data.Follower = Target
            and then Capacity = Target_Capacity
         then
            if To_Update then
               System.Shared_Locking.Enter;
               if Target.Next_Follower /= null then
                  --  detach next-followers
                  declare
                     New_Data : Data_Access;
                  begin
                     System.Shared_Locking.Leave; -- *B*
                     Copy (New_Data, Target.Data, Target_Length, Capacity);
                     System.Shared_Locking.Enter;
                     --  target uses old data, other followers use new data
                     if Target.Next_Follower = null then
                        Free (New_Data); -- unfollowed by other task at *B*
                     else
                        New_Data.Follower := Target.Next_Follower;
                        declare
                           I : access Container := Target.Next_Follower;
                        begin
                           while I /= null loop
                              I.Data := New_Data;
                              I := I.Next_Follower;
                           end loop;
                        end;
                     end if;
                     Target.Next_Follower := null;
                  end;
               end if;
               System.Shared_Locking.Leave;
            end if;
         else
            --  reallocation
            System.Shared_Locking.Enter;
            declare
               To_Copy : constant Boolean := Shared (Target.Data);
               New_Data : Data_Access;
               To_Free : Data_Access;
            begin
               System.Shared_Locking.Leave;
               if To_Copy then
                  Copy (New_Data, Target.Data, Target_Length, Capacity); -- *A*
               else
                  Move (New_Data, Target.Data, Target_Length, Capacity);
               end if;
               System.Shared_Locking.Enter;
               Unfollow (Target, To_Free);
               pragma Assert (To_Copy or else To_Free /= null);
               if To_Free /= null then
                  Free (To_Free); -- unfollowed by other task at *A*, or move
               end if;
               Follow (Target, New_Data);
            end;
            System.Shared_Locking.Leave;
         end if;
      else
         declare
            New_Data : Data_Access;
         begin
            Allocate (New_Data, Capacity);
            Follow (Target, New_Data); -- no sync
         end;
      end if;
   end Unique;

   procedure Set_Length (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type;
      New_Length : Count_Type;
      Allocate : not null access procedure (
         Target : out Data_Access;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access))
   is
      function Downcast is
         new Unchecked_Conversion (Data_Access, Data_Ex_Access);
   begin
      if New_Length > Target_Length then
         --  inscreasing
         if New_Length > Target_Capacity then
            --  expanding
            declare
               New_Capacity : constant Count_Type :=
                  Count_Type'Max (Target_Capacity * 2, New_Length);
            begin
               Unique (
                  Target,
                  Target_Length,
                  Target_Capacity,
                  New_Capacity,
                  False,
                  Allocate => Allocate,
                  Move => Move,
                  Copy => Copy,
                  Free => Free);
            end;
            Downcast (Target.Data).Max_Length := New_Length;
         else
            --  try to use reserved area
            pragma Assert (Target.Data /= null); -- Target_Capacity > 0
            System.Shared_Locking.Enter; -- emulate CAS
            if Downcast (Target.Data).Max_Length = Target_Length
               and then not Downcast (Target.Data).Is_Aliased
            then
               declare
                  pragma Suppress (Accessibility_Check);
                  Source : constant not null access Container :=
                     Target.Data.Follower;
                  To_Free : Data_Access;
               begin
                  if Target /= Source then
                     pragma Assert (Target.Data = Source.Data);
                     pragma Assert (Source.Data.Follower = Source);
                     Unfollow (Target, To_Free);
                     pragma Assert (To_Free = null); -- Source still have it
                     Target.Data := Source.Data;
                     Target.Next_Follower := Source;
                     Source.Data.Follower := Target;
                  end if;
               end;
               Downcast (Target.Data).Max_Length := New_Length;
               System.Shared_Locking.Leave;
            else
               System.Shared_Locking.Leave;
               Unique (
                  Target,
                  Target_Length,
                  Target_Capacity,
                  Target_Capacity,
                  False,
                  Allocate => Allocate,
                  Move => Move,
                  Copy => Copy,
                  Free => Free);
               Downcast (Target.Data).Max_Length := New_Length;
            end if;
         end if;
      else
         --  decreasing
         if Target.Data /= null and then not Shared (Target.Data) then
            Downcast (Target.Data).Max_Length := New_Length;
         end if;
      end if;
   end Set_Length;

end Ada.Containers.Copy_On_Write;
