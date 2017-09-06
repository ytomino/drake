pragma Check_Policy (Trace => Ignore);
with System.Shared_Locking;
package body Ada.Containers.Copy_On_Write is

   procedure Follow (
      Target : not null Container_Access;
      Data : not null Data_Access);
   procedure Follow (
      Target : not null Container_Access;
      Data : not null Data_Access) is
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
      Target : not null Container_Access;
      To_Free : out Data_Access);
   procedure Unfollow (
      Target : not null Container_Access;
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

   Constant_Zero : aliased constant Count_Type := 0;

   --  implementation

   function Shared (Data : Data_Access) return Boolean is
      Result : Boolean;
   begin
      if Data = null then
         Result := True; -- null as sentinel
      else
         pragma Assert (Data.Follower /= null);
         Result := Data.Follower.Next_Follower /= null;
      end if;
      return Result;
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
            pragma Assert (Target.Data = null);
            pragma Assert (Target.Next_Follower = null);
            null;
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

   procedure Copy (
      Target : not null access Container;
      Source : not null access constant Container;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         New_Length : Count_Type;
         Capacity : Count_Type)) is
   begin
      Copy_On_Write.Copy (Target, Source, 0, 0,
         Allocate => Allocate, Copy => Copy);
   end Copy;

   procedure Copy (
      Target : not null access Container;
      Source : not null access constant Container;
      Length : Count_Type;
      New_Capacity : Count_Type;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         New_Length : Count_Type;
         Capacity : Count_Type)) is
   begin
      pragma Assert (Target.all = (null, null));
      if Source.Data /= null then
         declare
            New_Data : Data_Access;
         begin
            Copy (New_Data, Source.Data, Length, Length, New_Capacity);
            Follow (Target, New_Data); -- no sync
         end;
      elsif New_Capacity > 0 then
         declare
            New_Data : Data_Access;
         begin
            Allocate (New_Data, Length, New_Capacity);
            Follow (Target, New_Data); -- no sync
         end;
      end if;
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
      To_Update : Boolean;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      Unique (Target, 0, 0, 0, 0, To_Update,
         Allocate => Allocate,
         Move => Move,
         Copy => Copy,
         Free => Free,
         Max_Length => Zero'Access);
   end Unique;

   procedure Unique (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type;
      New_Length : Count_Type;
      New_Capacity : Count_Type;
      To_Update : Boolean;
      Allocate : not null access procedure (
         Target : out not null Data_Access;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Move : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Count_Type;
         New_Length : Count_Type;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access);
      Max_Length : not null access function (Data : not null Data_Access)
         return not null access Count_Type) is
   begin
      if Target.Data /= null then
         if Target.Data.Follower = Target
            and then New_Capacity = Target_Capacity
         then
            if To_Update then
               if Target.Next_Follower /= null then -- shared
                  declare -- target(owner) can access its Max_Length
                     Max_Length : constant Count_Type :=
                        Unique.Max_Length (Target.Data).all;
                     New_Data : Data_Access;
                     To_Free : Data_Access := null;
                  begin
                     Copy (
                        New_Data,
                        Target.Data,
                        Max_Length,
                        Max_Length,
                        Target_Capacity); -- not shrinking
                     --  target uses old data, other followers use new data
                     System.Shared_Locking.Enter;
                     if Target.Next_Follower = null then
                        --  all other containers unfollowed it by another task
                        To_Free := New_Data;
                     else
                        --  reattach next-followers to new copied data
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
                     System.Shared_Locking.Leave;
                     if To_Free /= null then
                        Free (To_Free);
                     end if;
                  end;
               end if;
            end if;
         else
            --  reallocation
            declare
               To_Copy : constant Boolean :=
                  Target.Data.Follower.Next_Follower /= null; -- shared
                  --  should this reading be locked or not?
               New_Data : Data_Access;
               To_Free : Data_Access;
            begin
               if To_Copy then
                  Copy (
                     New_Data,
                     Target.Data,
                     Target_Length,
                     New_Length,
                     New_Capacity);
               else
                  Move (
                     New_Data,
                     Target.Data,
                     Target_Length,
                     New_Length,
                     New_Capacity);
               end if;
               System.Shared_Locking.Enter;
               Unfollow (Target, To_Free);
               Follow (Target, New_Data);
               System.Shared_Locking.Leave;
               pragma Assert (To_Copy or else To_Free /= null);
               if To_Free /= null then
                  --  all containers unfollowed it by another task, or move
                  Free (To_Free);
               end if;
            end;
         end if;
      else
         declare
            New_Data : Data_Access;
         begin
            Allocate (New_Data, New_Length, New_Capacity);
            Follow (Target, New_Data); -- no sync
         end;
      end if;
   end Unique;

   procedure In_Place_Set_Length (
      Target : not null access Container;
      Target_Length : Count_Type;
      Target_Capacity : Count_Type;
      New_Length : Count_Type;
      Failure : out Boolean;
      Max_Length : not null access function (Data : not null Data_Access)
         return not null access Count_Type) is
   begin
      if New_Length > Target_Length then
         --  inscreasing
         if New_Length > Target_Capacity then
            --  expanding
            Failure := True; -- should be reallocated
         else
            --  try to use reserved area
            pragma Assert (Target.Data /= null); -- Target_Capacity > 0
            if Target.Data.Follower.Next_Follower = null then -- not shared
               Max_Length (Target.Data).all := New_Length;
               Failure := False; -- success
            elsif Target.Data.Follower = Target then
               declare -- the owner only can access its Max_Length
                  Max_Length : constant not null access Count_Type :=
                     In_Place_Set_Length.Max_Length (Target.Data);
               begin
                  if Max_Length.all = Target_Length then
                     Max_Length.all := New_Length;
                     Failure := False; -- success
                  else
                     Failure := True; -- should be copied
                  end if;
               end;
            else
               Failure := True; -- should be copied
            end if;
         end if;
      else
         --  decreasing
         if not Shared (Target.Data) then
            Max_Length (Target.Data).all := New_Length;
         end if;
         Failure := False; -- success
      end if;
   end In_Place_Set_Length;

   function Zero (Data : not null Data_Access)
      return not null access Count_Type
   is
      pragma Unreferenced (Data);
   begin
      return Constant_Zero'Unrestricted_Access;
   end Zero;

end Ada.Containers.Copy_On_Write;
