pragma Check_Policy (Trace, Off);
with System.Shared_Locking;
package body Ada.Containers.Inside.Copy_On_Write is

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

   procedure Unique (
      Target : not null access Container;
      To_Update : Boolean;
      Capacity : Count_Type;
      Allocate : not null access procedure (Target : out Data_Access);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Capacity : Count_Type);
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Target.Data /= null then
         System.Shared_Locking.Enter;
         if Target.Data.Follower /= Target then
            declare
               New_Data : Data_Access;
               To_Free : Data_Access;
            begin
               System.Shared_Locking.Leave;
               Copy (New_Data, Target.Data, Capacity); -- *A*
               System.Shared_Locking.Enter;
               Unfollow (Target, To_Free);
               if To_Free /= null then
                  Free (To_Free); -- unfollowed by other task at *A*
               end if;
               Follow (Target, New_Data);
            end;
         elsif To_Update and then Target.Next_Follower /= null then
            --  detach next-followers
            declare
               New_Data : Data_Access;
            begin
               System.Shared_Locking.Leave;
               Copy (New_Data, Target.Data, Capacity); -- *B*
               System.Shared_Locking.Enter;
               --  target uses old data, other followers use new data
               New_Data.Follower := Target.Next_Follower;
               declare
                  I : access Container := Target.Next_Follower;
               begin
                  while I /= null loop
                     I.Data := New_Data;
                     I := I.Next_Follower;
                  end loop;
               end;
               Target.Next_Follower := null;
            end;
         end if;
         System.Shared_Locking.Leave;
      else
         declare
            New_Data : Data_Access;
         begin
            Allocate (New_Data);
            Follow (Target, New_Data); -- no sync
         end;
      end if;
   end Unique;

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
      Capacity : Count_Type;
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Capacity : Count_Type))
      return Container is
   begin
      return Result : aliased Container := (null, null) do
         if Source.Data /= null then
            declare
               New_Data : Data_Access;
            begin
               Copy (New_Data, Source.Data, Capacity);
               Follow (Result'Access, New_Data); -- no sync
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

end Ada.Containers.Inside.Copy_On_Write;
