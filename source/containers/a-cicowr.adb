pragma Check_Policy (Trace, Off);
package body Ada.Containers.Inside.Copy_On_Write is
   --  should synchronize... unimplemented

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
         pragma Check (Trace, Debug.Put ("1"));
      else
         --  keep first follower
         Target.Next_Follower := Data.Follower.Next_Follower;
         Data.Follower.Next_Follower := Target;
         pragma Check (Trace, Debug.Put ("2"));
      end if;
      pragma Check (Trace, Debug.Put ("leave"));
   end Follow;

   procedure Unfollow (
      Target : not null access Container)
   is
      Data : constant Data_Access := Target.Data;
   begin
      pragma Check (Trace, Debug.Put ("enter"));
      if Data.Follower = Target then
         Data.Follower := Target.Next_Follower;
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

   procedure Unique (
      Target : not null access Container;
      To_Update : Boolean;
      Capacity : Count_Type;
      Allocate : not null access procedure (Target : out Data_Access);
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Capacity : Count_Type)) is
   begin
      if Target.Data /= null then
         if Target.Data.Follower /= Target then
            declare
               New_Data : Data_Access;
            begin
               Copy (New_Data, Target.Data, Capacity);
               Unfollow (Target);
               Follow (Target, New_Data);
            end;
         elsif To_Update and then Target.Next_Follower /= null then
            --  detach next-followers
            declare
               New_Data : Data_Access;
            begin
               Copy (New_Data, Target.Data, Capacity);
               New_Data.Follower := Target.Next_Follower;
               declare
                  I : access Container := Target.Next_Follower;
               begin
                  while I /= null loop
                     I.Data := New_Data;
                     I := I.Next_Follower;
                  end loop;
               end;
            end;
         end if;
      else
         declare
            New_Data : Data_Access;
         begin
            Allocate (New_Data);
            Follow (Target, New_Data);
         end;
      end if;
   end Unique;

   procedure Adjust (
      Target : not null access Container)
   is
      Data : constant Data_Access := Target.Data;
   begin
      if Data /= null then
         Follow (Target, Data);
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
            Follow (Target, Source.Data);
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
         Unfollow (Target);
         if Data.Follower = null then
            Free (Data);
         end if;
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
               Follow (Result'Access, New_Data);
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
               Follow (Target, Source.Data);
            end if;
            Unfollow (Source);
         end if;
      end if;
   end Move;

end Ada.Containers.Inside.Copy_On_Write;
