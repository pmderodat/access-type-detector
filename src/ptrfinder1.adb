with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs;
with Libadalang.Analysis;   use Libadalang.Analysis;
with Libadalang.Common;     use Libadalang.Common;
with Libadalang.Helpers;

procedure ptrfinder1 is

   package Helpers renames Libadalang.Helpers;
   package LAL renames Libadalang.Analysis;
   package Slocs renames Langkit_Support.Slocs;

   procedure Report (Node : LAL.Ada_Node'Class);
   --  Report the use of an access type at Filename/Line_Number on the standard
   --  output.

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);
   --  Look for the use of access types in Unit

   package App is new Helpers.App
     (Name         => "ptrfinder1",
      Description  => "Look for the use of access types in the input sources",
      Process_Unit => Process_Unit);

   ------------
   -- Report --
   ------------

   procedure Report (Node : LAL.Ada_Node'Class) is
      Filename : constant String := Node.Unit.Get_Filename;
      Line     : constant Slocs.Line_Number :=
         Node.Sloc_Range.Start_Line;
   begin
      Put_Line (Filename & ":"
                & Ada.Strings.Fixed.Trim (Line'Image, Ada.Strings.Left));
   end Report;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      pragma Unreferenced (Job_Ctx);

      function Process_Node (Node : Ada_Node'Class) return Visit_Status;
      --  Callback for LAL.Traverse

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
            when Ada_Base_Type_Decl =>
               if Node.As_Base_Type_Decl.P_Is_Access_Type then
                  Report (Node);
               end if;

            when Ada_Object_Decl =>
               if Node.As_Object_Decl.F_Type_Expr
                  .P_Designated_Type_Decl.P_Is_Access_Type
               then
                  Report (Node);
               end if;

            when others =>

               --  Nothing interesting was found in this Node so continue
               --  processing it for other violations.

               return Into;
         end case;

         --  A violation was detected, skip over any further processing of this
         --  node.

         return Over;
      end Process_Node;

   begin
      if not Unit.Has_Diagnostics then
         Unit.Root.Traverse (Process_Node'Access);
      end if;
   end Process_Unit;

begin
   App.Run;
end ptrfinder1;
