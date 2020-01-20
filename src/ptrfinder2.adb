with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with Langkit_Support.Slocs;
with Libadalang.Analysis;   use Libadalang.Analysis;
with Libadalang.Common;     use Libadalang.Common;
with Libadalang.Helpers;

procedure ptrfinder2 is

   package Helpers renames Libadalang.Helpers;
   package LAL renames Libadalang.Analysis;
   package Slocs renames Langkit_Support.Slocs;

   function Verify
     (Filename : String; Line : Slocs.Line_Number) return Boolean;
   --  Return whether Filename can be read and that its Line'th line contains
   --  the " access " substring.

   procedure Report (Node : LAL.Ada_Node'Class);
   --  Report the use of an access type at Filename/Line_Number on the standard
   --  output. If --verify is enabled, check that the first source line
   --  corresponding to Node contains the " access " substring.

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);
   --  Look for the use of access types in Unit

   package App is new Helpers.App
     (Name         => "ptrfinder2",
      Description  => "Look for the use of access types in the input sources",
      Process_Unit => Process_Unit);

   package Do_Verify is new GNATCOLL.Opt_Parse.Parse_Flag
     (App.Args.Parser,
      Long => "--verify",
      Help => "Verify detected ""access"" occurences");

   ------------
   -- Verify --
   ------------

   function Verify
     (Filename : String; Line : Slocs.Line_Number) return Boolean
   is
      --  Here, we could directly look for an "access" token in the list of
      --  tokens corresponding to Line in this unit. However, in the spirit of
      --  the original program, re-read the file with Ada.Text_IO.

      Found : Boolean := False;
      --  Whether we have found the substring on the expected line

      File : File_Type;
      --  File to read (Filename)
   begin
      Open (File, In_File, Filename);
      for I in 1 .. Line loop
         declare
            use type Slocs.Line_Number;

            Line_Content : constant String := Get_Line (File);
         begin
            if I = Line
               and then Ada.Strings.Fixed.Index (Line_Content, " access ") > 0
            then
               Found := True;
            end if;
         end;
      end loop;
      Close (File);
      return Found;
   exception
      when Use_Error | Name_Error | Device_Error =>
         Close (File);
         return Found;
   end Verify;

   ------------
   -- Report --
   ------------

   procedure Report (Node : LAL.Ada_Node'Class) is
      Filename   : constant String := Node.Unit.Get_Filename;
      Line       : constant Slocs.Line_Number :=
         Node.Sloc_Range.Start_Line;
      Line_Image : constant String :=
         Ada.Strings.Fixed.Trim (Line'Image, Ada.Strings.Left);
   begin
      if Do_Verify.Get then
         if Verify (Filename, Line) then
            Put_Line ("Access Type Verified on line #"
                      & Line_Image & " of " & Filename);
         else
            Put_Line ("Suspected Access Type *NOT* Verified on line #"
                      & Line_Image & " of " & Filename);
         end if;

      else
         Put_Line (Filename & ":" & Line_Image);
      end if;
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
end ptrfinder2;
