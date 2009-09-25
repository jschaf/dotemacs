--  Copyright (C) 2006, 2007 Free Software Foundation.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);


with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Csets;
with Errout;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Gnatvsn;
with Namet;
with Prj.Env;
with Prj.Part;
with Prj.Proc;
with Prj.Tree;
with Prj;
with Snames;
with Types;
procedure Gnatpath is

   procedure Help;
   procedure Help is
   begin
      Put_Line ("Usage: " & Command_Name & " list -v -Pproject_name.gpr");
      Put_Line ("  for GNAT version "& Gnatvsn.Gnat_Version_String);
      Put_Line ("  output paths from project file, " &
                  "in same format as 'gnat list -v'");
      Put_Line ("  but much faster");
   end Help;

   procedure Output_Path (Path : GNAT.OS_Lib.String_Access);
   procedure Output_Path (Path : GNAT.OS_Lib.String_Access)
   is
      use GNAT.Directory_Operations;
      Start, Last : Natural;
   begin
      Start := Path'First;
      while Start <= Path'Last loop
         Last := Start;
         while Last <= Path'Last
           and then Path (Last) /= GNAT.OS_Lib.Path_Separator
         loop
            Last := Last + 1;
         end loop;

         Put_Line ("    " &
                     Format_Pathname (Path (Start .. Last - 1), UNIX) & '/');
         Start := Last + 1;
      end loop;
   end Output_Path;

   procedure Output_Paths (Gpr_Filename : String);
   procedure Output_Paths (Gpr_Filename : String)
   is
      use GNAT.Directory_Operations;
      use Prj;
      use Prj.Tree;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Success      : Boolean;

      Tree      : constant Project_Node_Tree_Ref := new Project_Node_Tree_Data;
      View_Tree : constant Project_Tree_Ref      := new Project_Tree_Data;
   begin
      Namet.Initialize;
      Csets.Initialize;
      Snames.Initialize;
      Prj.Initialize (View_Tree);
      Prj.Tree.Initialize (Tree);
      Errout.Initialize;

      --  WORKAROUND: Errout.Initialize doesn't do this
      Errout.Error_Msg_Name_1 := Types.No_Name;
      Errout.Error_Msg_Name_2 := Types.No_Name;
      Errout.Error_Msg_Name_3 := Types.No_Name;

      Change_Dir (Dir_Name (Gpr_Filename));
      Prj.Part.Parse
        (Tree, Project, Gpr_Filename, Always_Errout_Finalize => True);
      Prj.Proc.Process
        (View_Tree, Project_View, Success, Project, Tree,
         Report_Error => null);
      Errout.Finalize;

      if Success then
         Put_Line ("Source Search Path:");
         Put_Line ("   <Current_Directory>");
         Output_Path (Prj.Env.Ada_Include_Path (Project_View, View_Tree));

         --  FIXME: also need runtime library path!

         New_Line;
         Put_Line ("Object Search Path:");
         Put_Line ("   <Current_Directory>");
         Output_Path (Prj.Env.Ada_Objects_Path (Project_View, View_Tree));

         --  FIXME: also need runtime library path!
      else
         Set_Exit_Status (Failure);
      end if;
   end Output_Paths;

begin
   if Argument_Count /= 3 then
      Help;
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Gpr_File : constant String := Ada.Command_Line.Argument (3);
   begin
      Output_Paths (Gpr_File (3 .. Gpr_File'Last));
   end;

exception
when E : others =>
   Put_Line ("unexpected exception " & Ada.Exceptions.Exception_Name (E));
   Set_Exit_Status (Failure);
end Gnatpath;
