with Interfaces.C.Strings;
with System;
package body Sdefault is
   pragma Style_Checks (Off);
   S1 : aliased constant String := "<unknown>";
   S2 : aliased constant String := "<unknown>";
   S3 : aliased constant String := "<unknown>";
   S4 : aliased constant String := "<unknown>";

   function Include_Dir_Default_Name return String_Ptr is
   begin
      return new String'(S1);
   end Include_Dir_Default_Name;
   function Object_Dir_Default_Name return String_Ptr is
   begin
      return new String'(S2);
   end Object_Dir_Default_Name;
   function Target_Name return String_Ptr is
   begin
      return new String'(S3);
   end Target_Name;
   function Search_Dir_Prefix return String_Ptr is
   begin
      return new String'(S4);
   end Search_Dir_Prefix;


   --  Provide dummy C functions

   pragma Warnings (Off); -- passing unconstrained array to C function.
   procedure set_std_prefix (S : String; Len : Integer);
   pragma Export (C, set_std_prefix, "set_std_prefix");
   procedure set_std_prefix (S : String; Len : Integer)
   is
      --  spec copied from $GPS/gnat_src/osint.adb
      --  body copied from $GPS/common/src/set_std_prefix.c
   begin
      null;
   end set_std_prefix;
   pragma Warnings (Off);

   function C_Update_Path
     (Path, Component : System.Address)
     return System.Address;
   pragma Export (C, C_Update_Path, "update_path");
   function C_Update_Path
     (Path, Component : System.Address)
     return System.Address
   is
      --  spec copied from $GPS/gnat_src/osint.adb
      --  copied from $GPS/common/src/update_path.c
   begin
      return Path;
   end C_Update_Path;

   --  spec copied from $GPS/gnat_src/mlib.adb
   --  body copied from $GPS/common/src/run_path_option.c
   Run_Path_Option_Ptr : Interfaces.C.Strings.chars_ptr :=
     Interfaces.C.Strings.New_String ("");
   pragma Export (C, Run_Path_Option_Ptr, "__gnat_run_path_option");

end Sdefault;
