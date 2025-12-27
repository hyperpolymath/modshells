-- tests/test_shell_manager.adb
-- SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
-- Unit tests for Shell_Manager package

with Ada.Text_IO;
with Ada.Directories;
with Shell_Manager;

procedure Test_Shell_Manager is

   use Ada.Text_IO;
   use Ada.Directories;

   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   procedure Pass (Name : String) is
   begin
      Tests_Passed := Tests_Passed + 1;
      Put_Line ("PASS: " & Name);
   end Pass;

   procedure Fail (Name : String; Reason : String := "") is
   begin
      Tests_Failed := Tests_Failed + 1;
      Put ("FAIL: " & Name);
      if Reason'Length > 0 then
         Put (" - " & Reason);
      end if;
      New_Line;
   end Fail;

   -- Test: To_String returns correct values
   procedure Test_To_String is
   begin
      Tests_Run := Tests_Run + 1;
      if Shell_Manager.To_String (Shell_Manager.Bash) = "bash" and
         Shell_Manager.To_String (Shell_Manager.Zsh) = "zsh" and
         Shell_Manager.To_String (Shell_Manager.Nushell) = "nushell" and
         Shell_Manager.To_String (Shell_Manager.Fish) = "fish"
      then
         Pass ("To_String returns correct shell names");
      else
         Fail ("To_String returns incorrect values");
      end if;
   end Test_To_String;

   -- Test: Get_Shell_Binary_Name returns correct binary names
   procedure Test_Get_Shell_Binary_Name is
   begin
      Tests_Run := Tests_Run + 1;
      if Shell_Manager.Get_Shell_Binary_Name (Shell_Manager.Bash) = "bash" and
         Shell_Manager.Get_Shell_Binary_Name (Shell_Manager.Nushell) = "nu" and
         Shell_Manager.Get_Shell_Binary_Name (Shell_Manager.Oils) = "osh" and
         Shell_Manager.Get_Shell_Binary_Name (Shell_Manager.Pwsh) = "pwsh"
      then
         Pass ("Get_Shell_Binary_Name returns correct binaries");
      else
         Fail ("Get_Shell_Binary_Name returns incorrect values");
      end if;
   end Test_Get_Shell_Binary_Name;

   -- Test: Detect_Shells returns all shell types
   procedure Test_Detect_Shells is
      Shells : constant Shell_Manager.Shell_List := Shell_Manager.Detect_Shells;
   begin
      Tests_Run := Tests_Run + 1;
      if Shells'Length = Shell_Manager.Max_Shells then
         Pass ("Detect_Shells returns all " & Natural'Image (Shell_Manager.Max_Shells) & " shells");
      else
         Fail ("Detect_Shells returned wrong count",
               "Expected" & Natural'Image (Shell_Manager.Max_Shells) &
               ", got" & Natural'Image (Shells'Length));
      end if;
   end Test_Detect_Shells;

   -- Test: Create_Modshell_Directories creates structure
   procedure Test_Create_Directories is
      Test_Path : constant String := "/tmp/modshells-test-" &
                                     Natural'Image (Natural (Ada.Directories.Size ("/dev/null")))(2 .. 6);
      All_Exist : Boolean := True;
   begin
      Tests_Run := Tests_Run + 1;

      -- Clean up if exists
      if Exists (Test_Path) then
         Delete_Tree (Test_Path);
      end if;

      -- Create directories
      Shell_Manager.Create_Modshell_Directories (Test_Path);

      -- Check all subdirectories exist
      for Dir of (1 => "core", 2 => "tools", 3 => "misc", 4 => "os", 5 => "ui") loop
         if not Exists (Test_Path & "/" & Dir) then
            All_Exist := False;
         end if;
      end loop;

      if All_Exist then
         Pass ("Create_Modshell_Directories creates all subdirectories");
      else
         Fail ("Create_Modshell_Directories missing subdirectories");
      end if;

      -- Cleanup
      if Exists (Test_Path) then
         Delete_Tree (Test_Path);
      end if;

   exception
      when others =>
         Fail ("Create_Modshell_Directories raised exception");
         if Exists (Test_Path) then
            Delete_Tree (Test_Path);
         end if;
   end Test_Create_Directories;

   -- Test: Get_Sourcing_Block returns non-empty string
   procedure Test_Get_Sourcing_Block is
      Block : constant String := Shell_Manager.Get_Sourcing_Block
        (Shell_Manager.Bash, "/test/path");
   begin
      Tests_Run := Tests_Run + 1;
      if Block'Length > 100 and then
         (for some I in Block'Range => Block (I) = ASCII.LF)
      then
         Pass ("Get_Sourcing_Block returns valid multi-line block");
      else
         Fail ("Get_Sourcing_Block returned empty or single-line");
      end if;
   end Test_Get_Sourcing_Block;

   -- Test: Sourcing block contains signature
   procedure Test_Sourcing_Block_Signature is
      Block : constant String := Shell_Manager.Get_Sourcing_Block
        (Shell_Manager.Bash, "/test/path");
      Has_Start : Boolean := False;
      Has_End   : Boolean := False;
   begin
      Tests_Run := Tests_Run + 1;

      -- Check for MODSHELLS_START
      for I in Block'First .. Block'Last - 14 loop
         if Block (I .. I + 14) = "MODSHELLS_START" then
            Has_Start := True;
            exit;
         end if;
      end loop;

      -- Check for MODSHELLS_END
      for I in Block'First .. Block'Last - 12 loop
         if Block (I .. I + 12) = "MODSHELLS_END" then
            Has_End := True;
            exit;
         end if;
      end loop;

      if Has_Start and Has_End then
         Pass ("Sourcing block contains signature markers");
      else
         Fail ("Sourcing block missing signature markers");
      end if;
   end Test_Sourcing_Block_Signature;

begin
   Put_Line ("==========================================");
   Put_Line ("Shell_Manager Unit Tests");
   Put_Line ("==========================================");
   New_Line;

   -- Run all tests
   Test_To_String;
   Test_Get_Shell_Binary_Name;
   Test_Detect_Shells;
   Test_Create_Directories;
   Test_Get_Sourcing_Block;
   Test_Sourcing_Block_Signature;

   -- Summary
   New_Line;
   Put_Line ("==========================================");
   Put_Line ("Test Summary");
   Put_Line ("==========================================");
   Put_Line ("Tests run:   " & Natural'Image (Tests_Run));
   Put_Line ("Tests passed:" & Natural'Image (Tests_Passed));
   Put_Line ("Tests failed:" & Natural'Image (Tests_Failed));
   New_Line;

   if Tests_Failed = 0 then
      Put_Line ("All tests passed!");
   else
      Put_Line ("Some tests failed.");
   end if;

end Test_Shell_Manager;
