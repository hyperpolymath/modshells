-- src/main/modshells.adb
-- SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
with Shell_Manager;
with Config_Store;
with Ada.Text_IO;

procedure Modshells is

   Config_Path : constant String := Config_Store.Get_Modshell_Root_Path;
   Shells      : Shell_Manager.Shell_List := Shell_Manager.Detect_Shells;

begin
   Ada.Text_IO.Put_Line ("=== Modshells v0.1 ===");
   Ada.Text_IO.Put_Line ("Configuration path: " & Config_Path);
   Ada.Text_IO.New_Line;

   -- Step 1: Idempotent creation of directories (core, tools, misc, os, ui)
   Ada.Text_IO.Put_Line ("Creating modular directory structure...");
   Shell_Manager.Create_Modshell_Directories (Root_Path => Config_Path);
   Ada.Text_IO.Put_Line ("  Directories ready: core, tools, misc, os, ui");
   Ada.Text_IO.New_Line;

   -- Step 2: Detect installed shells
   Ada.Text_IO.Put_Line ("Detecting installed shells...");
   for I in Shells'Range loop
      declare
         Status_Str : constant String :=
           (if Shells (I).Status = Shell_Manager.Installed then "[installed]"
            else "[not found]");
      begin
         Ada.Text_IO.Put_Line ("  " & Shell_Manager.To_String (Shells (I).Name) &
                               ": " & Status_Str);
      end;
   end loop;
   Ada.Text_IO.New_Line;

   -- Step 3: Modularise all installed shells
   Ada.Text_IO.Put_Line ("Modularising shell configurations...");
   Shell_Manager.Modularise_All_Shells (Modshells_Path => Config_Path);
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("=== Modshells complete ===");
   Ada.Text_IO.Put_Line ("Add configuration snippets to the modular directories.");
   Ada.Text_IO.Put_Line ("Files are sourced alphabetically (use numeric prefixes for ordering).");

exception
   when others =>
      Ada.Text_IO.Put_Line ("FATAL ERROR: Modshells failed during setup.");
      raise;
end Modshells;