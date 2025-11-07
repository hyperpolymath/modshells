-- src/shell_manager/shell_manager.adb
with GNAT.OS_Lib;
with Ada.Text_IO;
with Config_Store; -- Required for LMDB calls
use Ada.Text_IO;

package body Shell_Manager is

   function Detect_Shells return Shell_List is
      -- Simplification for v0.0: Return a known list.
      Shell_List_Data : constant Shell_List := (
         1 => (Name => Bash,    Status => Installed),
         2 => (Name => Zsh,     Status => Installed),
         3 => (Name => Nushell, Status => Installed),
         4 => (Name => Fish,    Status => Can_Be_Installed),
         5 => (Name => Pwsh,    Status => Not_Installed)
      );
   begin
      return Shell_List_Data;
   end Detect_Shells;
   
   function To_String(Shell : Shell_Type) return String is
   begin
      -- Strips the leading parenthesis from the Enum's image ('(Bash)' -> 'Bash')
      return Shell_Type'Image(Shell)(2..Shell_Type'Image(Shell)'Last - 1);
   end To_String;

   function Is_Modularized (Shell : Shell_Type) return Boolean is
      -- v0.0 Implementation: Check for a placeholder string in the config file.
      Config_File_Path : constant String :=
         GNAT.OS_Lib.Get_Environment_Variable("HOME") & "/." & To_String(Shell) & "rc";
      
      -- This is the unique source string we will inject into the main config file.
      Source_Check_String : constant String := "### MODULAR_SHELLS_ROOT_SOURCE ###";
      
      -- NOTE: The full implementation requires reading the file content and checking for Source_Check_String.
   begin
      Put_Line("Safety Check: Checking if " & Config_File_Path & " is already modularised...");
      -- In v0.0, we always return false to test the modularisation logic.
      return False;
   end Is_Modularized;

   procedure Modularise_Config(Shell : Shell_Type) is
      use Config_Store; -- Use the LMDB package
   begin
      if Is_Modularized(Shell) then
         Put_Line("Config for " & To_String(Shell) & " already modularised. Exiting idempotently.");
         return;
      end if;
      
      -- 1. Show Config Plan (User confirmation needed)
      -- Show_Config_Plan(Shell, ...); -- Implemented in v0.1
      
      -- 2. Backup using LMDB (LMDB/Config_Store procedure call)
      -- Backup_File(Shell);
      
      -- 3. Create directories (idempotent creation logic)
      
      -- 4. Inject source commands (idempotent injection logic)
      
      Put_Line("Modularisation stub complete for " & To_String(Shell) & ". (LMDB calls skipped in v0.0)");
      
   end Modularise_Config;
   
end Shell_Manager;
