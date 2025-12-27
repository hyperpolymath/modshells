-- src/shell_manager/shell_manager.adb
-- SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
with Ada.Directories;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Environment_Variables;

package body Shell_Manager is

   use Ada.Directories;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   -- Signature markers for idempotency
   MODSHELLS_START : constant String := "# MODSHELLS_START - DO NOT REMOVE THIS LINE";
   MODSHELLS_END   : constant String := "# MODSHELLS_END";

   -- Standard binary search paths
   type Path_Array is array (Positive range <>) of access constant String;

   Bin_Path_1 : aliased constant String := "/usr/bin/";
   Bin_Path_2 : aliased constant String := "/bin/";
   Bin_Path_3 : aliased constant String := "/usr/local/bin/";
   Bin_Path_4 : aliased constant String := "/opt/homebrew/bin/";

   Binary_Paths : constant Path_Array (1 .. 4) :=
     (Bin_Path_1'Access, Bin_Path_2'Access, Bin_Path_3'Access, Bin_Path_4'Access);

   ----------------------------------------------------------------------
   -- Helper: Get HOME directory
   ----------------------------------------------------------------------
   function Get_Home return String is
      Home_Ptr : constant Ada.Strings.Unbounded.String_Access :=
        Ada.Environment_Variables.Value ("HOME");
   begin
      if Home_Ptr /= null then
         return Home_Ptr.all;
      else
         return Current_Directory;
      end if;
   exception
      when others => return Current_Directory;
   end Get_Home;

   ----------------------------------------------------------------------
   -- Helper: Join path components
   ----------------------------------------------------------------------
   function Join_Path (Base : in String; Name : in String) return String is
      Path_Sep : constant Character := Separator;
   begin
      if Base'Length > 0 and then Base (Base'Last) /= Path_Sep then
         return Base & Path_Sep & Name;
      else
         return Base & Name;
      end if;
   end Join_Path;

   ----------------------------------------------------------------------
   -- Get_Shell_Binary_Name: Returns the executable name for each shell
   ----------------------------------------------------------------------
   function Get_Shell_Binary_Name (Shell : Shell_Type) return String is
   begin
      case Shell is
         when Bash    => return "bash";
         when Dash    => return "dash";
         when Fish    => return "fish";
         when Ion     => return "ion";
         when Nushell => return "nu";
         when Tcsh    => return "tcsh";
         when Zsh     => return "zsh";
         when Oils    => return "osh";
         when Pwsh    => return "pwsh";
         when Ksh     => return "ksh";
      end case;
   end Get_Shell_Binary_Name;

   ----------------------------------------------------------------------
   -- To_String: Shell type to string
   ----------------------------------------------------------------------
   function To_String (Shell : Shell_Type) return String is
   begin
      case Shell is
         when Bash    => return "bash";
         when Dash    => return "dash";
         when Fish    => return "fish";
         when Ion     => return "ion";
         when Nushell => return "nushell";
         when Tcsh    => return "tcsh";
         when Zsh     => return "zsh";
         when Oils    => return "oils";
         when Pwsh    => return "pwsh";
         when Ksh     => return "ksh";
      end case;
   end To_String;

   ----------------------------------------------------------------------
   -- Is_Shell_Installed: Check if shell binary exists
   ----------------------------------------------------------------------
   function Is_Shell_Installed (Shell : Shell_Type) return Boolean is
      Binary_Name : constant String := Get_Shell_Binary_Name (Shell);
   begin
      -- Check common binary paths
      for I in Binary_Paths'Range loop
         declare
            Full_Path : constant String := Binary_Paths (I).all & Binary_Name;
         begin
            if Exists (Full_Path) then
               return True;
            end if;
         end;
      end loop;
      return False;
   exception
      when others => return False;
   end Is_Shell_Installed;

   ----------------------------------------------------------------------
   -- Detect_Shells: Real implementation checking installed shells
   ----------------------------------------------------------------------
   function Detect_Shells return Shell_List is
      Result : Shell_List (1 .. Max_Shells);
      Count  : Natural := 0;
   begin
      for Shell in Shell_Type'Range loop
         Count := Count + 1;
         Result (Count).Name := Shell;
         if Is_Shell_Installed (Shell) then
            Result (Count).Status := Installed;
         else
            Result (Count).Status := Not_Installed;
         end if;
      end loop;
      return Result (1 .. Count);
   end Detect_Shells;

   ----------------------------------------------------------------------
   -- Get_Config_File_Path: Return the config file path for each shell
   ----------------------------------------------------------------------
   function Get_Config_File_Path (Shell : Shell_Type) return String is
      Home : constant String := Get_Home;
   begin
      case Shell is
         when Bash =>
            return Join_Path (Home, ".bashrc");
         when Zsh =>
            return Join_Path (Home, ".zshrc");
         when Fish =>
            return Join_Path (Join_Path (Join_Path (Home, ".config"), "fish"), "config.fish");
         when Nushell =>
            return Join_Path (Join_Path (Join_Path (Home, ".config"), "nushell"), "config.nu");
         when Ion =>
            return Join_Path (Join_Path (Join_Path (Home, ".config"), "ion"), "initrc");
         when Oils =>
            return Join_Path (Home, ".oshrc");
         when Tcsh =>
            return Join_Path (Home, ".tcshrc");
         when Ksh =>
            return Join_Path (Home, ".kshrc");
         when Dash =>
            -- Dash uses ENV variable; we use .profile as fallback
            return Join_Path (Home, ".profile");
         when Pwsh =>
            -- PowerShell on Linux
            return Join_Path (Join_Path (Join_Path (Join_Path (Home, ".config"), "powershell"), "Microsoft.PowerShell_profile.ps1"), "");
      end case;
   end Get_Config_File_Path;

   ----------------------------------------------------------------------
   -- Create_Backup: Create timestamped backup of a file
   ----------------------------------------------------------------------
   function Create_Backup (File_Path : in String) return String is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;

      Now        : constant Time := Clock;
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Ada.Calendar.Formatting.Hour_Number;
      Minute     : Ada.Calendar.Formatting.Minute_Number;
      Second     : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;

      function Pad2 (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         if N < 10 then
            return "0" & S (S'First + 1 .. S'Last);
         else
            return S (S'First + 1 .. S'Last);
         end if;
      end Pad2;

      Backup_Path : Unbounded_String;
   begin
      if not Exists (File_Path) then
         return "";  -- Nothing to backup
      end if;

      -- Get current time components
      Split (Now, Year, Month, Day, Hour, Minute, Second, Sub_Second);

      -- Build backup filename: original.modshells-backup-YYYYMMDD-HHMMSS
      Backup_Path := To_Unbounded_String (File_Path & ".modshells-backup-" &
        Pad2 (Natural (Year)) &
        Pad2 (Natural (Month)) &
        Pad2 (Natural (Day)) & "-" &
        Pad2 (Natural (Hour)) &
        Pad2 (Natural (Minute)) &
        Pad2 (Natural (Second)));

      -- Copy the file
      Copy_File (File_Path, To_String (Backup_Path));

      Put_Line ("  Backup created: " & To_String (Backup_Path));
      return To_String (Backup_Path);
   exception
      when others =>
         Put_Line ("  Warning: Could not create backup of " & File_Path);
         return "";
   end Create_Backup;

   ----------------------------------------------------------------------
   -- Get_Sourcing_Block: Generate shell-specific sourcing code
   ----------------------------------------------------------------------
   function Get_Sourcing_Block (Shell : Shell_Type; Modshells_Path : in String) return String is
      LF : constant Character := ASCII.LF;
   begin
      case Shell is
         when Bash | Zsh | Ksh | Dash =>
            -- POSIX-compatible shells
            return LF &
              MODSHELLS_START & LF &
              "# Source modular shell configurations" & LF &
              "for _modshells_dir in core tools misc os ui; do" & LF &
              "  _modshells_path=\"" & Modshells_Path & "/$_modshells_dir\"" & LF &
              "  if [ -d \"$_modshells_path\" ]; then" & LF &
              "    for _modshells_file in \"$_modshells_path\"/*.sh; do" & LF &
              "      [ -f \"$_modshells_file\" ] && . \"$_modshells_file\"" & LF &
              "    done" & LF &
              "  fi" & LF &
              "done" & LF &
              "unset _modshells_dir _modshells_path _modshells_file" & LF &
              MODSHELLS_END & LF;

         when Fish =>
            return LF &
              "# MODSHELLS_START - DO NOT REMOVE THIS LINE" & LF &
              "# Source modular shell configurations" & LF &
              "for _modshells_dir in core tools misc os ui" & LF &
              "  set _modshells_path \"" & Modshells_Path & "/$_modshells_dir\"" & LF &
              "  if test -d $_modshells_path" & LF &
              "    for _modshells_file in $_modshells_path/*.fish" & LF &
              "      test -f $_modshells_file; and source $_modshells_file" & LF &
              "    end" & LF &
              "  end" & LF &
              "end" & LF &
              "# MODSHELLS_END" & LF;

         when Nushell =>
            return LF &
              "# MODSHELLS_START - DO NOT REMOVE THIS LINE" & LF &
              "# Source modular shell configurations" & LF &
              "let modshells_root = \"" & Modshells_Path & "\"" & LF &
              "for dir in [core tools misc os ui] {" & LF &
              "  let dir_path = ($modshells_root | path join $dir)" & LF &
              "  if ($dir_path | path exists) {" & LF &
              "    for file in (glob ($dir_path | path join \"*.nu\")) {" & LF &
              "      source $file" & LF &
              "    }" & LF &
              "  }" & LF &
              "}" & LF &
              "# MODSHELLS_END" & LF;

         when Ion =>
            return LF &
              "# MODSHELLS_START - DO NOT REMOVE THIS LINE" & LF &
              "# Source modular shell configurations" & LF &
              "for dir in core tools misc os ui" & LF &
              "  let path = \"" & Modshells_Path & "/$dir\"" & LF &
              "  if test -d $path" & LF &
              "    for file in $path/*.ion" & LF &
              "      if test -f $file" & LF &
              "        source $file" & LF &
              "      end" & LF &
              "    end" & LF &
              "  end" & LF &
              "end" & LF &
              "# MODSHELLS_END" & LF;

         when Tcsh =>
            return LF &
              "# MODSHELLS_START - DO NOT REMOVE THIS LINE" & LF &
              "# Source modular shell configurations" & LF &
              "foreach _modshells_dir (core tools misc os ui)" & LF &
              "  set _modshells_path = \"" & Modshells_Path & "/$_modshells_dir\"" & LF &
              "  if (-d $_modshells_path) then" & LF &
              "    foreach _modshells_file ($_modshells_path/*.tcsh)" & LF &
              "      if (-f $_modshells_file) source $_modshells_file" & LF &
              "    end" & LF &
              "  endif" & LF &
              "end" & LF &
              "# MODSHELLS_END" & LF;

         when Oils =>
            -- Oils (OSH/YSH) is POSIX-compatible
            return LF &
              MODSHELLS_START & LF &
              "# Source modular shell configurations" & LF &
              "for _modshells_dir in core tools misc os ui; do" & LF &
              "  _modshells_path=\"" & Modshells_Path & "/$_modshells_dir\"" & LF &
              "  if [ -d \"$_modshells_path\" ]; then" & LF &
              "    for _modshells_file in \"$_modshells_path\"/*.sh; do" & LF &
              "      [ -f \"$_modshells_file\" ] && source \"$_modshells_file\"" & LF &
              "    done" & LF &
              "  fi" & LF &
              "done" & LF &
              "unset _modshells_dir _modshells_path _modshells_file" & LF &
              MODSHELLS_END & LF;

         when Pwsh =>
            return LF &
              "# MODSHELLS_START - DO NOT REMOVE THIS LINE" & LF &
              "# Source modular shell configurations" & LF &
              "$modshellsRoot = \"" & Modshells_Path & "\"" & LF &
              "foreach ($dir in @('core', 'tools', 'misc', 'os', 'ui')) {" & LF &
              "  $dirPath = Join-Path $modshellsRoot $dir" & LF &
              "  if (Test-Path $dirPath) {" & LF &
              "    Get-ChildItem -Path $dirPath -Filter '*.ps1' | ForEach-Object {" & LF &
              "      . $_.FullName" & LF &
              "    }" & LF &
              "  }" & LF &
              "}" & LF &
              "# MODSHELLS_END" & LF;
      end case;
   end Get_Sourcing_Block;

   ----------------------------------------------------------------------
   -- Create_Modshell_Directories: Idempotent directory creation
   ----------------------------------------------------------------------
   procedure Create_Modshell_Directories (Root_Path : in String) is
      Subdirs : constant array (1 .. 5) of String (1 .. 5) :=
        ("core ", "tools", "misc ", "os   ", "ui   ");
   begin
      if not Exists (Root_Path) then
         Create_Path (Root_Path);
      end if;

      for I in Subdirs'Range loop
         declare
            Subdir_Name : constant String := Ada.Strings.Fixed.Trim (Subdirs (I), Ada.Strings.Right);
            Full_Path   : constant String := Join_Path (Root_Path, Subdir_Name);
         begin
            if not Exists (Full_Path) then
               Create_Directory (Full_Path);
            end if;
         end;
      end loop;
   exception
      when Name_Error | Use_Error => raise;
   end Create_Modshell_Directories;

   ----------------------------------------------------------------------
   -- Is_Modularized: Check if config file contains our signature
   ----------------------------------------------------------------------
   function Is_Modularized (Shell : Shell_Type) return Boolean is
      Config_Path : constant String := Get_Config_File_Path (Shell);
      File_Handle : File_Type;
      Line_Buffer : String (1 .. 1024);
      Last        : Natural;
   begin
      if not Exists (Config_Path) then
         return False;
      end if;

      Open (File_Handle, In_File, Config_Path);

      while not End_Of_File (File_Handle) loop
         Get_Line (File_Handle, Line_Buffer, Last);
         -- Check if line contains our signature
         if Last >= MODSHELLS_START'Length then
            if Line_Buffer (1 .. MODSHELLS_START'Length) = MODSHELLS_START then
               Close (File_Handle);
               return True;
            end if;
         end if;
      end loop;

      Close (File_Handle);
      return False;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return False;
      when others =>
         if Is_Open (File_Handle) then
            Close (File_Handle);
         end if;
         return False;
   end Is_Modularized;

   ----------------------------------------------------------------------
   -- Modularise_Config: Full modularisation for a single shell
   ----------------------------------------------------------------------
   procedure Modularise_Config (Shell : Shell_Type; Modshells_Path : in String) is
      Config_Path   : constant String := Get_Config_File_Path (Shell);
      Config_Dir    : constant String := Containing_Directory (Config_Path);
      Sourcing_Code : constant String := Get_Sourcing_Block (Shell, Modshells_Path);
      File_Handle   : File_Type;
      Backup_Result : String (1 .. 256);
      Backup_Len    : Natural := 0;
   begin
      Put_Line ("Modularising " & To_String (Shell) & "...");

      -- Check if already modularized
      if Is_Modularized (Shell) then
         Put_Line ("  Already modularized, skipping.");
         return;
      end if;

      -- Ensure config directory exists (for Fish, Nushell, etc.)
      if not Exists (Config_Dir) then
         Create_Path (Config_Dir);
         Put_Line ("  Created config directory: " & Config_Dir);
      end if;

      -- Create backup if file exists
      if Exists (Config_Path) then
         declare
            Backup_Path : constant String := Create_Backup (Config_Path);
         begin
            if Backup_Path'Length > 0 then
               Backup_Len := Natural'Min (Backup_Path'Length, 256);
               Backup_Result (1 .. Backup_Len) := Backup_Path (Backup_Path'First .. Backup_Path'First + Backup_Len - 1);
            end if;
         end;
      end if;

      -- Append sourcing block to config file
      Open (File_Handle, Append_File, Config_Path);
      Put (File_Handle, Sourcing_Code);
      Close (File_Handle);

      Put_Line ("  Injected modshells sourcing block.");

   exception
      when Ada.IO_Exceptions.Name_Error =>
         -- File doesn't exist, create it
         Create (File_Handle, Out_File, Config_Path);
         Put (File_Handle, Sourcing_Code);
         Close (File_Handle);
         Put_Line ("  Created " & Config_Path & " with modshells sourcing block.");
      when others =>
         if Is_Open (File_Handle) then
            Close (File_Handle);
         end if;
         Put_Line ("  Error modularising " & To_String (Shell));
         raise;
   end Modularise_Config;

   ----------------------------------------------------------------------
   -- Modularise_All_Shells: Modularise all installed shells
   ----------------------------------------------------------------------
   procedure Modularise_All_Shells (Modshells_Path : in String) is
      Shells : constant Shell_List := Detect_Shells;
   begin
      for I in Shells'Range loop
         if Shells (I).Status = Installed then
            Modularise_Config (Shells (I).Name, Modshells_Path);
         end if;
      end loop;
   end Modularise_All_Shells;

end Shell_Manager;
