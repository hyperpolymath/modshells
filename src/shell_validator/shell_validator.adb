-- SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
-- src/shell_validator/shell_validator.adb
--
-- Shell Validator Implementation
-- Initial stub implementation with framework for comprehensive validation

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNAT.OS_Lib;

package body Shell_Validator is

   ---------------------------------------------------------------------------
   -- Helper Functions
   ---------------------------------------------------------------------------

   function Make_Result
     (Valid      : Boolean;
      Level      : Severity;
      Code       : String;
      Message    : String;
      File_Path  : String := "";
      Line_Num   : Natural := 0;
      Suggestion : String := "") return Validation_Result
   is
   begin
      return (Valid       => Valid,
              Level       => Level,
              Code        => To_Unbounded_String (Code),
              Message     => To_Unbounded_String (Message),
              File_Path   => To_Unbounded_String (File_Path),
              Line_Number => Line_Num,
              Suggestion  => To_Unbounded_String (Suggestion));
   end Make_Result;

   procedure Add_Result
     (Report : in out Validation_Report;
      Result : Validation_Result)
   is
   begin
      if Report.Result_Count < Max_Results then
         Report.Result_Count := Report.Result_Count + 1;
         Report.Results (Result_Index (Report.Result_Count)) := Result;

         if Result.Valid then
            Report.Pass_Count := Report.Pass_Count + 1;
         else
            case Result.Level is
               when Info    => null;
               when Warning => Report.Warn_Count := Report.Warn_Count + 1;
               when Error   => Report.Error_Count := Report.Error_Count + 1;
               when Critical => Report.Critical_Count := Report.Critical_Count + 1;
               when Security => Report.Security_Count := Report.Security_Count + 1;
            end case;
         end if;
      end if;
   end Add_Result;

   function File_Exists (Path : String) return Boolean is
   begin
      return Ada.Directories.Exists (Path) and then
             Ada.Directories.Kind (Path) = Ordinary_File;
   exception
      when others => return False;
   end File_Exists;

   function Directory_Exists (Path : String) return Boolean is
   begin
      return Ada.Directories.Exists (Path) and then
             Ada.Directories.Kind (Path) = Directory;
   exception
      when others => return False;
   end Directory_Exists;

   ---------------------------------------------------------------------------
   -- 1. SYNTAX AND SEQUENCE VALIDATION
   ---------------------------------------------------------------------------

   function Validate_Syntax (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      if not File_Exists (File_Path) then
         Add_Result (Report, Make_Result
           (Valid => False, Level => Error, Code => "SYN001",
            Message => "File not found: " & File_Path,
            File_Path => File_Path));
         return Report;
      end if;

      -- TODO: Execute 'bash -n' and 'shellcheck' for syntax validation
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SYN000",
         Message => "Syntax validation pending implementation",
         File_Path => File_Path,
         Suggestion => "Run: bash -n " & File_Path & " && shellcheck " & File_Path));

      return Report;
   end Validate_Syntax;

   function Validate_Command_Sequence (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Parse file and check command ordering
      -- Examples: exports before use, PATH before which, etc.
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SEQ000",
         Message => "Command sequence validation pending implementation",
         File_Path => File_Path));
      return Report;
   end Validate_Command_Sequence;

   function Detect_Infinite_Loops (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Static analysis for potential infinite loops
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "LOOP000",
         Message => "Loop detection pending implementation",
         File_Path => File_Path));
      return Report;
   end Detect_Infinite_Loops;

   function Detect_Skipped_Blocks (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Dead code detection
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "DEAD000",
         Message => "Dead code detection pending implementation",
         File_Path => File_Path));
      return Report;
   end Detect_Skipped_Blocks;

   ---------------------------------------------------------------------------
   -- 2. DIRECTORY AND FILE VALIDATION
   ---------------------------------------------------------------------------

   function Validate_Directory_Structure (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
      Required_Dirs : constant array (1 .. 5) of String (1 .. 5) :=
        ("core/", "tools", "misc/", "os///", "ui///");
      Dir_Names : constant array (1 .. 5) of String (1 .. 5) :=
        ("core ", "tools", "misc ", "os   ", "ui   ");
   begin
      if not Directory_Exists (Root_Path) then
         Add_Result (Report, Make_Result
           (Valid => False, Level => Error, Code => "DIR001",
            Message => "Root directory not found: " & Root_Path,
            File_Path => Root_Path));
         return Report;
      end if;

      -- Check for required subdirectories
      for I in Dir_Names'Range loop
         declare
            Dir_Name : constant String := Trim (Dir_Names (I), Ada.Strings.Both);
            Full_Path : constant String := Root_Path & "/" & Dir_Name;
         begin
            if Directory_Exists (Full_Path) then
               Add_Result (Report, Make_Result
                 (Valid => True, Level => Info, Code => "DIR100",
                  Message => "Directory exists: " & Dir_Name,
                  File_Path => Full_Path));
            else
               Add_Result (Report, Make_Result
                 (Valid => False, Level => Warning, Code => "DIR101",
                  Message => "Missing directory: " & Dir_Name,
                  File_Path => Full_Path,
                  Suggestion => "Create with: mkdir -p " & Full_Path));
            end if;
         end;
      end loop;

      return Report;
   end Validate_Directory_Structure;

   function Validate_Symlinks (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Traverse directory and check symlinks
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "LINK000",
         Message => "Symlink validation pending implementation",
         File_Path => Root_Path));
      return Report;
   end Validate_Symlinks;

   function Validate_File_Presence (Config_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      if File_Exists (Config_Path) then
         Add_Result (Report, Make_Result
           (Valid => True, Level => Info, Code => "FILE100",
            Message => "Configuration file exists",
            File_Path => Config_Path));
      else
         Add_Result (Report, Make_Result
           (Valid => False, Level => Error, Code => "FILE001",
            Message => "Configuration file not found",
            File_Path => Config_Path));
      end if;
      return Report;
   end Validate_File_Presence;

   function Validate_Completeness (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check for orphaned modules and missing dependencies
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "COMP000",
         Message => "Completeness validation pending implementation",
         File_Path => Root_Path));
      return Report;
   end Validate_Completeness;

   ---------------------------------------------------------------------------
   -- 3. PERMISSION VALIDATION
   ---------------------------------------------------------------------------

   function Validate_Permissions (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check file permissions (should be 644 or 755)
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "PERM000",
         Message => "Permission validation pending implementation",
         File_Path => File_Path,
         Suggestion => "Expected: 644 for configs, 755 for scripts"));
      return Report;
   end Validate_Permissions;

   function Validate_Ownership (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Verify file is owned by current user
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "OWN000",
         Message => "Ownership validation pending implementation",
         File_Path => File_Path));
      return Report;
   end Validate_Ownership;

   function Detect_World_Writable (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Find world-writable files (security risk)
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "WRIT000",
         Message => "World-writable detection pending implementation",
         File_Path => Root_Path,
         Suggestion => "Run: find " & Root_Path & " -perm -002 -type f"));
      return Report;
   end Detect_World_Writable;

   function Detect_SUID_SGID (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Find inappropriate SUID/SGID bits
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SUID000",
         Message => "SUID/SGID detection pending implementation",
         File_Path => Root_Path));
      return Report;
   end Detect_SUID_SGID;

   ---------------------------------------------------------------------------
   -- 4. DUPLICATION DETECTION
   ---------------------------------------------------------------------------

   function Detect_Duplicate_Aliases (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Parse all files for alias definitions, find duplicates
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "DUP-A000",
         Message => "Duplicate alias detection pending implementation",
         File_Path => Root_Path,
         Suggestion => "Run: grep -rh '^alias ' " & Root_Path & " | sort | uniq -d"));
      return Report;
   end Detect_Duplicate_Aliases;

   function Detect_Duplicate_Functions (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Parse all files for function definitions, find duplicates
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "DUP-F000",
         Message => "Duplicate function detection pending implementation",
         File_Path => Root_Path));
      return Report;
   end Detect_Duplicate_Functions;

   function Detect_Duplicate_Exports (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Parse all files for export statements, find duplicates
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "DUP-E000",
         Message => "Duplicate export detection pending implementation",
         File_Path => Root_Path));
      return Report;
   end Detect_Duplicate_Exports;

   function Detect_Path_Duplicates (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Analyze PATH modifications for duplicates
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "DUP-P000",
         Message => "PATH duplicate detection pending implementation",
         File_Path => Root_Path,
         Suggestion => "Check: echo $PATH | tr ':' '\n' | sort | uniq -d"));
      return Report;
   end Detect_Path_Duplicates;

   function Detect_Conflicting_Settings (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Find settings that override each other
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "CONF000",
         Message => "Conflict detection pending implementation",
         File_Path => Root_Path));
      return Report;
   end Detect_Conflicting_Settings;

   ---------------------------------------------------------------------------
   -- 5. POSIX COMPLIANCE
   ---------------------------------------------------------------------------

   function Validate_POSIX_Compliance
     (File_Path : String;
      Level     : POSIX_Level := Relaxed) return Validation_Report
   is
      Report : Validation_Report;
      Level_Str : constant String := (case Level is
         when Strict   => "strict",
         when Relaxed  => "relaxed",
         when Extended => "extended");
   begin
      -- TODO: Run checkbashisms or similar tool
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "POSIX000",
         Message => "POSIX compliance check (" & Level_Str & ") pending implementation",
         File_Path => File_Path,
         Suggestion => "Run: checkbashisms -f " & File_Path));
      return Report;
   end Validate_POSIX_Compliance;

   function Detect_Bashisms (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Run checkbashisms
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "BASH000",
         Message => "Bashism detection pending implementation",
         File_Path => File_Path));
      return Report;
   end Detect_Bashisms;

   function Validate_Portable_Syntax (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check syntax works across multiple shells
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "PORT000",
         Message => "Portability validation pending implementation",
         File_Path => File_Path));
      return Report;
   end Validate_Portable_Syntax;

   ---------------------------------------------------------------------------
   -- 6. PERFORMANCE ANALYSIS
   ---------------------------------------------------------------------------

   function Analyze_Load_Time (Config_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Profile shell startup with time command
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "PERF000",
         Message => "Load time analysis pending implementation",
         File_Path => Config_Path,
         Suggestion => "Run: time bash -i -c exit"));
      return Report;
   end Analyze_Load_Time;

   function Detect_Slow_Commands (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Identify slow initialization commands
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SLOW000",
         Message => "Slow command detection pending implementation",
         File_Path => File_Path));
      return Report;
   end Detect_Slow_Commands;

   function Suggest_Lazy_Loading (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Suggest candidates for deferred loading
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "LAZY000",
         Message => "Lazy loading suggestions pending implementation",
         File_Path => File_Path));
      return Report;
   end Suggest_Lazy_Loading;

   function Analyze_Execution_Flow (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Analyze load order and dependencies
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "FLOW000",
         Message => "Execution flow analysis pending implementation",
         File_Path => Root_Path));
      return Report;
   end Analyze_Execution_Flow;

   ---------------------------------------------------------------------------
   -- 7. SECURITY VALIDATION
   ---------------------------------------------------------------------------

   function Detect_Hardcoded_Secrets (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Run gitleaks or similar secret detection
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SEC-S000",
         Message => "Secret detection pending implementation",
         File_Path => Root_Path,
         Suggestion => "Run: gitleaks detect --source " & Root_Path));
      return Report;
   end Detect_Hardcoded_Secrets;

   function Detect_Dangerous_Commands (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Detect rm -rf, eval, exec with untrusted input, etc.
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SEC-D000",
         Message => "Dangerous command detection pending implementation",
         File_Path => File_Path));
      return Report;
   end Detect_Dangerous_Commands;

   function Validate_Input_Sanitization (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check for proper quoting and sanitization
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SEC-I000",
         Message => "Input sanitization validation pending implementation",
         File_Path => File_Path));
      return Report;
   end Validate_Input_Sanitization;

   function Check_CVE_Vulnerabilities (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check for known shell CVEs (ShellShock, etc.)
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "CVE000",
         Message => "CVE vulnerability check pending implementation",
         File_Path => File_Path,
         Suggestion => "Check bash version for ShellShock (CVE-2014-6271)"));
      return Report;
   end Check_CVE_Vulnerabilities;

   function Validate_Sudo_Usage (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Validate safe sudo patterns
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SUDO000",
         Message => "Sudo usage validation pending implementation",
         File_Path => File_Path));
      return Report;
   end Validate_Sudo_Usage;

   ---------------------------------------------------------------------------
   -- 8. SELINUX AND FIREWALL COMPATIBILITY
   ---------------------------------------------------------------------------

   function Validate_SELinux_Context (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check SELinux context with ls -Z
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SEL000",
         Message => "SELinux context validation pending implementation",
         File_Path => File_Path,
         Suggestion => "Run: ls -Z " & File_Path));
      return Report;
   end Validate_SELinux_Context;

   function Detect_SELinux_Conflicts (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check for commands that may violate SELinux policies
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SEL-C000",
         Message => "SELinux conflict detection pending implementation",
         File_Path => Root_Path));
      return Report;
   end Detect_SELinux_Conflicts;

   function Validate_Firewall_Safety (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check for network commands that may conflict with firewall
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "FW000",
         Message => "Firewall safety validation pending implementation",
         File_Path => File_Path));
      return Report;
   end Validate_Firewall_Safety;

   ---------------------------------------------------------------------------
   -- 9. TOOL INTEGRATION VALIDATION
   ---------------------------------------------------------------------------

   function Validate_Starship_Config (Config_Path : String) return Validation_Report is
      Report : Validation_Report;
      Starship_Config : constant String := Config_Path & "/.config/starship.toml";
   begin
      if File_Exists (Starship_Config) then
         Add_Result (Report, Make_Result
           (Valid => True, Level => Info, Code => "STAR100",
            Message => "Starship config found",
            File_Path => Starship_Config));
      else
         Add_Result (Report, Make_Result
           (Valid => True, Level => Warning, Code => "STAR001",
            Message => "Starship config not found",
            File_Path => Starship_Config,
            Suggestion => "Create with: starship init bash"));
      end if;
      return Report;
   end Validate_Starship_Config;

   function Validate_Completion_Setup (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check bash-completion, carapace setup
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "COMP-S000",
         Message => "Completion setup validation pending implementation",
         File_Path => Root_Path));
      return Report;
   end Validate_Completion_Setup;

   function Validate_Direnv_Integration (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check direnv hook placement
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "DIRENV000",
         Message => "Direnv integration validation pending implementation",
         File_Path => Root_Path,
         Suggestion => "Ensure 'eval $(direnv hook bash)' is in config"));
      return Report;
   end Validate_Direnv_Integration;

   function Validate_Asdf_Integration (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check asdf initialization
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "ASDF000",
         Message => "asdf integration validation pending implementation",
         File_Path => Root_Path,
         Suggestion => "Ensure '. $HOME/.asdf/asdf.sh' is sourced"));
      return Report;
   end Validate_Asdf_Integration;

   function Validate_Atuin_Integration (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check atuin initialization
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "ATUIN000",
         Message => "Atuin integration validation pending implementation",
         File_Path => Root_Path));
      return Report;
   end Validate_Atuin_Integration;

   ---------------------------------------------------------------------------
   -- 10. FORMAL VERIFICATION HOOKS
   ---------------------------------------------------------------------------

   function Run_Formal_Verification
     (File_Path : String;
      Tool      : Formal_Tool := ShellCheck) return Validation_Report
   is
      Report : Validation_Report;
      Tool_Name : constant String := (case Tool is
         when ShellCheck => "shellcheck",
         when BATS       => "bats",
         when Bats_Core  => "bats-core",
         when Pyre       => "pyre",
         when Custom     => "custom");
   begin
      -- TODO: Execute the specified verification tool
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "FORMAL000",
         Message => "Formal verification with " & Tool_Name & " pending implementation",
         File_Path => File_Path,
         Suggestion => "Run: " & Tool_Name & " " & File_Path));
      return Report;
   end Run_Formal_Verification;

   function Generate_Verification_Report (Root_Path : String) return String is
   begin
      return "Verification Report for " & Root_Path & " (pending implementation)";
   end Generate_Verification_Report;

   function Export_For_Theorem_Prover (File_Path : String) return String is
   begin
      return "-- Theorem prover export for " & File_Path & " (experimental)";
   end Export_For_Theorem_Prover;

   ---------------------------------------------------------------------------
   -- 11. ANNOTATION AND METADATA
   ---------------------------------------------------------------------------

   function Validate_SPDX_Headers (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check all files for SPDX-License-Identifier headers
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "SPDX000",
         Message => "SPDX header validation pending implementation",
         File_Path => Root_Path,
         Suggestion => "Add: # SPDX-License-Identifier: AGPL-3.0-or-later"));
      return Report;
   end Validate_SPDX_Headers;

   function Validate_Annotations (File_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Check for required comments and annotations
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "ANNOT000",
         Message => "Annotation validation pending implementation",
         File_Path => File_Path));
      return Report;
   end Validate_Annotations;

   function Check_Documentation_Coverage (Root_Path : String) return Validation_Report is
      Report : Validation_Report;
   begin
      -- TODO: Ensure all functions/aliases have documentation
      Add_Result (Report, Make_Result
        (Valid => True, Level => Info, Code => "DOC000",
         Message => "Documentation coverage check pending implementation",
         File_Path => Root_Path));
      return Report;
   end Check_Documentation_Coverage;

   ---------------------------------------------------------------------------
   -- 12. COMPREHENSIVE VALIDATION
   ---------------------------------------------------------------------------

   function Validate_All
     (Root_Path : String;
      Level     : Validation_Level := Standard) return Validation_Report
   is
      Report : Validation_Report;
      Temp_Report : Validation_Report;
   begin
      -- Run validators based on level
      case Level is
         when Quick =>
            Temp_Report := Validate_Directory_Structure (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

         when Standard =>
            -- Directory structure
            Temp_Report := Validate_Directory_Structure (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

            -- Duplicates
            Temp_Report := Detect_Duplicate_Aliases (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

            Temp_Report := Detect_Path_Duplicates (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

         when Thorough =>
            -- All standard checks plus security
            Temp_Report := Validate_All (Root_Path, Standard);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

            Temp_Report := Detect_Hardcoded_Secrets (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

            Temp_Report := Detect_World_Writable (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

         when Paranoid =>
            -- Everything
            Temp_Report := Validate_All (Root_Path, Thorough);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

            Temp_Report := Validate_SELinux_Context (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;

            Temp_Report := Check_CVE_Vulnerabilities (Root_Path);
            for I in 1 .. Temp_Report.Result_Count loop
               Add_Result (Report, Temp_Report.Results (Result_Index (I)));
            end loop;
      end case;

      return Report;
   end Validate_All;

   procedure Print_Report (Report : Validation_Report) is
   begin
      Put_Line ("=== Shell Validator Report ===");
      Put_Line ("Results: " & Natural'Image (Report.Result_Count));
      Put_Line ("Pass: " & Natural'Image (Report.Pass_Count) &
                " | Warn: " & Natural'Image (Report.Warn_Count) &
                " | Error: " & Natural'Image (Report.Error_Count) &
                " | Critical: " & Natural'Image (Report.Critical_Count) &
                " | Security: " & Natural'Image (Report.Security_Count));
      Put_Line ("");

      for I in 1 .. Report.Result_Count loop
         declare
            R : constant Validation_Result := Report.Results (Result_Index (I));
            Prefix : constant String := (if R.Valid then "[PASS]" else "[FAIL]");
            Level_Str : constant String := (case R.Level is
               when Info     => "INFO",
               when Warning  => "WARN",
               when Error    => "ERROR",
               when Critical => "CRIT",
               when Security => "SEC");
         begin
            Put_Line (Prefix & " [" & Level_Str & "] " &
                     To_String (R.Code) & ": " & To_String (R.Message));
            if Length (R.File_Path) > 0 then
               Put_Line ("       File: " & To_String (R.File_Path));
            end if;
            if Length (R.Suggestion) > 0 then
               Put_Line ("       Suggestion: " & To_String (R.Suggestion));
            end if;
         end;
      end loop;
   end Print_Report;

   function Report_To_JSON (Report : Validation_Report) return String is
   begin
      -- TODO: Proper JSON serialization
      return "{""results"":" & Natural'Image (Report.Result_Count) & "}";
   end Report_To_JSON;

   function Report_To_SARIF (Report : Validation_Report) return String is
   begin
      -- TODO: SARIF format for CI/CD integration
      return "{""version"":""2.1.0"",""runs"":[]}";
   end Report_To_SARIF;

end Shell_Validator;
