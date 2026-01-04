-- SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
-- src/shell_validator/shell_validator.ads
--
-- Shell Validator - Comprehensive shell configuration validation
--
-- Provides validation, verification, and security analysis for shell
-- configurations including POSIX compliance, security auditing,
-- performance analysis, and formal verification hooks.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Shell_Validator is

   ---------------------------------------------------------------------------
   -- Validation Severity Levels
   ---------------------------------------------------------------------------

   type Severity is (Info, Warning, Error, Critical, Security);

   ---------------------------------------------------------------------------
   -- Validation Result Types
   ---------------------------------------------------------------------------

   type Validation_Result is record
      Valid       : Boolean := True;
      Level       : Severity := Info;
      Code        : Unbounded_String := Null_Unbounded_String;
      Message     : Unbounded_String := Null_Unbounded_String;
      File_Path   : Unbounded_String := Null_Unbounded_String;
      Line_Number : Natural := 0;
      Suggestion  : Unbounded_String := Null_Unbounded_String;
   end record;

   Max_Results : constant := 1000;
   type Result_Index is range 1 .. Max_Results;
   type Result_Array is array (Result_Index) of Validation_Result;

   type Validation_Report is record
      Results      : Result_Array;
      Result_Count : Natural := 0;
      Pass_Count   : Natural := 0;
      Warn_Count   : Natural := 0;
      Error_Count  : Natural := 0;
      Critical_Count : Natural := 0;
      Security_Count : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   -- 1. SYNTAX AND SEQUENCE VALIDATION
   ---------------------------------------------------------------------------
   -- Validates correct bash command sequences and syntax

   function Validate_Syntax (File_Path : String) return Validation_Report;
   -- Run shell syntax check (bash -n, shellcheck)

   function Validate_Command_Sequence (File_Path : String) return Validation_Report;
   -- Ensure commands are in correct order (exports before use, etc.)

   function Detect_Infinite_Loops (File_Path : String) return Validation_Report;
   -- Detect potential infinite loops or recursion

   function Detect_Skipped_Blocks (File_Path : String) return Validation_Report;
   -- Find code blocks that may never execute (dead code)

   ---------------------------------------------------------------------------
   -- 2. DIRECTORY AND FILE VALIDATION
   ---------------------------------------------------------------------------
   -- Validates directory structure and file linking

   function Validate_Directory_Structure (Root_Path : String) return Validation_Report;
   -- Check modular directory structure (core/, tools/, misc/, os/, ui/)

   function Validate_Symlinks (Root_Path : String) return Validation_Report;
   -- Verify all symlinks are valid and not broken

   function Validate_File_Presence (Config_Path : String) return Validation_Report;
   -- Check all required files exist

   function Validate_Completeness (Root_Path : String) return Validation_Report;
   -- Ensure no orphaned or missing module files

   ---------------------------------------------------------------------------
   -- 3. PERMISSION VALIDATION
   ---------------------------------------------------------------------------
   -- Validates file permissions for security

   function Validate_Permissions (File_Path : String) return Validation_Report;
   -- Check read/write/execute permissions are correct

   function Validate_Ownership (File_Path : String) return Validation_Report;
   -- Verify file ownership (should be current user)

   function Detect_World_Writable (Root_Path : String) return Validation_Report;
   -- Find any world-writable config files (security risk)

   function Detect_SUID_SGID (Root_Path : String) return Validation_Report;
   -- Detect inappropriate SUID/SGID bits

   ---------------------------------------------------------------------------
   -- 4. DUPLICATION DETECTION
   ---------------------------------------------------------------------------
   -- Finds redundant or conflicting configurations

   function Detect_Duplicate_Aliases (Root_Path : String) return Validation_Report;
   -- Find duplicate alias definitions

   function Detect_Duplicate_Functions (Root_Path : String) return Validation_Report;
   -- Find duplicate function definitions

   function Detect_Duplicate_Exports (Root_Path : String) return Validation_Report;
   -- Find duplicate environment variable exports

   function Detect_Path_Duplicates (Root_Path : String) return Validation_Report;
   -- Find duplicate PATH entries

   function Detect_Conflicting_Settings (Root_Path : String) return Validation_Report;
   -- Find settings that override each other unexpectedly

   ---------------------------------------------------------------------------
   -- 5. POSIX COMPLIANCE
   ---------------------------------------------------------------------------
   -- Validates POSIX shell compliance

   type POSIX_Level is (Strict, Relaxed, Extended);

   function Validate_POSIX_Compliance
     (File_Path : String;
      Level     : POSIX_Level := Relaxed) return Validation_Report;
   -- Check for POSIX compliance at specified strictness level

   function Detect_Bashisms (File_Path : String) return Validation_Report;
   -- Find bash-specific syntax in supposedly portable scripts

   function Validate_Portable_Syntax (File_Path : String) return Validation_Report;
   -- Ensure script works across sh, dash, bash, ksh, zsh

   ---------------------------------------------------------------------------
   -- 6. PERFORMANCE ANALYSIS
   ---------------------------------------------------------------------------
   -- Analyzes shell startup and execution performance

   function Analyze_Load_Time (Config_Path : String) return Validation_Report;
   -- Profile shell configuration load time

   function Detect_Slow_Commands (File_Path : String) return Validation_Report;
   -- Find commands that may slow down shell startup

   function Suggest_Lazy_Loading (File_Path : String) return Validation_Report;
   -- Suggest candidates for lazy/deferred loading

   function Analyze_Execution_Flow (Root_Path : String) return Validation_Report;
   -- Analyze optimal execution order

   ---------------------------------------------------------------------------
   -- 7. SECURITY VALIDATION
   ---------------------------------------------------------------------------
   -- Security auditing and CVE tracking

   function Detect_Hardcoded_Secrets (Root_Path : String) return Validation_Report;
   -- Find hardcoded passwords, tokens, API keys

   function Detect_Dangerous_Commands (File_Path : String) return Validation_Report;
   -- Find potentially dangerous commands (rm -rf, eval, etc.)

   function Validate_Input_Sanitization (File_Path : String) return Validation_Report;
   -- Check for proper input sanitization in scripts

   function Check_CVE_Vulnerabilities (File_Path : String) return Validation_Report;
   -- Check for known CVEs in shell constructs

   function Validate_Sudo_Usage (File_Path : String) return Validation_Report;
   -- Validate safe sudo usage patterns

   ---------------------------------------------------------------------------
   -- 8. SELINUX AND FIREWALL COMPATIBILITY
   ---------------------------------------------------------------------------
   -- System security integration checks

   function Validate_SELinux_Context (File_Path : String) return Validation_Report;
   -- Check SELinux context is appropriate

   function Detect_SELinux_Conflicts (Root_Path : String) return Validation_Report;
   -- Find commands that may conflict with SELinux policies

   function Validate_Firewall_Safety (File_Path : String) return Validation_Report;
   -- Check for commands that may conflict with firewall rules

   ---------------------------------------------------------------------------
   -- 9. TOOL INTEGRATION VALIDATION
   ---------------------------------------------------------------------------
   -- Validates integration with shell ecosystem tools

   function Validate_Starship_Config (Config_Path : String) return Validation_Report;
   -- Validate starship prompt configuration

   function Validate_Completion_Setup (Root_Path : String) return Validation_Report;
   -- Check completion system configuration (bash-completion, carapace)

   function Validate_Direnv_Integration (Root_Path : String) return Validation_Report;
   -- Validate direnv hook placement

   function Validate_Asdf_Integration (Root_Path : String) return Validation_Report;
   -- Validate asdf/mise version manager setup

   function Validate_Atuin_Integration (Root_Path : String) return Validation_Report;
   -- Validate atuin history integration

   ---------------------------------------------------------------------------
   -- 10. FORMAL VERIFICATION HOOKS
   ---------------------------------------------------------------------------
   -- Integration points for formal verification tools

   type Formal_Tool is (ShellCheck, BATS, Bats_Core, Pyre, Custom);

   function Run_Formal_Verification
     (File_Path : String;
      Tool      : Formal_Tool := ShellCheck) return Validation_Report;
   -- Run formal verification using specified tool

   function Generate_Verification_Report (Root_Path : String) return String;
   -- Generate comprehensive formal verification report

   function Export_For_Theorem_Prover (File_Path : String) return String;
   -- Export shell logic for external theorem prover (experimental)

   ---------------------------------------------------------------------------
   -- 11. ANNOTATION AND METADATA
   ---------------------------------------------------------------------------
   -- Validates annotations and documentation

   function Validate_SPDX_Headers (Root_Path : String) return Validation_Report;
   -- Check all files have SPDX license headers

   function Validate_Annotations (File_Path : String) return Validation_Report;
   -- Check for required annotations and comments

   function Check_Documentation_Coverage (Root_Path : String) return Validation_Report;
   -- Ensure functions/aliases are documented

   ---------------------------------------------------------------------------
   -- 12. COMPREHENSIVE VALIDATION
   ---------------------------------------------------------------------------
   -- Run all validators

   type Validation_Level is (Quick, Standard, Thorough, Paranoid);

   function Validate_All
     (Root_Path : String;
      Level     : Validation_Level := Standard) return Validation_Report;
   -- Run all applicable validators based on level

   procedure Print_Report (Report : Validation_Report);
   -- Print formatted validation report to stdout

   function Report_To_JSON (Report : Validation_Report) return String;
   -- Export report as JSON

   function Report_To_SARIF (Report : Validation_Report) return String;
   -- Export report in SARIF format for CI/CD integration

end Shell_Validator;
