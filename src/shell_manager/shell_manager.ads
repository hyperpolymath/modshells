-- src/shell_manager/shell_manager.ads
-- SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
-- Handles shell detection, idempotency checking, and high-level configuration logic.
package Shell_Manager is

    type Shell_Type is (Bash, Dash, Fish, Ion, Nushell, Tcsh, Zsh, Oils, Pwsh, Ksh);
    type Shell_Status is (Installed, Not_Installed, Can_Be_Installed);

    type Shell_Info is record
        Name   : Shell_Type;
        Status : Shell_Status;
    end record;

    type Shell_List is array (Positive range <>) of Shell_Info;

    -- Maximum number of shells we support
    Max_Shells : constant := 10;
    type Shell_List_Fixed is array (1 .. Max_Shells) of Shell_Info;

    ------------------------------------------------------------------------
    -- Shell Detection (v0.1 - Real Implementation)
    ------------------------------------------------------------------------

    function Detect_Shells return Shell_List;
    -- Detects installed shells by checking common binary paths.

    function Is_Shell_Installed (Shell : Shell_Type) return Boolean;
    -- Checks if a specific shell binary exists on the system.

    function Get_Shell_Binary_Name (Shell : Shell_Type) return String;
    -- Returns the binary name for a shell (e.g., "bash", "zsh", "nu").

    function To_String (Shell : Shell_Type) return String;
    -- Converts Shell_Type to lowercase string representation.

    ------------------------------------------------------------------------
    -- Configuration File Paths
    ------------------------------------------------------------------------

    function Get_Config_File_Path (Shell : Shell_Type) return String;
    -- Returns the path to a shell's primary configuration file.
    -- e.g., ~/.bashrc, ~/.zshrc, ~/.config/fish/config.fish

    ------------------------------------------------------------------------
    -- Backup Operations (v0.1 - New)
    ------------------------------------------------------------------------

    function Create_Backup (File_Path : in String) return String;
    -- Creates a timestamped backup of the file. Returns backup path.
    -- Format: <original>.modshells-backup-YYYYMMDD-HHMMSS

    ------------------------------------------------------------------------
    -- Shell-Specific Sourcing Syntax (v0.1 - New)
    ------------------------------------------------------------------------

    function Get_Sourcing_Block (Shell : Shell_Type; Modshells_Path : in String) return String;
    -- Generates the shell-specific sourcing code block.
    -- Includes MODSHELLS_START/END signature markers.

    ------------------------------------------------------------------------
    -- Directory Creation
    ------------------------------------------------------------------------

    procedure Create_Modshell_Directories (Root_Path : in String);
    -- Idempotently creates the required modular shell directories (core, tools, misc, os, ui).

    ------------------------------------------------------------------------
    -- Idempotency and Modularisation
    ------------------------------------------------------------------------

    function Is_Modularized (Shell : Shell_Type) return Boolean;
    -- Checks if the shell's config file already contains MODSHELLS_START signature.

    procedure Modularise_Config (Shell : Shell_Type; Modshells_Path : in String);
    -- Full modularisation: backup config, inject sourcing block if not present.

    procedure Modularise_All_Shells (Modshells_Path : in String);
    -- Modularises all detected/installed shells.

end Shell_Manager;
