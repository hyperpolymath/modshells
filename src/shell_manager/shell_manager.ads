-- src/shell_manager/shell_manager.ads
-- Handles shell detection, idempotency checking, and high-level configuration logic.
package Shell_Manager is
    
    type Shell_Type is (Bash, Dash, Fish, Ion, Nushell, Tcsh, Zsh, Oils, Pwsh, Ksh);
    type Shell_Status is (Installed, Not_Installed, Can_Be_Installed);
    
    type Shell_Info is record
        Name   : Shell_Type;
        Status : Shell_Status;
    end record;
    
    type Shell_List is array (Positive range <>) of Shell_Info;
    
    function Detect_Shells return Shell_List;
    -- Stub: Returns a fixed list for v0.0.

    function To_String(Shell : Shell_Type) return String;
    
    ------------------------------------------------------------------------
    -- Configuration and Idempotency
    ------------------------------------------------------------------------
    
    -- **NEW PROCEDURE FOR DIRECTORY CREATION**
    procedure Create_Modshell_Directories (Root_Path : in String);
    -- Idempotently creates the required modular shell directories (core, tools, misc, os, ui).

    -- Idempotency Check (Rhodium Standard requirement)
    function Is_Modularized (Shell : Shell_Type) return Boolean;
    -- Checks if the shell's config file (.bashrc, etc.) already sources the modular directories.
    
    procedure Modularise_Config(Shell : Shell_Type);
    -- Safe procedure that performs backup, directory creation, and source injection.

end Shell_Manager;
