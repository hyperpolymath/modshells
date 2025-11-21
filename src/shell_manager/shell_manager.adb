with Ada.Directories;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Config_Store; 
with Shell_Manager; 

-- Assuming shell_manager.ads defines Shell_Type, Shell_Status, Shell_Info, and Shell_List
package body Shell_Manager is

    -- FIX: Ensure Ada.Directories is in the use clause for Separator visibility.
    -- Use clauses for visibility
    use Ada.Directories, Ada.Strings.Unbounded, Ada.Text_IO;
    
    -- Constant signature for Is_Modularized check
    MODULARIZED_SIGNATURE : constant String := "# MODSHELLS_START - DO NOT REMOVE THIS LINE";

    -- FIX: Type must be constrained OR bounds must be given immediately. 
    -- We define a generic type and rely on the constant definition to constrain it.
    type Directory_List_Type is array (Positive range <>) of String;
    Required_Subdirectories : constant Directory_List_Type :=
      ("core", "tools", "misc", "os", "ui");
      
    ----------------------------------------------------------------------
    -- Helper function to join two path components correctly.
    ----------------------------------------------------------------------
    function Join_Path (Base : in String; Name : in String) return String is
        Path_Separator : constant Character := Separator; -- Separator is now visible
    begin
        if Base'Length > 0 and then Base(Base'Last) /= Path_Separator then
            return Base & Path_Separator & Name;
        else
            return Base & Name;
        end if;
    end Join_Path;

    ----------------------------------------------------------------------
    -- Idempotently creates the modular shell directories.
    ----------------------------------------------------------------------
    procedure Create_Modshell_Directories (Root_Path : in String) is
    begin
        if not Exists(Root_Path) then
            begin
                Create_Directory(Root_Path);
            exception
                -- FIX: Qualify ambiguous exceptions
                when Ada.Directories.Name_Error | Ada.Directories.Use_Error => raise; 
            end;
        end if;

        for Subdir_Name of Required_Subdirectories loop
            declare
                Full_Path : constant String := Join_Path(Root_Path, Subdir_Name);
            begin
                if not Exists(Full_Path) then
                    Create_Directory(Full_Path);
                end if;
            exception
                -- FIX: Qualify ambiguous exceptions
                when Ada.Directories.Name_Error | Ada.Directories.Use_Error => raise;
            end;
        end loop;
    end Create_Modshell_Directories;

    ----------------------------------------------------------------------
    -- Shell Detection Stub (v0.0 Alpha)
    ----------------------------------------------------------------------
    function Detect_Shells return Shell_List is
        -- Assumes Shell_List is an unconstrained type defined in .ads. 
        -- FIX: MUST constrain the array instance here.
        Shell_Array : Shell_List(1..3); 
    begin
        Shell_Array(1).Name := Nushell;
        Shell_Array(1).Status := Installed;
        Shell_Array(2).Name := Bash;
        Shell_Array(2).Status := Installed;
        Shell_Array(3).Name := Zsh;
        Shell_Array(3).Status := Installed;
        return Shell_Array;
    end Detect_Shells;

    ----------------------------------------------------------------------
    -- Converts Shell_Type enumeration to String.
    ----------------------------------------------------------------------
    function To_String(Shell : Shell_Type) return String is
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
    -- Idempotency Check (Checks if the config file already sources the structure).
    ----------------------------------------------------------------------
    function Is_Modularized (Shell : Shell_Type) return Boolean is
        Config_File_Path : constant String := "/tmp/mock_config.txt"; 
        File_Handle : File_Type;
        Line        : String;
        Last        : Natural;
    begin
        begin
            Open(File_Handle, In_File, Config_File_Path);
        exception
            -- FIX: Explicitly qualify Name_Error to resolve hiding ambiguity
            when Ada.IO_Exceptions.Name_Error => return False; 
            when others     => raise;
        end;
        
        while not End_Of_File(File_Handle) loop
            Get_Line(File_Handle, Line, Last);
            if Line(1..Last) = MODULARIZED_SIGNATURE(1..Last) then
                Close(File_Handle);
                return True; 
            end if;
        end loop;
        
        Close(File_Handle);
        return False;
    end Is_Modularized;

    ----------------------------------------------------------------------
    -- MISSING BODY FIX: Placeholder for Modularise_Config
    ----------------------------------------------------------------------
    procedure Modularise_Config(Shell : Shell_Type) is
        -- Stub to satisfy the package specification
    begin
        -- The only statement:
        Ada.Text_IO.Put_Line("STUB: Modularising config for " & To_String(Shell)); 
    end Modularise_Config;
end Shell_Manager;
