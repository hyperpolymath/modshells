with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Directories; 
with Ada.Text_IO; 
with Ada.IO_Exceptions;

package body Config_Store is
    
    -- Fix: Use clause needed for String_Access operations (like != null)
    use Ada.Strings.Unbounded; 
    
    ENV_VAR_NAME : constant String := "MODSHELLS_CONFIG_PATH";

    ----------------------------------------------------------------------
    -- Helper function to get the current user's home directory robustly.
    ----------------------------------------------------------------------
    function Get_Home_Directory return String is
    begin
        --  Ada.Environment_Variables.Value returns a String (not an access
        --  type); guard with Exists rather than a null check.
        if Ada.Environment_Variables.Exists ("HOME") then
            return Ada.Environment_Variables.Value ("HOME");
        else
            return Ada.Directories.Current_Directory;
        end if;
    exception
        when Ada.IO_Exceptions.Name_Error => 
            return Ada.Directories.Current_Directory; 
        when others =>
            raise;
    end Get_Home_Directory;

    -- Canonical default config root: ~/.config/modshells (matches the deployed
    -- estate layout). MODSHELLS_CONFIG_PATH overrides.
    DEFAULT_ROOT_PATH : constant String :=
        Get_Home_Directory & "/.config/modshells";

    ----------------------------------------------------------------------
    -- Implements the robust retrieval of the modular shell root path.
    ----------------------------------------------------------------------
    function Get_Modshell_Root_Path return String is
    begin
        if Ada.Environment_Variables.Exists (ENV_VAR_NAME) then
            return Ada.Environment_Variables.Value (ENV_VAR_NAME);
        else
            return DEFAULT_ROOT_PATH;
        end if;
    exception
        when others =>
            Ada.Text_IO.Put_Line("Error retrieving config path. Raising exception.");
            raise;
    end Get_Modshell_Root_Path;
    
end Config_Store;
