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
        -- FIX: Must explicitly use the fully qualified access type
        Home_Path_Ptr : constant Ada.Strings.Unbounded.String_Access := Ada.Environment_Variables.Value("HOME"); 
    begin
        if Home_Path_Ptr /= null then
            return Home_Path_Ptr.all; 
        else
            return Ada.Directories.Current_Directory; 
        end if;
    exception
        when Ada.IO_Exceptions.Name_Error => 
            return Ada.Directories.Current_Directory; 
        when others =>
            raise;
    end Get_Home_Directory;

    -- The default path for Nushell configuration on Linux/Kinoite
    -- FIX: Use fully qualified Separator
    DEFAULT_ROOT_PATH : constant String := 
        Get_Home_Directory & Ada.Directories.Separator & ".config/nushell/modshells";

    ----------------------------------------------------------------------
    -- Implements the robust retrieval of the modular shell root path.
    ----------------------------------------------------------------------
    function Get_Modshell_Root_Path return String is
        Path_Value : String := "";
        -- FIX: Must explicitly use the fully qualified access type
        Env_Value_Ptr : constant Ada.Strings.Unbounded.String_Access := Ada.Environment_Variables.Value(ENV_VAR_NAME); 
    begin
        if Env_Value_Ptr /= null then
            Path_Value := Env_Value_Ptr.all;
        else
            Path_Value := DEFAULT_ROOT_PATH;
        end if;
        
        return Path_Value;
        
    exception
        when others =>
            Ada.Text_IO.Put_Line("Error retrieving config path. Raising exception.");
            raise;
    end Get_Modshell_Root_Path;
    
end Config_Store;
