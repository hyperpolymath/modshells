with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Directories; 
with Ada.Text_IO; 

package body Config_Store is
    
    use Ada.Environment_Variables;
    use Ada.Strings.Unbounded;
    
    ENV_VAR_NAME : constant String := "MODSHELLS_CONFIG_PATH";

    function Get_Home_Directory return String is
    begin
        declare
            Home_Path_Ptr : constant String_Access := Value("HOME");
        begin
            if Home_Path_Ptr /= null then
                return Home_Path_Ptr.all;
            else
                return Ada.Directories.Current_Directory; 
            end if;
        exception
            when Name_Error => 
                return Ada.Directories.Current_Directory; 
            when others =>
                raise;
        end;
    end Get_Home_Directory;

    DEFAULT_ROOT_PATH : constant String := 
        Get_Home_Directory & Ada.Directories.Separator & ".config/nushell/modshells";

    function Get_Modshell_Root_Path return String is
        Path_Value : String := "";
        Env_Value_Ptr : String_Access;
    begin
        -- 1. Check for the environment variable (Accessibility & Interoperability)
        Env_Value_Ptr := Value(ENV_VAR_NAME);

        if Env_Value_Ptr /= null then
            Path_Value := Env_Value_Ptr.all;
        else
            -- 2. Fall back to the calculated default path (Dependability)
            Path_Value := DEFAULT_ROOT_PATH;
        end if;
        
        return Path_Value;
        
    exception
        when others =>
            Ada.Text_IO.Put_Line("Error retrieving config path. Raising exception.");
            raise;
    end Get_Modshell_Root_Path;
    
    -- ... Placeholder for other functions in Config_Store package body ...

end Config_Store;