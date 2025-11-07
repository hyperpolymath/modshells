-- File: modshells/src/main/modshells.adb

with Shell_Manager;  -- 1. Import the package
-- ... other necessary packages (e.g., Ada.Strings.Unbounded) ...

procedure Modshells is

   -- NOTE: This path would ideally be derived from environment variables 
   -- or configuration settings managed by your config_store package.
   -- For this example, we use a placeholder path:
   Modshell_Root_Path : constant String := 
     "~/.config/nushell/modshells"; 

begin
   -- [Application Initialisation Logic]

   -- 2. Call the new idempotent directory creation function.
   Shell_Manager.Create_Modshell_Directories(
      Root_Path => Modshell_Root_Path
   );
   
   -- [Rest of the application logic]
   
exception
   when others =>
      -- Implement robust error handling (Dependability priority)
      -- perhaps using your config_store or logging utilities here.
      raise; 
end Modshells;
