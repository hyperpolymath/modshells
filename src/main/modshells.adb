with Shell_Manager;
with Config_Store;
with Ada.Text_IO;

procedure Modshells is

   Config_Path : constant String := Config_Store.Get_Modshell_Root_Path;

begin
   Ada.Text_IO.Put_Line("Starting modshells initialisation...");
   Ada.Text_IO.Put_Line("Configuration path: " & Config_Path);

   -- Idempotent creation of directories (core, tools, misc, os, ui)
   Shell_Manager.Create_Modshell_Directories(
      Root_Path => Config_Path
   );
   
   Ada.Text_IO.Put_Line("Modular directories created idempotently.");
   
   -- [Continue with application logic, such as shell detection, etc.]
   
exception
   when others =>
      declare
         Error_Msg : constant String := 
            "FATAL ERROR: Modshells failed during initial setup.";
      begin
         Ada.Text_IO.Put_Line(Error_Msg);
         raise; 
      end;
end Modshells;