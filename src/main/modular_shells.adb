-- src/main/modular_shells.adb
with Ada.Text_IO;
with Shell_Manager;
use Ada.Text_IO;
use Shell_Manager;

procedure Modular_Shells is
   -- Placeholders for the main execution loop (v0.0)
   Shells : Shell_List := Detect_Shells;
begin
   Put_Line("Modular Shells Utility (v0.0) - Safety First.");
   Put_Line("Detected Shells:");
   
   for I in Shells'Range loop
      Put_Line("  - " & To_String(Shells(I).Name) & " (" & Shell_Status'Image(Shells(I).Status) & ")");
   end loop;
   
   -- NOTE: Full installation and modularisation logic will be implemented here in v0.1
   -- The system currently only lists and confirms the safety framework.

end Modular_Shells;
