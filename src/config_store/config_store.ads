-- src/config_store/config_store.ads
-- Handles LMDB interaction for transactional backup and state logging.
package Config_Store is

   -- Initializes the LMDB environment and opens the configuration map.
   procedure Initialize_Store;

   -- Stores a full configuration file backup transactionally.
   procedure Store_Backup(
      Shell_Name : in Shell_Manager.Shell_Type;
      File_Path  : in String;
      Content    : in String);

   -- Retrieves the last stored backup content for a shell.
   function Retrieve_Backup(Shell_Name : Shell_Manager.Shell_Type) return String;
   
   -- Rollback procedure (Future TUI/CLI command)
   procedure Rollback_To_Last_State(Shell_Name : Shell_Manager.Shell_Type);
   
end Config_Store;
