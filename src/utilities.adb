with Gnat.OS_Lib; use Gnat.OS_Lib;
with Gnat.Directory_Operations; use Gnat.Directory_Operations;



package body Utilities is

   function to_earray (vec : country_entries_vector.Vector) return country_entries_array is
      arr: country_entries_array (1..Natural (vec.Length));
   begin
      for i in arr'Range loop
         arr(i) := vec.element(i-1);
      end loop;
      return arr;
   end;


   procedure launch_gnuplot (c : Country; dir_out : Unbounded_String) is
      gpcommand : string := "gnuplot " & country'image (c) & "_forecast.gp";
      args        : argument_list_access := argument_string_to_list (gpcommand);
      exit_status : integer;
   begin

      change_dir (to_string (dir_out));
      exit_status := spawn (program_name => args (args'first).all, args => args (args'first + 1 .. args'last));
      free (args);
      change_dir ("..");
   end;

end Utilities;
