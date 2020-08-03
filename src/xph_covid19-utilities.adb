with Gnat.OS_Lib; use Gnat.OS_Lib;
with Gnat.Directory_Operations; use Gnat.Directory_Operations;
-----------------------------------

with Ada.Text_IO; use  Ada.Text_IO;

package body xph_covid19.utilities is

   function to_country_entries_array (vec : country_entries_vector.Vector) return country_entries_array is
      arr: country_entries_array (1 .. natural (vec.length));
   begin
      for i in arr'range loop
         arr(i) := vec.element(i-1);
      end loop;
      return arr;
   end;

   function to_country_entries_vector(arr : country_entries_array) return country_entries_vector.vector is
      vec : country_entries_vector.vector;
   begin
      for i in arr'range loop
         vec.append (arr(i));
      end loop;
      return vec;
   end;

   procedure to_uarray_access (vec_access : uvec_access; arr_access: in out uarray_access) is

   begin
      for i in vec_access.first_index .. vec_access.last_index loop
         arr_access(i+1) := vec_access.all.element(i);
      end loop;
   end;

   procedure launch_gnuplot (c : country; dir_out : unbounded_string) is
      gpcommand : string := "gnuplot " & country'image (c) & "_forecast.gp";
      args        : argument_list_access := argument_string_to_list (gpcommand);
      exit_status : integer;
   begin
      change_dir (to_string (dir_out));
      exit_status := spawn (program_name => args (args'first).all, args => args (args'first + 1 .. args'last));
      free (args);
      change_dir ("..");
   end;

end xph_covid19.utilities;
