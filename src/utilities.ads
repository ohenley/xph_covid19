with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-----------------------------------

with xph_covid19; use xph_covid19;

package utilities is
   
   function to_country_entries_array (vec : country_entries_vector.Vector) return country_entries_array;
   procedure launch_gnuplot (c : Country; dir_out : Unbounded_String);

end utilities;
