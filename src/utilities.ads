with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with xph_covid19; use xph_covid19;

package Utilities is
   
   function to_earray (vec : country_entries_vector.Vector) return country_entries_array;
   procedure launch_gnuplot (c : Country; dir_out : Unbounded_String);

end Utilities;
