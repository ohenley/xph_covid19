with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-----------------------------------

with xph_covid19; use xph_covid19;

package utilities is
   
   function to_country_entries_array (vec : country_entries_vector.vector) return country_entries_array;
   
   procedure to_uarray_access (vec_access : uvec_access; arr_access: in out uarray_access);
   
   procedure launch_gnuplot (c : Country; dir_out : Unbounded_String);

end utilities;
