with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-----------------------------------

with xph_covid19; use xph_covid19;

package xph_covid19.utilities is
   
   function to_country_entries_array (vec : country_entries_vector.vector) return country_entries_array;
   
   function to_country_entries_vector(arr : country_entries_array) return country_entries_vector.vector;
   
   procedure to_uarray_access (vec_access : uvec_access; arr_access: in out uarray_access);
   
   procedure launch_gnuplot (c : Country; dir_out : Unbounded_String);

end xph_covid19.utilities;
