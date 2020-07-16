with Gnat.Command_Line; use Gnat.Command_Line;
with Gnat.Directory_Operations; use Gnat.Directory_Operations;
with Gnat.String_Split; use Gnat.String_Split;
with Gnat.Calendar.Time_IO; use Gnat.Calendar.Time_IO;
with Gnat.OS_Lib; use Gnat.OS_Lib;
-----------------------------------
with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
-----------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.strings.Unbounded.Text_IO; use Ada.strings.Unbounded.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Generic_Array_Sort; -- use Ada.Containers.Generic_Array_Sort;
-----------------------------------
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
-----------------------------------
with Ada.exceptions; use Ada.exceptions;
-----------------------------------
with System.Multiprocessors; use System.Multiprocessors;
-----------------------------------

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;


with xph_covid19; use xph_covid19;
with xph_covid19.data; use xph_covid19.data;
with serialization; use serialization;
with utilities; use utilities;

procedure simulation is

   c : country;
   data_filename : String := "../data/covid19.csv";
   start_day_index : Integer := 65; -- start day, from the beginning
   end_day_index : Integer := -1; -- end day for special cases
   steps : integer := 20;
   minimize_by_density : boolean := false;
   zoom_factor : float := 4.0;
   convergence_tolerance : constant float := 0.01;
   fore_term : integer := 400; -- days total (data+forecast)

   time1 : time;

   bend : integer := 1; -- array index for bend in forecast_ce
   first_case : integer := 6; -- arr. index in forecast_ce when 1'st case appeared

begin

   show_software_infos;

   cmd_line : loop
      case getopt ("s: d e:") is
         when ascii.nul => exit;
            when 's' => start_day_index := integer'value(parameter);  -- get the start day for modeling e.g. -s 15
         when 'e' => end_day_index := integer'value(parameter); -- get the end day for modeling e.g. -e 60
         when 'd' =>
            minimize_by_density := true; -- minimize by density, not rates
         when others =>
            null;
      end case;
   end loop cmd_line;

   c := country'value(get_argument); -- the country to model, eg NZL

   declare
      ce : country_entries_array := get_country_data (data_filename, c);
   begin

      end_day_index := find_end_day (ce, start_day_index, end_day_index);

      put_line("Working on " & trim(all_countries(c).name,right) & "; Pop. density " & nicef(all_countries(c).pd));
      put ("Starting at day "); put(start_day_index); new_line;
      put ("Ending at day "); put(end_day_index); new_line;

      --Counting entries
      put_line (country'Image(c) & ": " & integer'Image(ce'Length) & " entries");

      if ce'Length <= start_day_index then
         put_line ("ERROR: not enough data for requested start day (" & integer'image (start_day_index) & ")");
         raise Program_Error;
      end if;

      sort_by_date (ce);

      declare
         covid_data : country_entries_array := sanitize_covid_data (ce, all_countries (c));
         model : model_parameters;
         unknowns_a1 : unknowns_vector.Vector;
         unknowns_b1 : unknowns_vector.Vector;
         unknowns_b2 : unknowns_vector.Vector;
         unknowns_k1 : unknowns_vector.Vector;
         unknowns_k2 : unknowns_vector.Vector;
         unknowns_ssrate : unknowns_vector.Vector;
         unknowns_ssrate_by_density : unknowns_vector.Vector;
         forecast_ce : country_entries_array (1 .. fore_term);

      begin
         time1 := clock;

         build_search_set (steps, u_range, unknowns_a1, unknowns_b1, unknowns_b2, unknowns_k1, unknowns_k2);

         time1 := clock;

         compute_ssrate (c,
                         start_day_index,
                         end_day_index,
                         covid_data,
                         unknowns_a1,
                         unknowns_b1,
                         unknowns_b2,
                         unknowns_k1,
                         unknowns_k2,
                         unknowns_ssrate,
                         unknowns_ssrate_by_density);

         characterize_best_model (model,
                                  unknowns_a1,
                                  unknowns_b1,
                                  unknowns_b2,
                                  unknowns_k1,
                                  unknowns_k2,
                                  unknowns_ssrate,
                                  unknowns_ssrate_by_density,
                                  minimize_by_density);

         show_model_unknows (model);

         zoom (c,
               steps,
               start_day_index,
               end_day_index,
               covid_data,
               minimize_by_density,
               unknowns_a1,
               unknowns_b1,
               unknowns_b2,
               unknowns_k1,
               unknowns_k2,
               unknowns_ssrate,
               unknowns_ssrate_by_density,
               model,
               convergence_tolerance);


         show_model_unknows (model);

         put_line ("Evaluating all unknowns completed in " & duration'image (clock - time1) & " s.");

         -- Calculate and output
         compute_simulated_rate (c, start_day_index, covid_data, model);

         -- forecast
         compute_forecast (c, covid_data, forecast_ce, model);

         -- detect bend and 1st case for graph
         bend := detect_bend (forecast_ce);

         first_case := detect_first_case (ce, forecast_ce);

         -- produce data out
         declare
            dir_out : String := make_data_dir_out (c, start_day_index, end_day_index, covid_data, minimize_by_density);
            area : constant float := all_countries (c).area;
            csv_filename : String := dir_out & country'image (c) & "_forecast.csv";
            gp_filename : String := dir_out & country'image (c) & "_forecast";
         begin
            csv_out(csv_filename, c, end_day_index, start_day_index, covid_data, forecast_ce, minimize_by_density, model, bend, area);
            gp_out (gp_filename, c, end_day_index, start_day_index, covid_data, forecast_ce, model, bend, area, first_case);
         end;
      end;
   end;
end;
