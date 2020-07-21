with Gnat.Command_Line; use Gnat.Command_Line;
-----------------------------------
with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
-----------------------------------
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-----------------------------------
with Ada.Calendar; use Ada.Calendar;
-----------------------------------
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
-----------------------------------

with xph_covid19; use xph_covid19;
with xph_covid19.data; use xph_covid19.data;
with serialization; use serialization;
with utilities; use utilities;

procedure simulation is

   data_filename : string := "../data/covid19.csv";

   c : country;
   start_day_index : integer := 65; -- start day, from the beginning
   end_day_index : integer := -1; -- end day for special cases
   steps : integer := 20;
   minimize_by_density : boolean := false;
   zoom_factor : float := 4.0;
   minimal_improvement_percentage : constant float := 0.3;
   fore_term : integer := 400; -- days total (data+forecast)
   bend_percent : float := 0.85;

   i,j,k,l,m : integer := steps + 1;

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
      size : integer := i*j*k*l*m;
      ce : country_entries_array := get_country_data (data_filename, c);

      ua1 : uarray_access := new unknowns_array (1 .. size);
      ub1 : uarray_access := new unknowns_array (1 .. size);
      ub2 : uarray_access := new unknowns_array (1 .. size);
      uk1 : uarray_access := new unknowns_array (1 .. size);
      uk2 : uarray_access := new unknowns_array (1 .. size);

      ssrates : uarray_access := new unknowns_array (1 .. size);
      ssrates_by_density : uarray_access := new unknowns_array (1 .. size);

   begin

      end_day_index := determine_end_day_index (ce, start_day_index, end_day_index);

      show_simulation_configuration (c, start_day_index, end_day_index, ce);

      if ce'Length <= start_day_index then
         put_line ("ERROR: not enough data for requested start day (" & integer'image (start_day_index) & ")");
         raise Program_Error;
      end if;

      sort_by_date (ce);

      build_search_set (steps, u_range, ua1, ub1, ub2, uk1, uk2);

      declare
         covid_data : country_entries_array := sanitize_covid_data (ce, all_countries (c));
         model : model_parameters;
         forecast_ce : country_entries_array (1 .. fore_term);
      begin

         time1 := clock;

         compute_ssrate (c,
                         start_day_index,
                         end_day_index,
                         covid_data,
                         ua1,
                         ub1,
                         ub2,
                         uk1,
                         uk2,
                         ssrates,
                         ssrates_by_density);

         characterize_best_model (model,
                                  ua1,
                                  ub1,
                                  ub2,
                                  uk1,
                                  uk2,
                                  ssrates,
                                  ssrates_by_density,
                                  minimize_by_density);

         show_model_unknows (model);


         zoom (c,
               steps,
               start_day_index,
               end_day_index,
               covid_data,
               minimize_by_density,
               ua1,
               ub1,
               ub2,
               uk1,
               uk2,
               ssrates,
               ssrates_by_density,
               model,
               minimal_improvement_percentage);


         show_model_unknows (model);

         put_line ("Evaluating all unknowns completed in " & duration'image (clock - time1) & " s.");

         compute_simulated_rate (c, start_day_index, covid_data, model);

         compute_forecast (c, covid_data, forecast_ce, model);

         bend := detect_bend (forecast_ce, bend_percent);

         first_case := detect_first_case (ce, forecast_ce);

         -- produce data out
         declare
            dir_out : String := make_data_dir_out (c, start_day_index, end_day_index, covid_data, minimize_by_density);
            csv_filename : String := dir_out & country'image (c) & "_forecast.csv";
            gp_filename : String := dir_out & country'image (c) & "_forecast";
         begin
            csv_out(csv_filename, c, end_day_index, start_day_index, covid_data, forecast_ce, minimize_by_density, model, bend);
            gp_out (gp_filename, c, end_day_index, start_day_index, covid_data, forecast_ce, model, bend, first_case);
         end;
      end;
   end;
end;
