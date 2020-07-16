with Gnat.Calendar.Time_IO; use Gnat.Calendar.Time_IO;

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;

with Ada.Calendar.Formatting;

with utilities; use utilities;
with xph_covid19.data; use xph_covid19.data;


package body xph_covid19 is

   function "<" (L, R : country_entry) return Boolean is
   begin
      return L.date < R.date;
   end "<";

   function refinement_check (model, last_model : in model_parameters; minimal_improvement_percentage : float) return boolean is
      diff : float := last_model.min_rate - model.min_rate;
      percentage : float := abs(diff)/last_model.min_rate;
   begin

      if diff > 0.0 and percentage > minimal_improvement_percentage then
         put_line ("Refinement of unknowns worked. New ssrate: " & float'image(model.min_rate) & ", Old ssrate: " & float'image (last_model.min_rate) & ", improvement: " & float'image (percentage));
         put_line ("Refined unknowns. " & float'image (model.u(a1)) & ", " & float'image (model.u(b1)) & ", " & float'image (model.u(b2)) & ", " & float'image (model.u(k1)) & ", " & float'image (model.u(k2)));
         return true;
      else
         put_line ("Refinement failed. percentage: " & float'image(percentage) & ", minimal_improvement_percentage: " & float'image(minimal_improvement_percentage));
         return false;
      end if;

   end;

   function find_end_day (ce : country_entries_array; start_day_index : integer; end_day_index : integer) return integer is
      end_day : integer;
   begin
      if end_day_index > 1 and then (end_day_index - start_day_index) > 5 then --end point must be at least 5 days after start
         end_day := end_day_index;
      else
         end_day := ce'last;
      end if;

      return end_day;
   end;

   function get_number_of_days (end_date : ada.Calendar.Time; start_date : ada.Calendar.Time) return integer is
   begin
      return integer (ada.calendar.arithmetic."-" (end_date, start_date));
   end;


   function sanitize_covid_data (ce : in out country_entries_array; c_data: country_data) return country_entries_array is
      use Ada.Calendar.Formatting;
      nbr_days : integer := get_number_of_days (ce(ce'last).date, ce(ce'first).date);
      data : country_entries_vector.vector;
      days_span : integer;
   begin

      declare
         item : country_entry;
      begin
         item.date := ce(ce'first).date;
         item.day_index := 0.0;
         item.cases := ce(ce'first).cases;
         item.cumulative_cases := item.cases;
         item.cumulative_cases_density := item.cumulative_cases / c_data.area;
         item.infection_rate := ce(ce'first).infection_rate;
         data.Append (item);
      end;

      for i in 2 .. ce'Length loop
         days_span := get_number_of_days (ce (i).date, ce (i-1).date);
         if days_span > 1 then
            for j in 1 .. days_span loop
               declare
                  element : country_entry;
               begin
                  element.date := ada.calendar.arithmetic."+" (ce (i).date, Day_Count(j));
                  element.cases := 0.0;
                  data.Append (element);
               end;
            end loop;
         else
            declare
               element : country_entry;
            begin
               element.date := ce (i).date;
               element.cases := ce (i).cases;
               data.Append (element);
            end;
         end if;
      end loop;

      for i in 1 .. data.last_index loop
         data (i).day_index := float(i);
         data (i).cumulative_cases := data (i).cases + data (i - 1).cumulative_cases;
         data (i).cumulative_cases_density := data (i).cumulative_cases / c_data.area;
         data (i).infection_rate := data (i).cumulative_cases - data (i - 1).cumulative_cases;
      end loop;

      --for item of data loop
         --Put_Line ("-----------------------------");
         --Put_Line (Image(item.date));
         --Put_Line ("day_index: " & float'Image(item.day_index));
         --Put_Line ("cases: " & float'Image(item.cases));
         --Put_Line ("cumulative_cases: " & float'Image(item.cumulative_cases));
         --Put_Line ("cumulative_cases_density: " & float'Image(item.cumulative_cases_density));
         --Put_Line ("infection_rate: " & float'Image(item.infection_rate));
      --end loop;

      return to_earray (data);
   end;

   function get_narrow_unknowns_range (model : model_parameters; zoom_factor : float) return unknowns_range is
      diff : float;
      unknowns_r : unknowns_range;
   begin
      for u in unknowns'range loop
         diff := (u_range (u, 2) - u_range (u, 1)) / (2.0 * zoom_factor);
         unknowns_r (u, 1) := model.u(u) - diff;
         unknowns_r (u, 2) := model.u(u) + diff;
         put_line("Range " & u'image & ": [" & float'image(unknowns_r (u, 1)) & ", " & float'image(unknowns_r (u, 2)) & "]");
      end loop;
      return unknowns_r;
   end;


   procedure build_search_set (steps : integer;
                               unknown_r : unknowns_range;
                               unknowns_a1 : in out unknowns_vector.Vector;
                               unknowns_b1 : in out unknowns_vector.Vector;
                               unknowns_b2 : in out unknowns_vector.Vector;
                               unknowns_k1 : in out unknowns_vector.Vector;
                               unknowns_k2 : in out unknowns_vector.Vector) is

      type unknowns_increments is array (Unknown'Range) of float;

      function get_unknowns_increments (steps : integer) return unknowns_increments is
         u_increments : unknowns_increments;
      begin
         for u in unknown'Range loop
            u_increments (u) := (unknown_r (u, 2) - unknown_r (u, 1)) / float(steps);
         end loop;

         return u_increments;
      end;

      function get_center_ranges return string is
         text : unbounded_string;
      begin
         for u in unknown'Range loop
           text := text & " " & u'image & ": " & float'image(((unknown_r (u, 2) - unknown_r (u, 1))/2.0) + unknown_r (u, 1));
         end loop;

         return to_string (text);
      end;


      u_increments : unknowns_increments := get_unknowns_increments(steps);
      start_time : time;
   begin

      start_time := clock;

      unknowns_a1.Clear;
      unknowns_b1.Clear;
      unknowns_b2.Clear;
      unknowns_k1.Clear;
      unknowns_k2.Clear;

      for i in 0 .. steps loop --a1
         for j in 0 .. steps loop --b1
            for k in 0 .. steps loop --b2
               for l in 0 .. steps loop --k1
                  for m in 0 .. steps loop --k2
                     unknowns_a1.Append(unknown_r (a1, 1) + float (i) * u_increments (a1));
                     unknowns_b1.Append(unknown_r (b1, 1) + float (j) * u_increments (b1));
                     unknowns_b2.Append(unknown_r (b2, 1) + float (k) * u_increments (b2));
                     unknowns_k1.Append(unknown_r (k1, 1) + float (l) * u_increments (k1));
                     unknowns_k2.Append(unknown_r (k2, 1) + float (m) * u_increments (k2));
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      put_line ("Build search set around " & get_center_ranges & " completed in " & duration'image (clock - start_time) & " s.");
   end;



   function to_unknowns_array (vec : unknowns_vector.Vector) return unknowns_array is
      arr: unknowns_array (1..Natural (vec.Length));
   begin
      for i in arr'Range loop
         arr(i) := vec.element(i-1);
      end loop;
      return arr;
   end;


   procedure compute_ssrate (c : country;
                             start_day_index : integer;
                             end_day_index : in out integer;
                             ce : country_entries_array;
                             unknowns_a1 : unknowns_vector.Vector;
                             unknowns_b1 : unknowns_vector.Vector;
                             unknowns_b2 : unknowns_vector.Vector;
                             unknowns_k1 : unknowns_vector.Vector;
                             unknowns_k2 : unknowns_vector.Vector;
                             unknowns_ssrate : in out unknowns_vector.Vector;
                             unknowns_ssrate_by_density : in out unknowns_vector.Vector) is
      -- start_time: time;
      pop_density : float := all_countries (c).pd;
      cumulative_cases_density : float;
      infection_rate : float;
      rate_diff : float := 0.0;
      rate_diff_by_density : float := 0.0;
      ssrate : float := 0.0;
      ssrate_by_density : float := 0.0;

      a1s : unknowns_array := to_unknowns_array (unknowns_a1);
      b1s : unknowns_array := to_unknowns_array (unknowns_b1);
      b2s : unknowns_array := to_unknowns_array (unknowns_b2);
      k1s : unknowns_array := to_unknowns_array (unknowns_k1);
      k2s : unknowns_array := to_unknowns_array (unknowns_k2);

      A : float;
      first_term : float;
      second_term : float;

   begin

      unknowns_ssrate.clear;
      unknowns_ssrate_by_density.clear;

      end_day_index := find_end_day (ce, start_day_index, end_day_index);

      for u in a1s'Range loop
         -- start_time := clock;
         rate_diff := 0.0;
         ssrate := 0.0;
         rate_diff_by_density := 0.0;
         ssrate_by_density := 0.0;
         cumulative_cases_density := ce (start_day_index).cumulative_cases_density;

         for n in start_day_index .. end_day_index loop

            A := pop_density - cumulative_cases_density;

            if A < 0.0 or cumulative_cases_density < 0.0 then
               ssrate := 1.0e9;
               ssrate_by_density := 1.0e9;
               exit;
            else
               first_term := k1s(u) * (A ** a1s(u)) * (cumulative_cases_density ** b1s(u));
               second_term := k2s(u) * (cumulative_cases_density ** b2s(u));

               infection_rate := first_term - second_term;
               cumulative_cases_density := cumulative_cases_density + infection_rate;

               rate_diff := infection_rate - ce (n).infection_rate;
               ssrate := ssrate + (rate_diff * rate_diff);

               rate_diff_by_density := cumulative_cases_density - ce (n).cumulative_cases_density;
               ssrate_by_density := ssrate_by_density + (rate_diff_by_density * rate_diff_by_density);
            end if;

         end loop;

         unknowns_ssrate.append(ssrate);
         unknowns_ssrate_by_density.append(ssrate_by_density);
         -- put_line ("Evaluating compute_ssrate completed in " & duration'image (clock - start_time) & " s.");
      end loop;
   end;


   function find_smallest_ssrate (unknowns_ssrate : unknowns_vector.Vector;
                                  unknowns_ssrate_by_density : unknowns_vector.Vector;
                                  minimize_by_density : Boolean;
                                  min_rate: in out float) return Integer is
      ssrates : unknowns_vector.Vector;
      smallest : float := float'last;
      index : integer := integer'last;
   begin
      if minimize_by_density then
         ssrates := unknowns_ssrate_by_density;
      else
         ssrates := unknowns_ssrate;
      end if;

      for u in ssrates.first_index .. ssrates.Last_Index loop
         if ssrates (u) < smallest then
            index := u;
            smallest := ssrates (u);
         end if;
      end loop;
      min_rate := smallest;
      put_line ("Smallest ssrate is: " & float'image (smallest));
      return index;
   end;

   procedure characterize_best_model (model : in out model_parameters;
                                      unknowns_a1 : unknowns_vector.Vector;
                                      unknowns_b1 : unknowns_vector.Vector;
                                      unknowns_b2 : unknowns_vector.Vector;
                                      unknowns_k1 : unknowns_vector.Vector;
                                      unknowns_k2 : unknowns_vector.Vector;
                                      unknowns_ssrate : unknowns_vector.Vector;
                                      unknowns_ssrate_by_density : unknowns_vector.Vector;
                                      minimize_by_density : boolean) is
      best_unknown_set_index : integer := -1;
      min_rate : float := 0.0;
   begin
      best_unknown_set_index := find_smallest_ssrate (unknowns_ssrate, unknowns_ssrate_by_density, minimize_by_density, min_rate);

      model.u(a1) := unknowns_a1(best_unknown_set_index);
      model.u(b1) := unknowns_b1(best_unknown_set_index);
      model.u(b2) := unknowns_b2(best_unknown_set_index);
      model.u(k1) := unknowns_k1(best_unknown_set_index);
      model.u(k2) := unknowns_k2(best_unknown_set_index);
      model.min_rate := min_rate;
   end;


   procedure compute_simulated_rate (c : country;
                                     start_day_index : integer;
                                     ce : in out country_entries_array;
                                     model : model_parameters) is

      pop_density : float := all_countries (c).pd;
      cumulative_cases_density : float;
      infection_rate : float;
      A : float;
      first_term : float;
      second_term : float;

   begin

      cumulative_cases_density := ce (start_day_index).cumulative_cases_density;

      for n in start_day_index .. ce'last loop

         A := pop_density - cumulative_cases_density;

         first_term := model.u(k1) * (A ** model.u(a1)) * (cumulative_cases_density ** model.u(b1));
         second_term := model.u(k2) * (cumulative_cases_density ** model.u(b2));

         infection_rate := first_term - second_term;
         cumulative_cases_density := cumulative_cases_density + infection_rate;

         -- record simulated entries
         ce (n).cumulative_cases_simulated := cumulative_cases_density * all_countries (c).area;
         ce (n).cumulative_cases_density_simulated := cumulative_cases_density;
         ce (n).infection_rate_simulated := infection_rate;

      end loop;

      Put_Line (float'image(ce(ce'last).cumulative_cases_simulated));

   end;


   procedure zoom (c : country;
                   steps : integer;
                   start_day_index : integer;
                   end_day_index : in out integer;
                   covid_data : country_entries_array;
                   minimize_by_density : Boolean;
                   unknowns_a1 : in out unknowns_vector.Vector;
                   unknowns_b1 : in out unknowns_vector.Vector;
                   unknowns_b2 : in out unknowns_vector.Vector;
                   unknowns_k1 : in out unknowns_vector.Vector;
                   unknowns_k2 : in out unknowns_vector.Vector;
                   unknowns_ssrate : in out unknowns_vector.Vector;
                   unknowns_ssrate_by_density : in out unknowns_vector.Vector;
                   model : in out model_parameters;
                   minimal_improvement_percentage : float) is

      start_time: time;
      last_model: model_parameters;
      pop_density : float := all_countries (c).pd;
      zoom_factor : float := 4.0;
      tdif : float := 0.0;
      refine : boolean := true; --convergence check
      min_rate : float := 0.0;
   begin
      start_time := clock;
      put_line ("Starting zoom.");

      while refine loop

         last_model := model;

         build_search_set (steps, get_narrow_unknowns_range (model, zoom_factor), unknowns_a1, unknowns_b1, unknowns_b2, unknowns_k1, unknowns_k2);

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


         refine := refinement_check (model, last_model, minimal_improvement_percentage);

         if not refine then
            model := last_model;
         end if;

      end loop;
      put_line ("Evaluating zoom completed in " & duration'image (clock - start_time) & " s.");
   end;


   procedure compute_forecast (c : country;
                               ce : country_entries_array;
                               forecast_ce : in out country_entries_array;
                               model : in out model_parameters) is
      pop_density : float := all_countries (c).pd;
      area : float := all_countries (c).area;
      exc_date : integer;
   begin
      forecast_ce (ce'range) := ce (ce'range);
      model.min_rate := ce (ce'last).infection_rate;
      model.max_rate := ce (ce'last).infection_rate;

      for n in ce'last + 1 .. forecast_ce'last loop

         forecast_ce (n).date := forecast_ce (n - 1).date + 1;
         forecast_ce (n).day_index := forecast_ce (n - 1).day_index + 1.0;
         exc_date := n;
         forecast_ce (n).infection_rate := model.u(k1) * ((pop_density - forecast_ce (n - 1).cumulative_cases_density_simulated) ** model.u(a1)) * (forecast_ce (n - 1).cumulative_cases_density_simulated ** model.u(b1))-model.u(k2) * forecast_ce (n - 1).cumulative_cases_density_simulated ** model.u(b2);
         forecast_ce (n).cumulative_cases_density_simulated := forecast_ce (n - 1).cumulative_cases_density_simulated + forecast_ce (n).infection_rate_simulated;
         forecast_ce (n).cumulative_cases_simulated := forecast_ce (n).cumulative_cases_density_simulated * area;

         if forecast_ce (n).infection_rate_simulated < model.min_rate then
            model.min_rate := forecast_ce (n).infection_rate_simulated;
         end if;

         if forecast_ce (n).infection_rate_simulated > model.max_rate then
            model.max_rate := forecast_ce (n).infection_rate_simulated;
         end if;
      end loop;
   end;


   function detect_bend (c_forcast_entries : country_entries_array) return Integer is
      tdif1 : float;
      bend_percent : constant float := 0.85;
      bend : Integer := 1;
   begin
      for n in reverse c_forcast_entries'range loop
         tdif1 := c_forcast_entries (n).cumulative_cases_density_simulated / c_forcast_entries (c_forcast_entries'last).cumulative_cases_density_simulated;
         if tdif1 < bend_percent then
            bend := n;
            exit;
         end if;
      end loop;

      return bend;
   end;

   function detect_first_case (ce : country_entries_array; forecast_ce : country_entries_array) return Integer is
      first_case : Integer := 0;
   begin
      for n in ce'range loop
         if forecast_ce (n).cumulative_cases > 0.0 then
            first_case := n;
            exit;
         end if;
      end loop;

      return first_case;
   end;


end xph_covid19;
