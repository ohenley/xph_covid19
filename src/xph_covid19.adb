with Gnat.Calendar.Time_IO; use Gnat.Calendar.Time_IO;
-----------------------------------
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
-----------------------------------
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
-----------------------------------
with Ada.Text_IO; use  Ada.Text_IO;
-----------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-----------------------------------
with Ada.Containers; use Ada.Containers;
-----------------------------------
with System.Multiprocessors; use System.Multiprocessors;
-----------------------------------

with utilities; use utilities;
with xph_covid19.data; use xph_covid19.data;


package body xph_covid19 is

   function "<" (L, R : country_entry) return Boolean is
   begin
      return L.date < R.date;
   end "<";

   function compute_fitting_improvement (model, last_model : in model_parameters) return float is
      fitting_improvement : float := (last_model.min_rate - model.min_rate) / last_model.min_rate;
   begin
      put_line ("Fitting Improvement. Current ssrate: " & float'image(model.min_rate) & ", Previous ssrate: " & float'image (last_model.min_rate) & ", fitting improvement: " & float'image (fitting_improvement));
      return fitting_improvement;
   end;

   function determine_end_day_index (ce : country_entries_array; start_day_index : integer; end_day_index : integer) return integer is
      end_day : integer;
   begin
      if end_day_index > 1 and then (end_day_index - start_day_index) > 5 then -- end point must be at least 5 days after start
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
                  element.date := ada.calendar.arithmetic."+" (ce (i-1).date, Day_Count(j));
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

      --  for item of data loop
      --     Put_Line ("-----------------------------");
      --     Put_Line (Image(item.date));
      --     Put_Line ("day_index: " & float'Image(item.day_index));
      --     Put_Line ("cases: " & float'Image(item.cases));
      --     Put_Line ("cumulative_cases: " & float'Image(item.cumulative_cases));
      --     Put_Line ("cumulative_cases_density: " & float'Image(item.cumulative_cases_density));
      --     Put_Line ("infection_rate: " & float'Image(item.infection_rate));
      --  end loop;

      return to_country_entries_array (data);
   end;

   function get_narrow_unknowns_range (model : model_parameters; zoom_factor : float) return unknowns_range is
      diff : float;
      unknowns_r : unknowns_range;
   begin
      for u in unknowns'range loop
         diff := (u_range (u, 2) - u_range (u, 1)) / zoom_factor;
         unknowns_r (u, 1) := model.u(u) - diff;
         unknowns_r (u, 2) := model.u(u) + diff;
         put_line("Range " & u'image & ": [" & float'image(unknowns_r (u, 1)) & ", " & float'image(unknowns_r (u, 2)) & "]");
      end loop;
      return unknowns_r;
   end;


   procedure build_search_set (steps : integer;
                               unknown_r : unknowns_range;
                               ua1 : in out uarray_access;
                               ub1 : in out uarray_access;
                               ub2 : in out uarray_access;
                               uk1 : in out uarray_access;
                               uk2 : in out uarray_access) is

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

      u_a1, u_b1, u_b2, u_k1, u_k2 : uvec_access := new unknowns_vector.vector;
   begin

      start_time := clock;

      for i in 0 .. steps loop --a1
         for j in 0 .. steps loop --b1
            for k in 0 .. steps loop --b2
               for l in 0 .. steps loop --k1
                  for m in 0 .. steps loop --k2
                     u_a1.Append(unknown_r (a1, 1) + float (i) * u_increments (a1));
                     u_b1.Append(unknown_r (b1, 1) + float (j) * u_increments (b1));
                     u_b2.Append(unknown_r (b2, 1) + float (k) * u_increments (b2));
                     u_k1.Append(unknown_r (k1, 1) + float (l) * u_increments (k1));
                     u_k2.Append(unknown_r (k2, 1) + float (m) * u_increments (k2));
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      to_uarray_access (u_a1, ua1);
      to_uarray_access (u_b1, ub1);
      to_uarray_access (u_b2, ub2);
      to_uarray_access (u_k1, uk1);
      to_uarray_access (u_k2, uk2);


      put_line ("Build search set around " & get_center_ranges & " completed in " & duration'image (clock - start_time) & " s.");

   end;


   procedure compute_ssrate (c : country;
                             start_day_index : integer;
                             end_day_index : integer;
                             ce : country_entries_array;
                             a1s : uarray_access;
                             b1s : uarray_access;
                             b2s : uarray_access;
                             k1s : uarray_access;
                             k2s : uarray_access;
                             ssrates : out uarray_access;
                             ssrates_by_density : out uarray_access) is

      task type tt (first : integer; last : integer);
      task body tt is
         --start_time: time;
         pop_density : float := all_countries (c).pd;
         cumulative_cases_density : float;
         infection_rate : float;
         rate_diff : float := 0.0;
         rate_diff_by_density : float := 0.0;
         ssrate : float := 0.0;
         ssrate_by_density : float := 0.0;

         A : float;
         first_term : float;
         second_term : float;
      begin
         pop_density := all_countries (c).pd;


         for u in first .. last loop
            --start_time := clock;
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
                  first_term := k1s.all(u) * (A ** a1s.all(u)) * (cumulative_cases_density ** b1s.all(u));
                  second_term := k2s.all(u) * (cumulative_cases_density ** b2s.all(u));

                  infection_rate := first_term - second_term;
                  cumulative_cases_density := cumulative_cases_density + infection_rate;

                  rate_diff := infection_rate - ce (n).infection_rate;
                  ssrate := ssrate + (rate_diff * rate_diff);

                  rate_diff_by_density := cumulative_cases_density - ce (n).cumulative_cases_density;
                  ssrate_by_density := ssrate_by_density + (rate_diff_by_density * rate_diff_by_density);
               end if;

            end loop;

            ssrates (u) := ssrate;
            ssrates_by_density (u) := ssrate_by_density;

            --put_line ("Evaluating compute_ssrate completed in " & duration'image (clock - start_time) & " s.");

         end loop;

         --put_line ("Evaluating compute_ssrate completed in " & duration'image (clock - start_time) & " s.");

      end;

      type task_indices is array (1 .. 8) of integer;
      span : integer := (integer(a1s'length) / integer (number_of_cpus));
      firsts : task_indices;
      lasts : task_indices;
   begin
      for i in 1 .. 8 loop
         firsts(i) := (span * integer(i)) - (span - 1);
         lasts(i) := span * integer(i);
      end loop;

      declare
         t1 : tt (firsts(1), lasts(1));
         t2 : tt (firsts(2), lasts(2));
         t3 : tt (firsts(3), lasts(3));
         t4 : tt (firsts(4), lasts(4));
         t5 : tt (firsts(5), lasts(5));
         t6 : tt (firsts(6), lasts(6));
         t7 : tt (firsts(7), lasts(7));
         t8 : tt (firsts(8), lasts(8));
      begin
         null;
      end;
   end;


   function find_smallest_ssrate (ssrates : uarray_access;
                                  ssrates_by_density : uarray_access;
                                  minimize_by_density : Boolean;
                                  min_rate: in out float) return Integer is
      ssrates_access : uarray_access;
      smallest : float := float'last;
      index : integer := integer'last;
   begin
      if minimize_by_density then
         ssrates_access := ssrates_by_density;
      else
         ssrates_access := ssrates;
      end if;

      for u in ssrates_access'range loop

         if ssrates_access (u) < smallest and ssrates_access (u) > 0.0 then
            --put_line (float'image (ssrates_access (u)) & " " & integer'image (u));
            index := u;
            smallest := ssrates_access (u);
         end if;
      end loop;
      min_rate := smallest;

      put_line ("Smallest ssrate is: " & float'image (smallest));

      return index;
   end;

   procedure characterize_best_model (model : in out model_parameters;
                                      a1s : uarray_access;
                                      b1s : uarray_access;
                                      b2s : uarray_access;
                                      k1s : uarray_access;
                                      k2s : uarray_access;
                                      ssrates : uarray_access;
                                      ssrates_by_density : uarray_access;
                                      minimize_by_density : boolean) is
      best_unknown_set_index : integer := -1;
      min_rate : float := 0.0;
   begin
      best_unknown_set_index := find_smallest_ssrate (ssrates, ssrates_by_density, minimize_by_density, min_rate);

      model.u(a1) := a1s(best_unknown_set_index);
      model.u(b1) := b1s(best_unknown_set_index);
      model.u(b2) := b2s(best_unknown_set_index);
      model.u(k1) := k1s(best_unknown_set_index);
      model.u(k2) := k2s(best_unknown_set_index);
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
                   a1s : in out uarray_access;
                   b1s : in out uarray_access;
                   b2s : in out uarray_access;
                   k1s : in out uarray_access;
                   k2s : in out uarray_access;
                   ssrates : in out uarray_access;
                   ssrates_by_density : in out uarray_access;
                   model : in out model_parameters;
                   minimal_improvement_percentage : float) is

      start_time: time;
      last_model: model_parameters;
      pop_density : float := all_countries (c).pd;
      zoom_factor : float := 4.0;
      tdif : float := 0.0;
      improvement : float := 1.0; --convergence check
      min_rate : float := 0.0;

   begin
      start_time := clock;
      put_line ("Start ZOOMING.");

      while improvement > minimal_improvement_percentage loop

         last_model := model;

         build_search_set (steps, get_narrow_unknowns_range (model, zoom_factor), a1s, b1s, b2s, k1s, k2s);

         compute_ssrate (c,
                         start_day_index,
                         end_day_index,
                         covid_data,
                         a1s, b1s, b2s, k1s, k2s,
                         ssrates,
                         ssrates_by_density);

         characterize_best_model (model,
                                  a1s, b1s, b2s, k1s, k2s,
                                  ssrates,
                                  ssrates_by_density,
                                  minimize_by_density);

         improvement := compute_fitting_improvement (model, last_model);

         if improvement < 0.0 then
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


   function detect_bend (c_forcast_entries : country_entries_array; bend_percent : float) return Integer is
      tdif : float;
      bend : Integer := 1;
   begin
      for n in reverse c_forcast_entries'range loop
         tdif := c_forcast_entries (n).cumulative_cases_density_simulated / c_forcast_entries (c_forcast_entries'last).cumulative_cases_density_simulated;
         if tdif < bend_percent then
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
