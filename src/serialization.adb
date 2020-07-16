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
with Ada.Containers.Generic_Array_Sort;
-----------------------------------
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
-----------------------------------
with Ada.exceptions; use Ada.exceptions;
-----------------------------------

with xph_covid19.data; use xph_covid19.data;
with utilities; use utilities;

package body serialization is

   procedure show_credentials is
   begin
      put_line ("COVID 19 Progression Model v.200620");
      put_line ("XR Pharmaceuticals Ltd., 2020");
      put_line ("www.xph.co.nz");
   end;

   procedure show_usage is
   begin
      put_line ("Usage: xph_covid19 -c [number of computing cores] -s [start day] -e [end day]");
      put_line ("-d (if to minimize by density instead of rate) country_ID (e.g. NZL)");
      put_line ("covid19.csv must be in the working directory");
      put_line ("Example: xph_covid19 -s 68 -c 4 -d NZL");
   end;

   procedure show_software_infos is
   begin
      Ada.Text_IO.put_line (45 * "-");
      show_credentials;
      show_usage;
      Ada.Text_IO.put_line (45 * "-");
      new_line;
   end;


   function get_country_data (filename: String; c : Country) return country_entries_array is

      country_entries : country_entries_vector.Vector;
      inf : file_type;
      ts : Unbounded_String;
      ss : Slice_Set;

   begin

      open (inf, in_file, filename);

      while not end_of_file (inf) loop
         ts := get_line (inf);
         create (ss, to_string (ts), ",", multiple);

         if slice (ss, 9) = country'image (c) then

            declare
               element : country_entry;
               d: Day_Number := day_number'Value (slice (ss, 2));
               m: Month_Number := month_number'Value (slice (ss, 3));
               y: Year_Number := year_number'Value (slice (ss, 4));
            begin
               element.date := time_of (y, m, d);
               element.cases := float'value (slice (ss, 5));
               country_entries.Append (element);
            end;
         end if;

      end loop;

      close (inf);

      return to_country_entries_array (country_entries);
   end;

   procedure show_model_unknows (model : model_parameters) is
   begin
      ada.text_io.put_line ("Model at this point. ssrate: " & float'image (model.min_rate) & " a1: " & float'image (model.u(a1)) & " b1: " & float'image (model.u(b1)) & " b2: " & float'image (model.u(b2)) & " k1: " & float'image (model.u(k1)) & " k2: " & float'image (model.u(k2)));
   end;


   function nice_float (tf : Float) return String is
      ttf : String(1..100);
   begin
      Put (ttf, tf, 1,0);
      return trim (ttf, both);
   end;

   function make_data_dir_out (c : country;
                               start_day_index : integer;
                               end_day_index : integer;
                               ma : country_entries_array;
                               minimize_by_density : boolean) return string is
      dir_out : unbounded_string;
   begin

      if minimize_by_density then
         dir_out := to_unbounded_string(c'img & "_day_" & trim (integer'image (start_day_index), both) & "_" & trim(integer'image (end_day_index), both) & "_d\");
      else
         dir_out := to_unbounded_string(c'img & "_day_" & trim (integer'image (start_day_index), both) & "_" & trim(integer'image (end_day_index), both) & "\");
      end if;

      make_dir (to_string (dir_out));

      return to_string (dir_out);

   exception when others => return to_string (dir_out);

   end;


   procedure csv_out(fn : in string;
                     c : Country;
                     start_day_index : Integer;
                     end_day_index : Integer;
                     ma : country_entries_array;
                     maf : country_entries_array;
                     minimize_by_density : Boolean;
                     model : model_parameters;
                     bend : integer) is

      area : float := all_countries(c).area;

      function create_row_data(Cc : country_entry) return string is
         Ts : unbounded_string := To_Unbounded_String (Trim (Image (Cc.date, ISO_Date), Both)) & ", ";
         Tts : string(1..100);

      begin
         ts := ts & to_unbounded_string (trim (integer'image (integer (cc.day_index)), both)) & ", ";
         Ts := Ts & To_Unbounded_String (Trim (Integer'Image (Integer (Cc.Cases)), Both)) & ", " & To_Unbounded_String (Trim (Integer'Image (Integer (Cc.cumulative_cases)), Both)) & ", ";
         Put (Tts, Cc.cumulative_cases_density, 4, 1);
         Ts := Ts & Trim (Tts, Both) & ", ";
         Put (Tts, Cc.infection_rate, 4, 1);
         Ts := Ts & Trim (Tts, Both) & ", ";
         Put (Tts, Cc.infection_rate_simulated, 4, 1);
         Ts := Ts & Trim (Tts, Both) &", ";
         Put (Tts, Cc.cumulative_cases_density_simulated, 4, 1);
         Ts := Ts & Trim (Tts, Both) &", ";
         Put (Tts, Cc.cumulative_cases_density_simulated * area, 4, 1);
         Ts := Ts & Trim (Tts, Both); --calc. cum. cases
         return To_String(Ts);
      end;

      outf : file_type;

   begin
      ada.text_io.create (outf, out_file, fn);
      ada.text_io.put_line (outf, "#COVID-19 Progression Model. XR Pharmaceuticals Ltd. 2020, www.xph.co.nz");
      ada.text_io.put_line (outf, "# " &trim(all_countries (c).name,both) & ",Pop:," & integer'image (integer (all_countries (c).pop)) & ",Pop. density:," & float'image (all_countries (c).pd)& ", Model days: "
                            & integer'image(start_day_index) & " to " & integer'image(end_day_index) & ", Opt_by_density: " & minimize_by_density'img);
      ada.text_io.put_line (outf, "#Model parameters:,a1:," & float'image (model.u(a1)) & ",b1:," & float'image (model.u(b1)) & ",b2:," & float'image (model.u(b2)) & ",k1:," & float'image (model.u(k1)) & ",k2:," & float'image (model.u(k2))
                            & ",bend date:," & image (maf (bend).date, iso_date) & ",cumulative cases:," & integer'image(integer (maf (maf'last).cumulative_cases_density_simulated * area)));
      ada.text_io.put_line (outf, "#Date, Day,Cases,CumCases,CumDens,Rate,CalcRate,CalcDens,CalcCumCases");
      for n in maf'range loop
         ada.text_io.put_line (outf, create_row_data (maf (n)));
      end loop;
      close (outf);
   end;

   procedure gp_out (fn : in string;
                     c : Country;
                     start_day_index : Integer;
                     end_day_index : Integer;
                     ma : country_entries_array;
                     maf : country_entries_array;
                     model : model_parameters;
                     bend : integer;
                     first_case:integer) is --fn is without extension

      area : float := all_countries(c).area;
      gpf : file_type;
      tf : integer := integer(float'ceiling (maf (maf'last).cumulative_cases_density_simulated * area / 100.0) * 100.0);
      x_first : string := image (maf (first_case).date - 5, iso_date);
      x_first1 : string := image (maf (first_case).date - 3, iso_date);
      x_bend : string := image (maf (bend).date, iso_date);
      y_bend : integer := integer (maf (bend).cumulative_cases_density_simulated * area);
      every_label : integer := first_case-1;
      end_label : integer;

   begin
      if end_day_index > 0 and (end_day_index - start_day_index) > 5 then
         end_label := integer(end_day_index);
      else
         end_label := ma'last;
      end if;
      ada.text_io.create (gpf, out_file, fn & ".gp");
      ada.text_io.put_line (gpf, "set terminal pngcairo size 1200,780");
      put_line (gpf, "set output " & """" & base_name(fn) & ".png" & """");
      put_line (gpf, "set datafile separator "",""");
      put_line (gpf, "set xdata time");
      put_line (gpf, "set timefmt '%Y-%m-%d'");
      put_line (gpf, "set format x ""%d-%m""");
      put_line (gpf, "set grid");
      put_line (gpf, "set key off");
      put_line (gpf, "set xlabel ""Date""");
      put_line (gpf, "set ylabel ""Cases, cumulative""");
      put (gpf, "set yrange [0:"); put (gpf, tf); put (gpf,"]"); New_Line(gpf);
      put_line (gpf, "set xrange [""" & x_first & """:""2020-10-01""]");
      put_line (gpf, "set arrow from " & """" & x_bend & """"  & ",0 to " &  """" & x_bend & """" & ", " & y_bend'img & " nohead lw 3 lc rgb 'dark-turquoise'");
      put (gpf, "set label """ & trim (all_countries (c).name, both)  & """" & " at """ & x_first1 & ""","); put (gpf, integer(float(tf) * 0.9)); put (gpf, " left"); new_line(gpf);
      put (gpf, "set label ""Model parameters: a1: "); put (gpf, model.u(a1), 1, 3,1); put (gpf, " b1: "); put (gpf, model.u(b1), 1, 3,1); put (gpf, " b2: ");
      put (gpf, model.u(b2), 1,3, 1); put (gpf, " k1: "); put (gpf, model.u(k1),1,3, 1); put (gpf, " k2: "); put (gpf, model.u(k2), 1, 3, 1); put (gpf, """ at """ & x_first1 & ""","); put (gpf, integer(float(tf) * 0.85)); put (gpf, " left"); new_line(gpf);
      put (gpf, "set label ""Cumulative cases: "); put (gpf, integer'image(integer (maf (maf'last).cumulative_cases_density_simulated * area))); put (gpf, ". Bend at " & x_bend & """" & " at """ & x_first1 & ""","); put (gpf, integer(float(tf) * 0.8)); put (gpf, " left"); new_line(gpf);
      put (gpf, "set label ""(C) XR Pharmaceuticals Ltd. xph.co.nz, 2020"" at " & """" & x_first1 & """" & ", "); put (gpf, -integer(float(tf)*0.053)); put(gpf," left"); new_line(gpf);
      put_line (gpf, "plot '" & base_name(fn) & ".csv' every ::" & trim (every_label'img, both) & "::" & trim (integer'Image(ma'last-1), both) & " using 1:4 with points pt 7 lw 3 lc rgb 'brown',\");
      put_line (gpf, "'" & base_name(fn) & ".csv' every ::" & trim (every_label'img, both) & " using 1:9 with lines dt 2 lw 2 lc rgb 'blue',\");
      put_line (gpf, "'" & base_name(fn) & ".csv' every ::" & trim (integer'image(start_day_index-1), both) & "::" & trim (integer'Image(end_label-1), both) & " using 1:4 with points pt 7 lc rgb 'red'");
      close (gpf);
   end;

end serialization;
