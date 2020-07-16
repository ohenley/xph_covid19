with gnat.command_line, ada.Text_IO, ada.Integer_Text_IO, ada.Float_Text_IO, ada.Strings.Unbounded, ada.strings.Unbounded.Text_IO,Ada.Containers.Generic_Array_Sort,gnat.Directory_Operations;
with ada.Strings, ada.Strings.Fixed,  ada.Numerics.Elementary_Functions,ada.Calendar, ada.exceptions,chern.country,gnat.String_Split,ada.Calendar.Arithmetic,chern.tasking,gnat.Calendar.Time_IO, gnat.OS_Lib;
use gnat.command_line, ada.Text_IO, ada.Integer_Text_IO, ada.Float_Text_IO, ada.Strings.Unbounded, ada.strings.Unbounded.Text_IO,chern.tasking,gnat.Directory_Operations, gnat.OS_Lib;
use ada.Strings, ada.Strings.Fixed,  ada.Numerics.Elementary_Functions,ada.Calendar, ada.exceptions,chern.country,gnat.String_Split,ada.Calendar.Arithmetic,gnat.Calendar.Time_IO;
procedure xph_covid19 is
inf,outf:file_type;
infn,outfn,ts:Unbounded_String;
ss:Slice_Set;
s:float:=2.0;--start day, from the beginning
ed:float:=0.0;--end day for special cases
c:country;
ce:integer:=0; --number of country entries in the file
bend:integer:=1;--array index for bend in maf
first_case:integer;--arr. index in maf when 1'st case appeared


   type c_entry is record
      date:ada.Calendar.Time;
      cases:float;
      day:float;--days in succession order 0,1,2,..
      cum:float:=0.0;--cumulative cases;
      cumd:float;--cumulative pop. density (cum/area)
      rate:float;
      ratec:float;--calculated rate
      cumdc: float;--calculated cumd
   end record;

type optimization is (rate,density);

   type model_p is record
      a1,b1,b2:float;--exponents
      k1,k2:float;--kin. const
      opt_type:optimization:=rate;--optimized by rate by default
      maxrate,minrate,bendrate:float;--max. and min. calc. rates, and the rate at the bend
   end record;

--starting search areas
   a1_range:array(1..2)of float:=(1.0e-10,1.0);
   b1_range:array(1..2)of float:=(1.0e-10,3.0);
   b2_range:array(1..2)of float:=(1.0e-10,1.0);
   k1_range:array(1..2)of float:=(1.0e-10,1.0);
   k2_range:array(1..2)of float:=(1.0e-10,5.0);
   steps:float:=20.0;


   model_set1,last_model_set:model_p;

   type earray is array (integer range <>) of c_entry;
   type earrayp is access earray;

 function "<" (L, R : c_entry) return Boolean is
   begin
      return L.date < R.date;
   end "<";

 procedure Sort is new Ada.Containers.Generic_Array_Sort (integer, c_entry, earray);

     type temp_date is record
      d:Day_Number;
	m: Month_Number;
      y:Year_Number;
      end record;

--Makes strings for the csv file, used in gnuplot
   FUNCTION Csv_Out(Cc:C_Entry) RETURN String   IS
      Ts:Unbounded_String:=To_Unbounded_String(Trim(Image(Cc.date,ISO_Date),Both)) & ", ";
      Tts:String(1..100);
   BEGIN
      ts := ts & to_unbounded_string (trim (integer'image (integer (cc.day)), both)) & ", ";
      Ts:=Ts & To_Unbounded_String(Trim(Integer'Image(Integer(Cc.Cases)),Both)) & ", " & To_Unbounded_String(Trim(Integer'Image(Integer(Cc.Cum)),Both)) & ", ";
      Put(Tts,Cc.Cumd,4,1); Ts:=Ts&Trim(Tts,Both)&", ";
      Put(Tts,Cc.Rate,4,1); Ts:=Ts&Trim(Tts,Both)&", ";
      Put(Tts,Cc.Ratec,4,1); Ts:=Ts&Trim(Tts,Both)&", ";
      Put(Tts,Cc.Cumdc,4,1); Ts:=Ts&Trim(Tts,Both)&", ";
      Put(Tts,Cc.Cumdc*all_countries(c).area,4,1); Ts:=Ts&Trim(Tts,Both);--calc. cum. cases
      RETURN To_String(Ts);
   END Csv_Out;

   FUNCTION Nicef (Tf:IN Float) RETURN String IS
      Ttf:String(1..100);
   BEGIN
      Put(Ttf,Tf, 1,0);
      RETURN Trim(Ttf,Both);
   END Nicef;


minimize_by_density:boolean:=false;
convergence_tolerance:constant float:=0.01;
fore_term:integer:=400;--days total (data+forecast)

   FUNCTION convergence_check (m1, m2 : IN model_p)return boolean IS
   BEGIN
      IF ABS (m1.a1 - m2.a1) > convergence_tolerance OR ELSE ABS (m1.b1 - m2.b1) > convergence_tolerance OR ELSE ABS (m1.b2 - m2.b2) > convergence_tolerance
        OR ELSE ABS (m1.k1 - m2.k1) > convergence_tolerance OR ELSE ABS (m1.k2 - m2.k2) > convergence_tolerance THEN
         put_line ("Convergence failed: " & float'image (ABS (m1.a1 - m2.a1)) & ", " & float'image (ABS (m1.b1 - m2.b1)) & ", " & float'image (ABS (m1.b2 - m2.b2)) & ", " & float'image (ABS (m1.k1 - m2.k1)) & ", " & float'image (ABS (m1.k2 - m2.k2)));
         RETURN false;
         end if;
         put_line ("CONVERGED");
         RETURN true;
      END convergence_check;

begin
ada.text_io.put_line(45*"-");
ada.text_io.put_line("COVID 19 Progression Model v.200620");
ada.text_io.put_line("XR Pharmaceuticals Ltd., 2020");
ada.text_io.put_line("www.xph.co.nz");
ada.text_io.put_line("Usage: xph_covid19 -c [number of computing cores] -s [start day] -e [end day]");
ada.text_io.put_line("-d (if to minimize by density instead of rate) country_ID (e.g. NZL)");
ada.text_io.put_line("covid19.csv must be in the working directory");
ada.text_io.put_line("Example: xph_covid19 -s 68 -c 4 - d NZL");
ada.text_io.put_line(45*"-");
   new_line;

cores:=1; --def. # of processors
cmd_line:   loop
      case getopt ("c: s: d e:") is
         when ascii.nul => exit;
	 when 's' =>s:=float'Value(Parameter);-- get the start day for modeling e.g. -s 15
         WHEN 'e' => ed:= float'value (parameter); -- get the end day for modeling e.g. -e 60
    	 when 'c' =>cores:=integer'Value(Parameter); --number of cores
when 'd' =>
minimize_by_density:=true; --minimize by density, not rates
model_set1.opt_type:=density;
last_model_set.opt_type:=density;
         when others =>
null;
      end case;
   end loop cmd_line;
--           ts:=To_Unbounded_String(Get_Argument);
--        put_line("arg " & ts);
c:=country'value(Get_Argument);--the country to model, eg NZL

   ada.Text_IO.put_line("Working on " & trim(all_countries(c).name,right) & "; Pop. density " & nicef(all_countries(c).popdens));
   put ("Starting at day ");put(s);new_line;

infn:=to_unbounded_string("covid19.csv");
--Counting entries
open (inf, in_file, to_string(infn));
    while not end_of_file(inf) loop
      ts:=get_line (inf);
      create(ss,to_string(ts),",",multiple);
      if slice(ss,9)=country'Image(c) then
	 ce:=ce+1;
            end if;
   end loop;
   put_line (country'Image(c) & ": " & integer'Image(ce) & " entries");
   if ce<=integer(s) then
      put_line("ERROR: not enough data for requested start day (" & integer'image(integer(s)) & ")");
      raise Program_Error;
   end if;

   DECLARE
      ma : earrayp := NEW earray (1 .. ce);
      maf : earrayp := NEW earray (1 .. fore_term);
      td : temp_date;
      cnt : integer := 0; --counter
      day_inc : float := 1.0; --differnce in days
      area : CONSTANT float := all_countries (c).area;
      pd : CONSTANT float := all_countries (c).popdens;
      end_day:Unbounded_String;
dir_out:Unbounded_String;
      PROTECTED set_check IS
         PROCEDURE check_data (a : IN model_p; ssum : IN float);
         PROCEDURE reset;
         FUNCTION get RETURN model_p;
      PRIVATE
         current_set : model_p; --current good model parameters
         current_min : float := 1.0e9; --current min. sum of squares in rates
      END set_check;

--creates csv file used by gnuplot
      PROCEDURE csv_out(fn:in string) IS
      BEGIN
         if ed> 0.0 and then (ed-s)>5.0 then
      end_day:=To_Unbounded_String(integer'image(integer(ed)));
      else
      end_day:=To_Unbounded_String(integer'image(ma.all'Last));-- & (all, " & ma.all'last'img & ")");
      end if;
         ada.text_io.create (outf, out_file, fn);
         ada.text_io.put_line (outf, "#COVID-19 Progression Model. XR Pharmaceuticals Ltd. 2020, www.xph.co.nz");
         ada.text_io.put_line (outf, "# " &trim(all_countries (c).name,both) & ",Pop:," & integer'image (integer (all_countries (c).pop)) & ",Pop. density:," & float'image (all_countries (c).popdens)& ", Model days: "
         & integer'image(integer(s)) & " to " & To_String(end_day)& ", Opt_by_density: " & minimize_by_density'img);
         ada.text_io.put_line (outf, "#Model parameters:,a1:," & float'image (model_set1.a1) & ",b1:," & float'image (model_set1.b1) & ",b2:," & float'image (model_set1.b2) & ",k1:," & float'image (model_set1.k1) & ",k2:," & float'image (model_set1.k2)
         & ",bend date:," & image (maf (bend).date, iso_date)& ",cumulative cases:," & integer'image(integer (maf (maf.ALL'last).cumdc*area)));
         ada.text_io.put_line (outf, "#Date, Day,Cases,CumCases,CumDens,Rate,CalcRate,CalcDens,CalcCumCases");
         FOR n IN maf.ALL'range LOOP
            ada.text_io.put_line (outf, csv_out (maf (n)));
         END LOOP;
         close (outf);
      END csv_out;

      PROCEDURE gp_out (fn : IN string) IS --fn is without extension
         gpf : file_type;
         tf : integer:=integer(float'ceiling (maf (maf.ALL'last).cumdc*area / 100.0) * 100.0);
         x_first : string := image (maf (first_case).date - 5, iso_date);
         x_first1 : string := image (maf (first_case).date - 3, iso_date);
         x_bend : string := image (maf (bend).date, iso_date);
         y_bend : integer := integer (maf (bend).cumdc*area);
         every_label : integer := first_case-1;
         end_label:integer;
      BEGIN
                        if ed>1.0 and then (ed-s)>5.0 then --end point must be at least 5 days after start
                        end_label:=integer(ed);
                        else
                        end_label:=ma.ALL'last;
                        end if;
         ada.text_io.create (gpf, out_file, fn & ".gp");
         ada.text_io.put_line (gpf, "set terminal pngcairo size 1200,780");
         put_line (gpf, "set output " & """" & base_name(fn) & ".png" & """");
         ada.text_io.put_line (gpf, "set datafile separator "",""");
         ada.text_io.put_line (gpf, "set xdata time");
         ada.text_io.put_line (gpf, "set timefmt '%Y-%m-%d'");
         ada.text_io.put_line (gpf, "set format x ""%d-%m""");
         ada.text_io.put_line (gpf, "set grid");
         ada.text_io.put_line (gpf, "set key off");
         ada.text_io.put_line (gpf, "set xlabel ""Date""");
         ada.text_io.put_line (gpf, "set ylabel ""Cases, cumulative""");
         ada.text_io.put (gpf, "set yrange [0:"); put (gpf, tf); put (gpf,"]");New_Line(gpf);
         ada.text_io.put_line (gpf, "set xrange [""" & x_first & """:""2020-10-01""]");
         ada.text_io.put_line (gpf, "set arrow from " & """" & x_bend & """"  & ",0 to " &  """" & x_bend & """" & ", " & y_bend'img & " nohead lw 3 lc rgb 'dark-turquoise'");
         put (gpf, "set label """ & trim (all_countries (c).name, both)  & """" & " at """ & x_first1 & ""","); put (gpf, integer(float(tf) * 0.9)); put (gpf, " left"); new_line(gpf);
         put (gpf, "set label ""Model parameters: a1: "); put (gpf, model_set1.a1, 1, 3,1); put (gpf, " b1: "); put (gpf, model_set1.b1, 1, 3,1); put (gpf, " b2: ");
         put (gpf, model_set1.b2, 1,3, 1); put (gpf, " k1: "); put (gpf, model_set1.k1,1,3, 1); put (gpf, " k2: "); put (gpf, model_set1.k2, 1, 3, 1); put (gpf, """ at """ & x_first1 & ""","); put (gpf, integer(float(tf) * 0.85)); put (gpf, " left"); new_line(gpf);
         put (gpf, "set label ""Cumulative cases: "); put (gpf, integer'image(integer (maf (maf.ALL'last).cumdc*area))); put (gpf, ". Bend at " & x_bend & """" & " at """ & x_first1 & ""","); put (gpf, integer(float(tf) * 0.8)); put (gpf, " left"); new_line(gpf);
         put (gpf, "set label ""(C) XR Pharmaceuticals Ltd. xph.co.nz, 2020"" at " & """" & x_first1 & """" & ", "); put (gpf, -integer(float(tf)*0.053)); put(gpf," left"); new_line(gpf);
         ada.text_io.put_line (gpf, "plot '" & base_name(fn) & ".csv' every ::" & trim (every_label'img, both) & "::" & trim (integer'Image(ma.ALL'last-1), both) & " using 1:4 with points pt 7 lw 3 lc rgb 'brown',\");
         ada.text_io.put_line (gpf, "'" & base_name(fn) & ".csv' every ::" & trim (every_label'img, both) & " using 1:9 with lines dt 2 lw 2 lc rgb 'blue',\");
         ada.text_io.put_line (gpf, "'" & base_name(fn) & ".csv' every ::" & trim (integer'image(integer(s)-1), both) & "::" & trim (integer'Image(end_label-1), both) & " using 1:4 with points pt 7 lc rgb 'red'");
         close (gpf);
      END gp_out;

      PROTECTED BODY set_check IS
         PROCEDURE check_data (a : IN model_p; ssum : IN float) IS
         BEGIN
            IF ssum < current_min THEN
               current_min := ssum;
               current_set := a;
            END IF;
         END check_data;
         PROCEDURE reset IS
         BEGIN
            current_min := 1.0e9;
         END reset;
         FUNCTION get RETURN model_p IS
         BEGIN
            RETURN current_set;
         END get;
      END set_check;

      time1:time;

      PROCEDURE main_cycle1 (x1, x2 : IN integer) IS --rough optimization
         model_set : model_p;
         ssrate : float := 0.0; --rate sum of squares
         trate,tcumdc:float;
         ratediff : float;
         a1_inc : float := (a1_range (2)-a1_range (1)) / steps; --constants increments in the cycle
         b1_inc : float := (b1_range (2)-b1_range (1)) / steps;
         b2_inc : float := (b2_range (2)-b2_range (1)) / steps;
         k1_inc : float := (k1_range (2)-k1_range (1)) / steps;
         k2_inc : float := (k2_range (2)-k2_range (1)) / steps;
              BEGIN
         FOR i IN 0 .. integer (steps) LOOP --a1
            put (i); put ("/"); put (integer(steps));put("... ");
            FOR j IN 0 .. integer (steps) LOOP --b1
               FOR k IN 0 .. integer (steps) LOOP --b2
                  FOR l IN x1 .. x2 LOOP --k1
                     FOR m IN 0 .. integer (steps) LOOP --k2
--                          put_line ("s=" & s'img & " ma first: " & ma.all'First'img );
--                          delay 10.0;
                        model_set.a1 := a1_range (1)+float (i) * a1_inc;
                        model_set.b1 := b1_range (1)+float (j) * b1_inc;
                        model_set.b2 := b2_range (1)+float (k) * b2_inc;
                        model_set.k1 := k1_range (1)+float (l) * k1_inc;
                        model_set.k2 := k2_range (1)+float (m) * k2_inc;
                        tcumdc:=ma(integer(s)-1).cumd;
--                          put_line ("New set: a1: " & float'image (model_set.a1) & " b1: " & float'image (model_set.b1) & " b2: " & float'image (model_set.b2) & " k1: " & float'image (model_set.k1) & " k2: " & float'image (model_set.k2) & " ssrate: " & float'image (ssrate));
                        DECLARE
                        endloop:integer;
                        BEGIN
                        if ed>1.0 and then (ed-s)>5.0 then --end point must be at least 5 days after start
                        endloop:=integer(ed);
                        else
                        endloop:=ma.ALL'last;
                        end if;
                           FOR n IN integer (s) .. endloop LOOP
                              day_inc := (ma (n).day - ma (n - 1).day);
--                                put_line ("pd-tcumdc=" & float'image (pd - tcumdc) & " tcumdc=" & float'image (tcumdc) & " dayinc=" & float'image (day_inc));
                              trate := (model_set.k1 * ((pd - tcumdc) ** model_set.a1) * (tcumdc ** model_set.b1)-model_set.k2 * tcumdc ** model_set.b2) / day_inc;
--                                put_line ("trate=" & float'image (trate) & " first half=" & float'image (model_set.k1 * ((pd - tcumdc) ** model_set.a1) * (tcumdc ** model_set.b1)) & " second half=" & float'image (model_set.k2 * (tcumdc ** model_set.b2)));
                              tcumdc := tcumdc + trate * day_inc;
                              if minimize_by_density then
                              ratediff := tcumdc - ma (n).cumd;
                              else
                                                            ratediff := trate - ma (n).rate;
                              end if;
                                                            ssrate := ssrate + ratediff * ratediff;
                           END LOOP;
                           exception when ada.Numerics.Argument_Error=> ssrate:=1.0e9;
                        END;
--                          put_line ("a1: " & float'image (model_set.a1) & " b1: " & float'image (model_set.b1) & " b2: " & float'image (model_set.b2) & " k1: " & float'image (model_set.k1) & " k2: " & float'image (model_set.k2) & " ssrate: " & float'image (ssrate));
                        set_check.check_data (model_set, ssrate);
                        ssrate := 0.0;
                     END LOOP;
                  END LOOP;
               END LOOP;
            END LOOP;
         END LOOP;
      END main_cycle1;


   BEGIN
      reset (inf);

      WHILE NOT end_of_file (inf) LOOP
         ts := get_line (inf);
         create (ss, to_string (ts), ",", multiple);
         IF slice (ss, 9) = country'image (c) THEN
            cnt := cnt + 1; --array  index
            td.d := day_number'value (slice (ss, 2));
            td.m := month_number'value (slice (ss, 3));
            td.y := year_number'value (slice (ss, 4));
            ma (cnt).date := time_of (td.y, td.m, td.d);
            ma (cnt).cases := float'value (slice (ss, 5));
         END IF;
      END LOOP;
      sort (ma.ALL);
      close (inf);
      ma (1).day := 0.0;
      ma (1).cum := ma (1).cases;
      ma (1).cumd:= ma (1).cum/area;
      ma (1).cumdc := ma (1).cumd;
      ma (1).rate := 0.0;
      ma (1).ratec := ma (1).cases / area;
      FOR i IN 2 .. ma'last LOOP
         ma (i).day := float (ada.calendar.arithmetic."-" (ma (i).date, ma (1).date));
         ma (i).cum := ma (i).cases + ma (i - 1).cum;
         ma (i).cumd := ma (i).cum / area;
         ma (i).rate := ((ma (i).cum - ma (i - 1).cum) / float (ada.calendar.arithmetic."-" (ma (i).date, ma (i - 1).date))) / area;
      END LOOP;
      time1 := clock;
      splitando (integer (steps), main_cycle1'access, to_unbounded_string ("Test " & integer'image (cores) & " cores"));
      put_line ("Completed in " & duration'image (clock - time1) & " s.");
      model_set1 := set_check.get;
      ada.text_io.put_line ("Final model: a1: " & float'image (model_set1.a1) & " b1: " & float'image (model_set1.b1) & " b2: " & float'image (model_set1.b2) & " k1: " & float'image (model_set1.k1) & " k2: " & float'image (model_set1.k2));
      set_check.reset;
            --Calculate and output
      FOR n IN integer(s) .. ma.ALL'last LOOP
      ma (integer(s) - 1).cumdc:= ma (integer(s) - 1).cumd;
         day_inc := (ma (n).day - ma (n - 1).day);
         ma (n).ratec := (model_set1.k1 * ((pd - ma (n - 1).cumdc) ** model_set1.a1) * (ma (n - 1).cumdc ** model_set1.b1)-model_set1.k2 * ma (n - 1).cumdc ** model_set1.b2)/day_inc;
         ma (n).cumdc := ma (n - 1).cumdc + ma (n).ratec * day_inc;
      END LOOP;
--              csv_out(country'image (c) & "_cycle 0.csv");
            last_model_set:=model_set1;
      --Zooming
      DECLARE
         zoom_factor : float := 4.0;
         tdif : float := 0.0;
         ccheck:boolean;--convergence check
      BEGIN
         FOR i IN 1 .. 5 LOOP
            tdif := (a1_range (2)-a1_range (1)) / (2.0 * zoom_factor);
            a1_range (1) := model_set1.a1 - tdif;      a1_range (2) := model_set1.a1 + tdif;
            tdif := (b1_range (2)-b1_range (1)) / (2.0 * zoom_factor);
            b1_range (1) := model_set1.b1 - tdif;      b1_range (2) := model_set1.b1 + tdif;
            tdif := (b2_range (2)-b2_range (1)) / (2.0 * zoom_factor);
            b2_range (1) := model_set1.b2 - tdif;      b2_range (2) := model_set1.b2 + tdif;
            tdif := (k1_range (2)-k1_range (1)) / (2.0 * zoom_factor);
            k1_range (1) := model_set1.k1 - tdif;      k1_range (2) := model_set1.k1 + tdif;
            tdif := (k2_range (2)-k2_range (1)) / (2.0 * zoom_factor);
            k2_range (1) := model_set1.k2 - tdif;      k2_range (2) := model_set1.k2 + tdif;
            time1 := clock;
            splitando (integer (steps), main_cycle1'access, to_unbounded_string ("Test " & integer'image (cores) & " cores"));
            ada.Text_IO.put_line ("Cycle " & i'img & " completed in " & duration'image (clock - time1) & " s.");
            model_set1 := set_check.get;
            ada.text_io.put_line ("Final model: a1: " & float'image (model_set1.a1) & " b1: " & float'image (model_set1.b1) & " b2: " & float'image (model_set1.b2) & " k1: " & float'image (model_set1.k1) & " k2: " & float'image (model_set1.k2));
            set_check.reset;
            --Calculate and output
               ma (integer (s) - 1).cumdc := ma (integer (s) - 1).cumd;
            FOR n IN integer (s) .. ma.ALL'last LOOP
               day_inc := (ma (n).day - ma (n - 1).day);
               ma (n).ratec := (model_set1.k1 * ((pd - ma (n - 1).cumdc) ** model_set1.a1) * (ma (n - 1).cumdc ** model_set1.b1)-model_set1.k2 * ma (n - 1).cumdc ** model_set1.b2) / day_inc;
               ma (n).cumdc := ma (n - 1).cumdc + ma (n).ratec * day_inc;
            END LOOP;
            ccheck:=convergence_check(model_set1,last_model_set);
            last_model_set:=model_set1;
         END LOOP;
      END;
      --Forecast
--        put_line ("Forecast...");
      declare
               exc_date:integer;
      begin
         maf (ma'range) := ma (ma'range);
         model_set1.minrate := ma (ma.ALL'last).ratec;
         model_set1.maxrate := ma (ma.ALL'last).ratec;
         FOR n IN ma.ALL'last + 1 .. maf.ALL'last LOOP
            maf (n).date := maf (n - 1).date + 1;
            maf (n).day := maf (n - 1).day + 1.0;
            exc_date:=n;
            maf (n).ratec := model_set1.k1 * ((pd - maf (n - 1).cumdc) ** model_set1.a1) * (maf (n - 1).cumdc ** model_set1.b1)-model_set1.k2 * maf (n - 1).cumdc ** model_set1.b2;
            maf (n).cumdc := maf (n - 1).cumdc + maf (n).ratec;
--            maf (n).cum := maf (n).cumdc * area;
            IF maf (n).ratec < model_set1.minrate THEN
               model_set1.minrate := maf (n).ratec;
            END IF;
            IF maf (n).ratec > model_set1.maxrate THEN
               model_set1.maxrate := maf (n).ratec;
            END IF;
         END LOOP;
         exception when ADA.NUMERICS.ARGUMENT_ERROR =>put_line ("Exception on " & image(maf(exc_date).date,ISO_Date));
         end;

      --              detect bend and 1st case for graph
      model_set1.bendrate := (model_set1.maxrate - model_set1.minrate) / 2.0;
--        put_line ("bendrate " & model_set1.bendrate'img);
      DECLARE
         mindiff : float := 1.0e6;
         tdif1 : float;
         bend_percent:constant float:=0.85;
         BEGIN
         FOR n IN REVERSE maf.ALL'range LOOP
--              tdif1 := ABS (maf (n).ratec - model_set1.bendrate);
tdif1:=maf(n).cumdc/maf (maf.ALL'last).cumdc;
--  ada.Text_IO.put ("tdif " & tdif1'img & " ");
            IF tdif1 < bend_percent THEN
--                 mindiff := tdif1;
               bend := n;
--                 put_line ("bend " & bend'img & "maf last " & maf.all'Last'img);
               exit;
            END IF;
         END LOOP;

         FOR n IN ma.ALL'range LOOP
            IF maf (n).cum > 0.0 THEN
               first_case := n;
               EXIT;
            END IF;
         END LOOP;

      END;
         if ed> 0.0 and then (ed-s)>5.0 then
      end_day:=To_Unbounded_String(integer'image(integer(ed)));
      else
      end_day:=To_Unbounded_String(integer'image(ma.all'Last));
      end if;
      if minimize_by_density then
dir_out:=c'img & "_day_" & trim(integer'image(integer(s)),both)&"_"& trim(end_day,both)&"_d\";
else
dir_out:=c'img & "_day_" & trim(integer'image(integer(s)),both)&"_"& trim(end_day,both)&"\";
end if;
declare
begin
Make_Dir(To_String(dir_out));
exception when others =>null;
end;
--Generation of the csv, gp (gnuplot), png files
      csv_out (to_string (dir_out) & country'image (c) & "_forecast.csv");
      gp_out (to_string (dir_out) & country'image (c) & "_forecast");
      DECLARE
         gpcommand : string := "gnuplot " &  country'image (c) & "_forecast.gp";
         args        : argument_list_access := argument_string_to_list (gpcommand);
         exit_status : integer;
      BEGIN
         change_dir (to_string (dir_out));
         exit_status := spawn (program_name => args (args'first).ALL, args  => args (args'first + 1 .. args'last));
         free (args);
         change_dir ("..");
      END;
   END;
end xph_covid19;
