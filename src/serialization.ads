with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with xph_covid19; use xph_covid19;

package Serialization is

   procedure show_credentials;
   procedure show_usage;

   procedure show_software_infos;

   function get_country_data (filename: String; c : Country) return country_entries_array;

   procedure show_model_unknows (model : model_parameters);

   function make_data_dir_out (c : Country;
                               start_day_index : Integer;
                               end_day_index : Integer;
                               ma : country_entries_array;
                               minimize_by_density : Boolean) return String;

   procedure csv_out (fn : in string;
                      c : Country;
                      start_day_index : Integer;
                      end_day_index : Integer;
                      ma : country_entries_array;
                      maf : country_entries_array;
                      --outf : in out file_type;
                      minimize_by_density : Boolean;
                      model_set1 : model_parameters;
                      bend : integer;
                      area : float);

   procedure gp_out (fn : in string;
                     c : Country;
                     start_day_index : Integer;
                     end_day_index : Integer;
                     ma : country_entries_array;
                     maf : country_entries_array;
                     model_set1 : model_parameters;
                     bend : integer;
                     area : float;
                     first_case : integer);

   function Nicef (Tf : IN Float) return String;

end Serialization;
