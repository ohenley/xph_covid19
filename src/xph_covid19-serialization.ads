with xph_covid19; use xph_covid19;

package xph_covid19.serialization is

   procedure show_credentials;

   procedure show_usage;

   procedure show_software_infos;

   procedure show_simulation_configuration (c: country; start_day_index : integer; end_day_index : integer; ce : country_entries_array);

   function get_country_data (filename: String; c : Country) return country_entries_array;

   procedure show_model_unknows (model : model_parameters);

   function make_data_dir_out (c : Country;
                               start_day_index : Integer;
                               end_day_index : Integer;
                               ma : country_entries_array;
                               minimize_by_density : Boolean) return String;

   procedure csv_out (fn : string;
                      c : Country;
                      start_day_index : Integer;
                      end_day_index : Integer;
                      ma : country_entries_array;
                      maf : country_entries_array;
                      minimize_by_density : Boolean;
                      model : model_parameters;
                      bend : integer);

   procedure gp_out (fn : string;
                     c : Country;
                     start_day_index : Integer;
                     end_day_index : Integer;
                     ma : country_entries_array;
                     maf : country_entries_array;
                     model : model_parameters;
                     bend : integer;
                     first_case : integer);

   function nice_float (tf : float) return string;

end xph_covid19.serialization;
