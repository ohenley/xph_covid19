with Ada.Calendar; use Ada.Calendar;
-----------------------------------
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
-----------------------------------

package xph_covid19 is

   type country is (ABW, AFG, AGO, ALB, AN0, ARE, ARG, ARM, ATG, AUS, AUT, AZE, BEL, BEN, BFA, BGD, BGR, BHR, BHS, BIH, BLR, BLZ, BMU, BOL,
                    BRA, BRB, BRN, BTN, CAF, CAN, CHE, CHL, CHN, CIV, CMR, COD, COG, COL, CPV, CRI, CUB, CUW, CYM, CYP, CZE, DEU, DJI, DMA,
                    DNK, DOM, DZA, ECU, EGY, ESP, EST, ETH, FIN, FJI, FRA, FRO, GAB, GBR, GEO, GHA, GIB, GIN, GMB, GNB, GNQ, GRC, GRD, GRL, GTM,
                    GUM, GUY, HND, HRV, HTI, HUN, IDN, IMN, IND, IRL, IRN, IRQ, ISL, ISR, ITA, JAM, JOR, JPN, KAZ, KEN, KGZ, KHM, KNA, KOR,
                    KWT, LBN, LBR, LBY, LCA, LIE, LKA, LTU, LUX, LVA, MAR, MDA, MDG, MDV, MEX, MKD, MLI, MLT, MMR, MNE, MNG, MOZ, MRT, MUS,
                    MYS, NAM, NCL, NER, NGA, NIC, NLD, NOR, NPL, NZL, OMN, PAK, PAN, PER, PHL, PNG, POL, PRT, PRY, PSE, PYF, QAT, ROU, RUS,
                    RWA, SAU, SDN, SEN, SGP, SLV, SMR, SOM, SRB, SUR, SVK, SVN, SWE, SYC, SYR, TCD, TGO, THA, TLS, TTO, TUN, TUR, TZA, UGA,
                    UKR, URY, USA, UZB, VCT, VEN, VIR, VNM, XKX, ZAF, ZMB, ZWE, PRI, SLE, SSD, TJK, DPL);

   type country_data is record
      name : string (1..32);
      area : float; -- km2
      pop : float; -- population
      pd : float; -- pop. density, pop/area
   end record;

   type countries is array (country) of country_data;

   type country_entry is record
      date : ada.Calendar.Time;
      day_index : float := 0.0;
      cases : float := 0.0;

      cumulative_cases : float := 0.0;
      cumulative_cases_density : float := 0.0;
      infection_rate : float := 0.0;

      cumulative_cases_simulated : float := 0.0;
      cumulative_cases_density_simulated : float := 0.0;
      infection_rate_simulated : float := 0.0;
   end record;

   type unknown is (a1, b1, b2, k1, k2); -- k1, k2 are constants, a1, b1, b2 are exponents
   type unknowns is array (unknown'Range) of float;

   type unknowns_array is array (integer range<>) of float;
   type uarray_access is access all unknowns_array;
   package unknowns_vector is new Ada.Containers.Vectors(Element_Type => float, Index_Type => Natural);
   type uvec_access is access all unknowns_vector.Vector;

   type unknowns_range is array (unknown'Range, 1..2) of float;

   type model_parameters is record
      u : unknowns;
      min_rate, max_rate : float; -- max. and min. calc. rates, and the rate at the bend
   end record;

   package country_entries_vector is new Ada.Containers.Vectors(Element_Type => country_entry, Index_Type => Natural);
   type country_entries_array is array (integer range <>) of country_entry;
   type country_entries_array_access is access all country_entries_array;

   function "<" (L, R : country_entry) return Boolean;
   procedure sort_by_date is new Ada.Containers.Generic_Array_Sort (integer, country_entry, country_entries_array);

   -----------------------------------

   function determine_end_day_index (ce : country_entries_array; start_day_index : integer; end_day_index : integer) return integer;

   function sanitize_covid_data (ce : in out country_entries_array; c_data: country_data) return country_entries_array;


   function get_narrow_unknowns_range (model : model_parameters; zoom_factor : float) return unknowns_range;


   procedure build_search_set (steps : integer;
                               unknown_r : unknowns_range;
                               ua1 : in out uarray_access;
                               ub1 : in out uarray_access;
                               ub2 : in out uarray_access;
                               uk1 : in out uarray_access;
                               uk2 : in out uarray_access);

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
                             ssrates_by_density : out uarray_access);

   procedure characterize_best_model (model : in out model_parameters;
                                      a1s : uarray_access;
                                      b1s : uarray_access;
                                      b2s : uarray_access;
                                      k1s : uarray_access;
                                      k2s : uarray_access;
                                      ssrates : uarray_access;
                                      ssrates_by_density : uarray_access;
                                      minimize_by_density : boolean);

   function find_smallest_ssrate (ssrates : uarray_access;
                                  ssrates_by_density : uarray_access;
                                  minimize_by_density : Boolean;
                                  min_rate: in out float) return Integer;


   procedure compute_simulated_rate (c : country;
                           start_day_index : integer;
                           ce : in out country_entries_array;
                           model : model_parameters);

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
                   minimal_improvement_percentage : float);

   procedure compute_forecast (c : country;
                               ce : country_entries_array;
                               forecast_ce : in out country_entries_array;
                               model : in out model_parameters);

   function detect_bend (c_forcast_entries : country_entries_array; bend_percent : float) return Integer;

   function detect_first_case (ce : country_entries_array; forecast_ce : country_entries_array) return Integer;

end xph_covid19;
