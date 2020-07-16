-- Utilities to split a task over several processors
with ada.Strings.Unbounded,ada.Calendar,ada.Text_IO,Ada.Integer_Text_IO,ada.Strings.Unbounded.Text_IO,Ada.Float_Text_IO;
use ada.Strings.Unbounded,ada.Calendar,ada.Text_IO,Ada.Integer_Text_IO,ada.Strings.Unbounded.Text_IO,Ada.Float_Text_IO;
package chern.tasking is

cores:integer:=1;-- # of processor cores from -c 1 getopt
type core_boundaries is array (positive range <>, positive range <>) of positive;

      protected semaphore1 is
      procedure add;
            procedure reset;
      function get return Integer;
   private
green_light:integer:=0;
   end semaphore1;

     function split_x_cores (nx:integer) return core_boundaries;

   task type processing_task (x1,x2:integer;b:not null access procedure (x1,x2:in integer)) is
   end processing_task;
   type processing_task_acc is access processing_task;
   procedure splitando(n_to_split:in integer; b: not null access procedure (x1,x2:in integer);msg:in Unbounded_String);--n_to_split is the total number of elements (e.g. 1st array dimension) 12/7/18
   --msg is some printout message for the process (e.g. "Setting Epot in" ... xx cores)
   -------------------------------
end chern.tasking;
