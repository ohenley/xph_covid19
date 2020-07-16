--Tasking utilities to split job over several processors
with ada.exceptions;
use ada.exceptions;
package body chern.tasking is

   protected body semaphore1 is
      procedure add is
      begin
         green_light := green_light + 1;
      end add;
      procedure reset is
      begin
	 green_light:=0;
      end reset;
      function get return Integer is
      begin
	 return green_light;
      end get;
   end semaphore1;

        function split_x_cores (nx:integer) return core_boundaries is
     tcores:core_boundaries (1..integer(cores),1..2);
     xt:integer:=0;
     xd:integer:=nx/integer(cores);
     begin
      for i in tcores'range(1) loop
         tcores(i,1):=xt+1;
         if i=tcores'last(1) then
            tcores(i,2):=nx;
            else
            tcores(i,2):=xt+xd;
         end if;
         put ("Core boundary ");  put(integer(i)); put(' '); put(tcores(i,1)); put(' '); put(tcores(i,2)); new_line;
         xt:=tcores(i,2);
      end loop;
      return tcores;
     end split_x_cores;


      task body processing_task is
      ex1:Exception_Occurrence;
   begin
      b(x1,x2);
     semaphore1.add;
exception when  ex1:others =>
put_line(exception_name(ex1));
put_line(exception_message(ex1));
semaphore1.add;
   end processing_task;


      procedure splitando(n_to_split:in integer; b:not null access procedure (x1,x2:in integer);msg:in Unbounded_String) is
      core_boundary: core_boundaries(1..integer(cores),1..2);-- of integer;-- x1 and x2 for proc. cores
      fr:array (core_boundary'range(1)) of processing_task_acc;
      tim1:time;
   begin
      core_boundary:=split_x_cores(n_to_split);
tim1:=clock;
semaphore1.reset;
    put (msg & " ");put (integer(cores)); put_line(" cores...");
      for i in fr'range loop
         fr(i):=new processing_task (core_boundary(i,1),core_boundary(i,2),b);
      end loop;
      while semaphore1.get < cores loop
      delay 5.0;
   end loop;
put("Elapsed time: ");   put(float(duration(clock-tim1)),aft=>3,exp=>0); put_line(" s.");
   end splitando;

end chern.tasking;
