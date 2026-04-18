select g.nombre
   from jornada j
   join day d
     on j.day_id = d.day_id
   join gero g
     on j.gero_id = g.gero_id
   where
    j.day_id = 1
    and
    j.shift_id = 1;
