-- list shifts for a day
SELECT
      h.description AS horario,
      d.nombre AS dia,
      g.nombre AS gero,
      s.nombre AS shift,
      s.shift_id as shift_id
  FROM jornada j
  JOIN horario h ON j.horario_id = h.horario_id
  JOIN day d ON j.day_id = d.day_id
  JOIN gero g ON j.gero_id = g.gero_id
  JOIN shift s ON j.shift_id = s.shift_id
  WHERE h.description = 'mayo'
    AND d.orden = 20;


-- change some shifts for a day
UPDATE jornada
  SET shift_id = 2
  WHERE horario_id = (SELECT horario_id FROM horario WHERE description = 'septiembre')
    AND day_id = (SELECT day_id FROM day WHERE orden = 29)
    AND gero_id IN (SELECT gero_id FROM gero WHERE nombre IN ('', '', ''));

