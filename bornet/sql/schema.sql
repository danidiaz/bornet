CREATE TABLE gero(
        gero_id INTEGER PRIMARY KEY, 
        nombre  TEXT,
        group_id INTEGER NOT NULL DEFAULT 0
    );

create table day(
        day_id INTEGER PRIMARY KEY,
        orden INTEGER,
        nombre TEXT
);

create table shift(
        shift_id INTEGER PRIMARY KEY, 
        nombre TEXT
    );

create table role(
    role_id INTEGER PRIMARY KEY, 
    shift_id INTEGER,
    nombre TEXT,
    FOREIGN KEY(shift_id) REFERENCES shift(shift_id)
);

-- | A set of assignations of work days and shifts to geros.
create table horario  (
    horario_id INTEGER PRIMARY KEY,
    description TEXT NOT NULL,
    -- name of the day of the week at which the horario starts
    weekday_start TEXT NOT NULL,
    -- epoch time in seconds
    created_at INTEGER NOT NULL
);

-- | Jornada laboral de un gero en un día y turno concretos, para cierto horario.
create table jornada(
    horario_id INTEGER,
    day_id INTEGER,
    gero_id INTEGER,
    shift_id INTEGER,
    FOREIGN KEY(horario_id) REFERENCES horario(horario_id)
    FOREIGN KEY(day_id) REFERENCES day(day_id)
    FOREIGN KEY(gero_id) REFERENCES gero(gero_id)
    FOREIGN KEY(shift_id) REFERENCES shift(shift_id)
    PRIMARY KEY (horario_id, day_id, gero_id)
);

-- A set of choices assigning roles to geros for certain days and shifts withing a given horario. 
-- A more detailed version of "horario", because now the roles are specified.
-- A more consistent name for this could be perhaps "horario_detallado".
create table experiment(
    experiment_id INTEGER PRIMARY KEY,
    description TEXT NOT NULL,
    horario_id INTEGER NOT NULL,
    -- epoch time in seconds
    created_at INTEGER NOT NULL,
    -- Are we allowed to modify this experiment? When was it locked?
    -- epoch time in seconds
    locked_at INTEGER,
    -- Has this experiment selected as important?
    selected_at INTEGER,
    FOREIGN KEY(horario_id) REFERENCES horario(horario_id)
);

create table jornada_detallada(
    experiment_id INTEGER,
    day_id INTEGER,
    gero_id INTEGER,
    role_id INTEGER,
    FOREIGN KEY(experiment_id) REFERENCES experiment(experiment_id)
    FOREIGN KEY(day_id) REFERENCES day(day_id)
    FOREIGN KEY(gero_id) REFERENCES gero(gero_id)
    FOREIGN KEY(role_id) REFERENCES role(role_id)
    PRIMARY KEY (experiment_id, day_id, gero_id)
);

