#!/usr/bin/env bash

rm db.sqlite && sqlite3 db.sqlite < sql/schema.sql && sqlite3 db.sqlite < sql/data.sql && sqlite3 db.sqlite < sql/horario_inserts.sql  