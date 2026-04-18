#!/usr/bin/env python3
"""
Generate SQL INSERT statements for the jornada table from horario.csv
"""
import csv

# Mapping of worker names from CSV to gero_id from data.sql
GERO_NAME_TO_ID = {
    'anon': 1,
    'anon': 2,
    'anon': 3,
    'anon': 4,
    'anon': 5,
    'anon': 6,
    'anon': 7,
    'anon': 8,
    'anon': 9,
    'anon': 10,  
    'anon': 11,
    'anon': 12
}

# Mapping of shift codes to shift_id
SHIFT_TO_TURNO_ID = {
    'M': 1,  # Mañana
    'T': 2   # Tarde
}

def generate_horario_inserts(csv_path='./horario.csv', output_path='./horario_inserts.sql'):
    """
    Read horario.csv and generate SQL INSERT statements
    """
    inserts = []

    with open(csv_path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)

        for row in reader:
            gero_name = row['GERO'].strip()

            # Skip empty rows
            if not gero_name:
                continue

            # Get the gero_id
            if gero_name not in GERO_NAME_TO_ID:
                print(f"Warning: Unknown worker name '{gero_name}' - skipping")
                continue

            gero_id = GERO_NAME_TO_ID[gero_name]

            # Process each day (columns 1-31)
            for day_num in range(1, 32):
                day_str = str(day_num)
                if day_str not in row:
                    continue

                shift = row[day_str].strip()

                # Skip if no shift assigned
                if not shift:
                    continue

                # Get the shift_id
                if shift not in SHIFT_TO_TURNO_ID:
                    print(f"Warning: Unknown shift '{shift}' for {gero_name} on day {day_num} - skipping")
                    continue

                shift_id = SHIFT_TO_TURNO_ID[shift]
                day_id = day_num  # day_id matches the day number

                # Create the insert tuple
                inserts.append(f"    ({day_id}, {shift_id}, {gero_id})")

    # Generate the SQL file
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("-- Auto-generated INSERT statements for jornada table\n")
        f.write("-- Generated from horario.csv\n\n")
        f.write("INSERT INTO jornada (\n")
        f.write("    day_id,\n")
        f.write("    shift_id,\n")
        f.write("    gero_id\n")
        f.write(")\nVALUES\n")
        f.write(",\n".join(inserts))
        f.write(";\n")

    print(f"Generated {len(inserts)} INSERT statements")
    print(f"Output written to {output_path}")

if __name__ == '__main__':
    generate_horario_inserts()
