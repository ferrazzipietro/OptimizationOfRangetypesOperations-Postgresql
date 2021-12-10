import psycopg2
import csv
import os
from sys import argv

def extract_data_from_csv(dir):
    files = {i: list() for i in os.listdir(dir) if ".csv" in i}
    for file in files:
        with open(dir+'/'+file, newline='') as csvfile:
            spamreader = csv.reader(csvfile, delimiter=',', quotechar=',')
            for row in spamreader:
                if 'LowerBounds' not in row:
                    files[file].append(row)
    return files

def insert_into_db(conn,cur,data):
    for table in data:
        tbl = table[:-4]
        cur.execute(f"CREATE TABLE {tbl}(rng int4range);")
        for rng in data[table]:
            lower = int(round(float(rng[0])))
            upper = int(round(float(rng[1])))
            cur.execute(f"INSERT INTO {tbl} VALUES(int4range('[{lower},{upper}]'))")
    conn.commit()

def verify_insert(conn,cur,data):
    for i in data:
        cur.execute(f"SELECT count(*) from {i[:-4]}")
        size = cur.fetchone()
        print(f"{i} |{size}")

mode = argv[1]
db = argv[2]
us = argv[3]
pwd = argv[4]
path = argv[5]

data = extract_data_from_csv(path)
conn = psycopg2.connect(
    host="localhost",
    database=db,
    user=us,
    password=pwd)

 # create a cursor
cur = conn.cursor()

if(mode == 'insert'):
    insert_into_db(conn,cur,data)
if(mode ==  'verify' or mode == 'insert'):
    verify_insert(conn,cur,data)

print("---END---")
# close the communication with the PostgreSQL
cur.close()
conn.close()
