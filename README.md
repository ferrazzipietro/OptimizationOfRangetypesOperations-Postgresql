# PosgreSQL-Sistem-Architecture-Project
Attempt to improve the estimation of the selectivity of "strictly left" and "overlaps" operators and cardinality of joins of type "range" in PostgreSQL by a theoretical point of view with easy implementation.
The idea is to use a Zero Inflated Negative Binomial model build on an approximation of the three-dimensional histograms of lower bounds and upper bounds. 
It has been implemented in R and then the parameters are given to type_analyze function of PostgreSQL that estimate the selectivity of "strictly left" and "overlaps" operators and cardinality of joins of type "range" by calculating some integrales under the curves.

# How To import Data

To import the 4 csv file containing each a different distribution of range
The file db_setup.py should be used:

python3 db_setup.py insert|verify DB_NAME USER(Most likekly postgres) PASSWORD PATH_WITH_ALL_CSV

e.g
python3 db_setup.py insert postgres postgres postgres ./RandomData

This screipt require psycopg2
to install it you must run the command : pip install psycopg2

If you have run into problems while trying to install psycopg2 so just be sure that:
- the path is set (try createdb or psql command) if it's not so run the following command -> PATH=/usr/local/pgsql/bin:$PATH
- and install sudo apt-get install libpq-dev
