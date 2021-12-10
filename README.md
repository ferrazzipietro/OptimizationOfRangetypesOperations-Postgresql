# PosgreSQL-Sistem-Architecture-Project
Attempt to improve the estimation of the selectivity of "strictly left" and "overlaps" operators and cardinality of joins of type "range" in PostgreSQL by a theoretical point of view with easy implementation.
The idea is to use a Zero Inflated Negative Binomial model build on an approximation of the three-dimensional histograms of lower bounds and upper bounds. 
It has been implemented in R and then the parameters are given to type_analyze function of PostgreSQL that estimate the selectivity of "strictly left" and "overlaps" operators and cardinality of joins of type "range" by calculating some integrales under the curves.
