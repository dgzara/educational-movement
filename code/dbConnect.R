library(igraph)
library(RPostgreSQL)
library(openair)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname= dbname, host="localhost",user=user,password=password )