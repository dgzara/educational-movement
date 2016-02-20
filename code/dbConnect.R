library(igraph)
library(RPostgreSQL)
library(openair)
library(xtable)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname= dbname, host="localhost",user=user,password=password )