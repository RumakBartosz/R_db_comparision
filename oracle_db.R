#############################
# ORACLE DB SCRIPT
#############################

# Load RODBC package
library(RODBC)

# Create a connection to the database called "channel"
channel <- odbcConnect("DATABASE", uid="USERNAME", pwd="PASSWORD")

# Query the database and put the results into the data frame
# "dataframe"

dataframe <- sqlQuery(channel, "
 SELECT *
 FROM
 SCHEMA.DATATABLE")