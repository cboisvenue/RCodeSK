#First try at connecting to CBM output mdb and querying from it

# July 7 2015
# Author: Byron Smiley
#-----------------------------------------------------------------------------

#packages:

library(RODBC)

odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                  DBQ=H:/saskatchewan/cbm/pafma_june_25_2015_1 Default Simulation Assumption.mdb")