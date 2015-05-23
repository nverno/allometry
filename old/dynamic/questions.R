## Why are there rows in wide dataframe that have no dbh or ht?
## Probably info from the other random yrs -- still have to check
tst <- subset(ppwide, is.na(dbh86) & is.na(dbh87) & is.na(dbh98) & is.na(dbh10) &
            is.na(ht86) & is.na(ht87) & is.na(ht98) & is.na(ht10))

## All trees missing info died in 2010 or 1998
nrow(subset(ppwide, yrmort == 1998 & is.na(dbh86) & is.na(dbh87) & is.na(dbh98) & is.na(dbh10) &
            is.na(ht86) & is.na(ht87) & is.na(ht98) & is.na(ht10)))
