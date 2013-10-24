# See what the raw data looks like: allometry, neighborhoods,...
pp <- read.csv("long.csv")

# Static

# Ht vs. DBH by elevation
p1 <- ggplot(pp, aes(dbh, ht)) + geom_point(alpha = .4, size=1) +
    facet_wrap(~ elevcl)


# Dynamics

# growth vs. prior
#  dbh
p2 <- ggplot(pp, aes(priordbh, dbhgrowth)) + geom_point(alpha=.4, size=1) +
    facet_wrap(~ elevcl)

# ba
p3 <- ggplot(pp, aes(priorba, bagrowth)) + geom_point(alpha=.4, size=1) +
    facet_wrap(~ elevcl)

# ht
p4 <- ggplot(pp, aes(priorht, htgrowth)) + geom_point(alpha=.4, size=1) +
    facet_wrap(~ elevcl)

#
# HT growth vs DBH


