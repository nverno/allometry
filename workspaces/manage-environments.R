### Manage stored data/workspaces for ABBA and Douglas Fir

### Environment to store model fits for all species/datasets
# currentfits.Rda stores model fits for both species, so names must be
#  identifiable
fit.env <- new.env()
load(file = "currentfits.Rda", envir = fit.env)
ls(all.names=TRUE, envir = fit.env)

#save(file = "currentfits.Rda", list=ls(all.names=TRUE,envir=tmp.env),envir=tmp.env)

### Doug Fir Environment
# dfworkspace.Rda stores targets, neighbors, neighbor matrices, etc... for
#  Doug Fir
df.env <- new.env()
load(file = "dfworkspace.Rda", envir = df.env)
ls(all.names=TRUE, envir = df.env)

# choose objects to save
# tosave <- ls()[grep("distances|bas|species", ls())] ## saves neighbor matrices
# save(file = "dfworkspace.Rda",
#     list = tosave, envir = .GlobalEnv)


### ABBA environment
# abbaworkspace.Rda stores targets, neighbors, neighbor matrices, fits, etc... for
#  ABBA
abba.env <- new.env()
load(file = "abbaworkspace.Rda", envir = abba.env)
ls(all.names=TRUE, envir = abba.env)

# choose objects to save
# tosave <- ls()[grep("distances|bas|species", ls())] ## saves neighbor matrices
# save(file = "abbaworkspace.Rda",
#     list = tosave, envir = .GlobalEnv)
