
addition <- 2 + 4
multiplic <- 2 * 5

and <- addition + multiplic

#alph is a vector (only one dimension)
alph <- c("a", "b", "c", "d", "e", "f", "g")

#as.data.frame makes if formatted as a data frame; two dimensional
dat <- as.data.frame(cbind(alph, star))

#rev reversed the order that star was applied in
#cbind puts the two sequences together in the data frame (i think)
dat <- as.data.frame(cbind(alph, rev(star)))

#$ delineates a certain column in a data frame
dat$star <- as.numeric(dat$star)

str(dat) #this will tell you what format something actually is
