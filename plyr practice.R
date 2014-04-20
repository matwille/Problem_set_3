setwd("~/R/Plyr practice")

rm(list = ls())

library(plyr)

library(xtable)

library(lattice)

gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
gDat <- read.delim(file = gdURL)
## str(gDat) here when working interactively
yearMin <- min(gDat$year)
jFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - yearMin), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}
## jFun(subset(gDat, country == 'India')) to see what it does
jCoefs <- ddply(gDat, ~country, jFun)



set.seed(916)
foo <- jCoefs[sample(nrow(jCoefs), size = 15), ]
foo <- xtable(foo)


print(foo, type = "html", include.rownames = FALSE)


jCoefs <- ddply(gDat, ~country, jFun)


jCoefs <- ddply(gDat, ~country + continent, jFun)
str(jCoefs)

tail(jCoefs)


set.seed(916)
foo <- jCoefs[sample(nrow(jCoefs), size = 15), ]
foo <- arrange(foo, intercept)
## foo <- foo[order(foo$intercept), ] # an uglier non-plyr way
foo <- xtable(foo)
print(foo, type = "latex", include.rownames = FALSE)




BREAK FOR THE Q&A SECTION




yearMin <- min(gDat$year)

jCoefs <- ddply(gDat, ~country, jFun)

head(jCoefs)



jFun <- function(x) {
  estCoefs <- coef(lm(lifeExp ~ I(year - yearMin), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}


jCoefs <- ddply(gDat, ~country, jFun)
head(jCoefs)


jFunTwoArgs <- function(x, cvShift = 0) {
  estCoefs <- coef(lm(lifeExp ~ I(year - cvShift), x))
  names(estCoefs) <- c("intercept", "slope")
  return(estCoefs)
}




jCoefsSilly <- ddply(gDat, ~country, jFunTwoArgs)
head(jCoefsSilly)


jCoefsSane <- ddply(gDat, ~country, jFunTwoArgs, cvShift = 1952)
head(jCoefsSane)



jCoefsBest <- ddply(gDat, ~country, jFunTwoArgs, cvShift = min(gDat$year))
head(jCoefsBest)








