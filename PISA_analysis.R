####################################################
#Setting Working Directory
setwd("C:/Users/Steffen/Arbeit/190528_ELATA Workshop/EALTA-2019-Workshop")

####################################################
# Import
####################################################
# Functions
library(psych)
library(ggplot2)
library(TAM)

# Data
load("PISA_2015_GERMANY.Rda")

####################################################
# Prepare Data
####################################################
# Funtion to retrieve all variable names in the data table
names(PISA_2015_GERMANY)

PISA_2015_GERMANY <- PISA_2015_GERMANY %>%
  rename(gender=ST004D01T)

# Contruct data table with only the items from the reading assessment
resp <- PISA_2015_GERMANY[c("CR055Q01S","DR055Q02C","DR055Q03C","DR055Q05C","CR104Q01S","CR104Q02S", 
                            "CR104Q05S","CR111Q01S","DR111Q02BC","DR111Q06C","CR227Q01S","CR227Q02S",
                            "DR227Q03C","DR227Q06C")]
resp[is.na(resp)] <- 0

resp_d <- resp
resp_d[resp_d==2] <- 1

####################################################
# Explore Data
####################################################
describe(resp)

table(PISA_2015_GERMANY$HISCED, useNA = "always")

ggplot(PISA_2015_GERMANY) +
  geom_histogram(aes(x=EAPREAD))

####################################################
# Unidimensional Model
####################################################
mod <- tam(resp)


####################################################
# Revision of Item Characteristics
####################################################

# Point-biserial correlatio
wle <- tam.wle(mod)
tam.ctt(resp, wlescore=wle$theta)

#ICCS
plot(mod)

# Gender DIF
mod_dif_d <- tam.mml.mfr(resp=resp_d, facets=PISA_2015_GERMANY, formulaA=~item+gender+item*gender)
summary(mod_dif_d)



####################################################
# Revision of Person Characteristics
####################################################
