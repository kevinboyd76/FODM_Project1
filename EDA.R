library(tidyverse)
library(cowplot)

################################################################################
################################     Polio     #################################
################################################################################
polio <- read.csv("the-number-of-reported-paralytic-polio-cases.csv")
head(polio)

# worldwide incidence per year
p1 <- ggplot(polio, aes(x=Year, y=Total..reported..polio.cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Worldwide Polio") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p1

# USA
polio %>% 
  filter(Code == "USA") %>%
  ggplot(aes(x=Year, y=Total..reported..polio.cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Polio in the USA") +
  ylab("Polio Incidence") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
###############################     Measles     ################################
################################################################################
measles <- read.csv("reported-cases-of-measles.csv")
head(measles)

# worldwide incidence per year
p2 <- ggplot(measles, aes(x=Year, y=Indicator.Measles...number.of.reported.cases)) +
  geom_bar(stat = "identity", fill = "orange2") +
  ggtitle("Worldwide Measles") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p2

# USA
measles %>% 
  filter(Code == "USA") %>%
  ggplot(aes(x=Year, y=Indicator.Measles...number.of.reported.cases)) +
  geom_bar(stat = "identity", fill = "orange2") +
  ggtitle("Measles in the USA") +
  ylab("Measles Incidence") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



################################################################################
################################     Hep B     #################################
################################################################################
hepb <- read.csv("hepatitis-b-incidence-sdgs.csv")
head(hepb)

# worldwide incidence per year
p3 <- ggplot(hepb, aes(x=Year, y=Incidence...Acute.hepatitis.B...Sex..Both...Age..All.Ages..Rate.)) +
  geom_bar(stat = "identity", fill = "seagreen") +
  ggtitle("Worldwide Hepetitis B") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p3

# USA
hepb %>% 
  filter(Code == "USA") %>%
  ggplot(aes(x=Year, y=Incidence...Acute.hepatitis.B...Sex..Both...Age..All.Ages..Rate.)) +
  geom_bar(stat = "identity", fill = "seagreen") +
  ggtitle("Hepatitis B in the USA") +
  ylab("Hepatitis B Incidence") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
############################     Tuberculosis     ##############################
################################################################################
tuberc <- read.csv("incidence-of-tuberculosis-sdgs.csv")
head(tuberc)

# worldwide incidence per year
p4 <- ggplot(tuberc, aes(x=Year, y=Incidence.of.tuberculosis..per.100.000.people.)) +
  geom_bar(stat = "identity", fill = "coral2") +
  ggtitle("Worldwide Tuberculosis") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
p4

# USA
tuberc %>% 
  filter(Code == "USA") %>%
  ggplot(aes(x=Year, y=Incidence.of.tuberculosis..per.100.000.people.)) +
  geom_bar(stat = "identity", fill = "coral2") +
  ggtitle("Tuberculosis in the USA") +
  ylab("Tuberculosis Incidence per 100000") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  


################################################################################
###########################     Plot all together    ###########################
################################################################################
plot_grid(p1, p2, p3, p4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)




################################################################################
############################     Merge Data Sets    ############################
################################################################################
names(tuberc)[1] <- "Country"
names(measles)[1] <- "Country"

LC <- unique(lifeExpectancy$Country)
PC <- unique(hepb$Entity)

LP_PC <- merge(lifeExpectancy, polio, by = "Country")
LP_PC_Country <- unique(LP_PC$Country)
lifeExpectancy

unique(lifeExpectancy[-match(LP_PC_Country, lifeExpectancy$Country),1])
unique(polio[-match(LP_PC_Country, polio$Country),1])

countrycode(LC, origin = 'country.name', destination = 'iso3c')
countrycode(PC, origin = 'country.name', destination = 'iso3c')

hepb$Code1 <- hepb$Code

# combine files
CombinedData <- merge(lifeExpectancy, hepb, by = c("Code1", "Year"))

# write file
write.table(CombinedData, file = "Hepb2.txt", quote=F,row.names = F,sep="\t")

################################################################################
############################     Merge Data Sets    ############################
################################################################################

library(countrycode)

# load the data and rename column
polio <- read.csv("the-number-of-reported-paralytic-polio-cases.csv")
names(polio)[1] <- "Country"
lifeExpectancy <- read.csv("Life Expectancy Data.csv")

# get country codes
lifeExpectancy$Code1 <- countrycode(lifeExpectancy$Country, origin = 'country.name', destination = 'iso3c')
polio$Code1 <- countrycode(polio$Country, origin = 'country.name', destination = 'iso3c')

# combine files
CombinedData <- merge(lifeExpectancy, polio, by = c("Code1", "Year"))

# write file
write.table(CombinedData, file = "Polio2.csv", quote=F,row.names = F,sep=",")

################################################################################
############################     Merge Data Sets    ############################
################################################################################

names(hepb)[1] <- "Country"
names(tuberc)[1] <- "Country"
names(measles)[1] <- "Country"
hepb$Code1 <- hepb$Code
tuberc$Code1 <- tuberc$Code
measles$Code1 <- measles$Code

tuberc$Code1 <- countrycode(polio$Country, origin = 'country.name', destination = 'iso3c')
measles$Code1 <- countrycode(polio$Country, origin = 'country.name', destination = 'iso3c')

# combine files
CombinedData <- merge(lifeExpectancy, hepb, by = c("Code1", "Year"))
CombinedData <- merge(lifeExpectancy, tuberc, by = c("Code1", "Year"))
CombinedData <- merge(lifeExpectancy, measles, by = c("Code1", "Year"))

# write file
write.table(CombinedData, file = "Measles2.csv", quote=F,row.names = F,sep=",")

################################################################################
############################     Merge Data Sets    ############################
################################################################################

