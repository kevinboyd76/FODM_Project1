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
  geom_bar(stat = "identity") +
  ggtitle("Tuberculosis in the USA") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  


################################################################################
###########################     Plot all together    ###########################
################################################################################
plot_grid(p1, p2, p3, p4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)


