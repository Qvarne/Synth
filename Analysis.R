# Load necessary packages
library(eurostat)
library(tidyverse)
# library(readxl)
library(Synth)
library(plm)
library(data.table)
# library(rgenoud)
# library(MSCMT)
# library(gsynth)
# library(MatchIt)
# library(fastDummies)
library(sjmisc)
library(pwt9)
# library(gghighlight)
# library(lmtest)
# library(sandwich)
# library(robustbase)
# library(clubSandwich)
# library(ggpubr)
# library(xtable)
# library(stargazer)
# library(Zelig)
# library(cem)
# library(mice)
# library(optmatch)
# library(influence.ME)
library(bdl)
# library(devtools)
library(augsynth)

# Continue here
# test <- data3 %>% mutate(treat = ifelse(geo == "DEA12", 2011,
#                                   ifelse(geo == "PL811", 2011, 
#                                     ifelse(geo == "CZ053", 2012,
#                                       ifelse(geo == "PL911", 2012,
#                                         ifelse(geo == "DE600", 2013,
#                                           ifelse(geo == "PL711", 2013,
#                                             ifelse(geo == "NL412", 2013,
#                                               ifelse(geo == "ES300", 2013,
#                                                 ifelse(geo == "UKI21", 2017)
#                                           
#                                                
#                                                    geo == "HU101" | geo == "FI1C4" | geo == "DE212" | geo == "DE254" | ///
#                                                   geo == "PL415" | geo == "CZ010" | geo == "LV006" | geo == "NL412" | geo == "AT130"  
#                                                 replace treated_new = 0 if treated_new == .

data3 %>% filter(geo == "PL811") %>% ggplot(aes(x = time)) + geom_line(aes(y = road_freight.growth))
data3 %>% filter(nuts.2 == "PL81") %>% select(id) %>% print(levels(factor(id)), n = 25)

data3 %>% filter(time >= 2000) %>%
  group_by(country, time) %>% 
  summarize(sum(road_freight.growth, na.rm = T)) %>%
  print(n = 653)

# Preparing data set for synthetic control
data4 <- data3 %>% filter(time >= 2005 & time < 2017) %>% filter(country == "PL") %>% 
  distinct(id, time, .keep_all= TRUE) %>%
  # mutate(treatment = ifelse(time >= 2009, 1, 0)) %>%
  group_by(id) %>%
  # fill(population, .direction = "updown") %>%
  # fill(coast, .direction = "updown") %>%
  # fill(metro, .direction = "updown") %>%
  # fill(urban, .direction = "updown") %>%
  # fill(port, .direction = "updown") %>%
  # fill(dependency.ratio_new, .direction = "updown") %>%
  # fill(gva.TOTAL.growth, .direction = "updown") %>%
  # fill(gva.TOTAL_capita, .direction = "updown") %>%
  # fill(gva.TOTAL, .direction = "updown") %>%
  filter(!is.na(id)) %>%
  # filter(!is.na(employment.TOTAL)) %>%
  # filter(!is.na(employment.A)) %>%
  # filter(!is.na(employment.B.D_E)) %>%
  # filter(!is.na(employment.C)) %>%
  # filter(!is.na(employment.F)) %>%
  # filter(!is.na(employment.G_J)) %>%
  # filter(!is.na(employment.J)) %>%
  # filter(!is.na(employment.K_N)) %>%
  # filter(!is.na(employment.L)) %>%
  # filter(!is.na(employment.M_N)) %>%
  # filter(!is.na(employment.O_U)) %>%
  # filter(!is.na(employment.R_U)) %>%
  # filter(!is.na(employment.TOTAL_capita)) %>%
  # filter(!is.na(employment.G_J_capita)) %>%
  # filter(!is.na(road_freight)) %>%
  filter(!is.na(nuts.2)) %>%
  filter(!is.na(time)) %>%
  # filter(!is.na(population)) %>%
  # filter(!is.na(gva.TOTAL_capita)) %>%
  # filter(!is.na(dependency.ratio_new)) %>%
  # filter(!is.na(gva.TOTAL.growth)) %>%
  filter(!is.na(road_freight.growth)) %>%
  # filter(!is.na(coast)) %>%
  # filter(!is.na(port)) %>%
  # filter(!is.na(metro)) %>%
  # filter(!is.na(urban)) %>%
  # filter(!is.na(gva.TOTAL)) %>%
  as.data.frame() %>% 
  select(c(
           "id",
           "time",
           "geo",
           # "population",
           # "coast",
           # "port",
           # "dependency.ratio_new",
           # "metro",
           # "gva.TOTAL_capita",
           # "gva.TOTAL.growth",
           # "urban",
           "road_freight.growth"
           )) %>%
  distinct(id, time, .keep_all= TRUE) %>% select(id, time, everything())
data5 <- make.pbalanced(data4, balance.type = "shared.individuals")
summary(data5)
length(levels(factor(data5$id)))


# data5 %>% filter(id == "403")
data5 %>% filter(geo == "PL811")
# loop <- unique(data5$id) 
# loop <- loop[!loop %in%  c(90)]
# loop <- loop[loop >= 174]
# c <- b[!b %in%  c(402:416)]
# c <- b[!b %in%  c(578:584)]

# dataprep <- list()
# road_freight.growth <- list()
# 
# for (x in loop) {
#   
b <- unique(data5$id) 
  
c <- b[!b %in%  c(1113:1114)]

# Run synthetic control estimation and plot results
dataprep.out <-
  dataprep(
    foo = data5,
    predictors = c(
      # "population",
      #   "coast",
      #   "urban",
      #   "port",
      #   "dependency.ratio_new",
      #   "metro",
        # "gva.TOTAL_capita"
      #   "gva.TOTAL.growth"
        # "road_freight.growth",
        # "gva.TOTAL"
                   ),
    predictors.op = "mean",
    dependent = "road_freight.growth",
    unit.variable = c("id"),
    time.variable = c("time"),
    special.predictors = list(
    #   # list("road_freight.growth", 1999, "mean"),
      # list("road_freight.growth", 2000, "mean"),
      list("road_freight.growth", 2001, "mean"),
      list("road_freight.growth", 2002, "mean"),
      list("road_freight.growth", 2003, "mean"),
      list("road_freight.growth", 2004, "mean"),
      list("road_freight.growth", 2005, "mean"),
      list("road_freight.growth", 2006, "mean"),
      list("road_freight.growth", 2007, "mean"),
      list("road_freight.growth", 2008, "mean"),
      list("road_freight.growth", 2009, "mean"),
      list("road_freight.growth", 2010, "mean")
    ),
    treatment.identifier = 1112,
    controls.identifier = c,
    time.predictors.prior = c(2001:2010),
    time.optimize.ssr = c(2001:2010),
    unit.names.variable = "geo",
    time.plot = 2001:2017
  )
# dataprep[[x]] <- dataprep.out

synth.out <- synth(dataprep.out)#, verbose = TRUE, optimxmethod = "All")
# synth[[x]] <- synth.out
# }

synth.tab <- synth.tab(synth.res = synth.out, dataprep.res = dataprep.out)
synth.tab$tab.w %>% as_tibble() %>% filter(w.weights > 0) %>% arrange(desc(w.weights))
synth.tab$tab.v

ggplot(data = tibble(dataprep.out$Y1plot, dataprep.out$Y0plot %*% synth.out$solution.w) %>% 
         rename(actual = 1) %>% rename(synthetic = 2) %>% gather() %>% group_by(key)  %>% 
         add_column(c(2001:2017,2001:2017)) %>% rename(year = 3), 
       aes(x = year, y = value, linetype = key)) +
  geom_line() + theme_classic() + geom_vline(xintercept = 2011, linetype = "longdash") +
  xlab("") + ylab("Employment (thousand people)") + theme(legend.position = c(0.9,0.3), legend.title = element_blank())
ä
