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

# 
# Load data from Eurostat (NUTS 3)
data <- get_eurostat(id = "demo_r_d3dens", time_format = "num") %>%
  rename(population.density = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gdp", time_format = "num", filters = list(unit = "MIO_EUR")), by = c("geo","time")) %>%
  rename(gdp.MIO_EUR = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gdp", time_format = "num", filters = list(unit = "EUR_HAB")), by = c("geo","time")) %>%
  rename(gdp.EUR_HAB = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gdp", time_format = "num", filters = list(unit = "PPS_HAB")), by = c("geo","time")) %>%
  rename(gdp.PPS_HAB = values) %>%
  left_join(get_eurostat(id = "road_go_na_rl3g", time_format = "num", filters = list(unit = "THS_T", nst07 = "TOTAL")), by = c("geo","time")) %>% 
  rename(road_freight.load = values) %>%
  select(-nst07) %>%
  left_join(get_eurostat(id = "road_go_na_ru3g", time_format = "num", filters = list(unit = "THS_T", nst07 = "TOTAL")), by = c("geo","time")) %>% 
  rename(road_freight.unload = values) %>%
  select(-nst07) %>%
  left_join(get_eurostat(id = "road_go_na7rl3g", time_format = "num", filters = list(unit = "THS_T", nstr24 = "25")), by = c("geo","time")) %>% 
  rename(road_freight.load.old = values) %>%
  select(-nstr24) %>% mutate(road_freight.load = ifelse(is.na(road_freight.load),road_freight.load.old,road_freight.load)) %>% 
  select(-road_freight.load.old) %>%
  left_join(get_eurostat(id = "road_go_na7ru3g", time_format = "num", filters = list(unit = "THS_T", nstr24 = "25")), by = c("geo","time")) %>% 
  rename(road_freight.unload.old = values) %>% 
  select(-nstr24) %>% mutate(road_freight.unload = ifelse(is.na(road_freight.unload),road_freight.unload.old,road_freight.unload)) %>% 
  select(-road_freight.unload.old) %>% mutate(road_freight = road_freight.load + road_freight.unload, na.rm = TRUE) %>%
  left_join(get_eurostat(id = "nama_10r_3popgdp", time_format = "num", filters = list(unit = "THS")), by = c("geo","time")) %>% 
  rename(population = values) %>%
  left_join(get_eurostat(id = "demo_r_d3area", time_format = "num", filters = list(unit = "KM2", landuse = "TOTAL")), by = c("geo","time")) %>%
  rename(area = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "A", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.A = values) %>% 
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "B-E", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.B_E = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "C", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.C = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "F", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.F = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "G-I", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.G_I = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "G-J", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.G_J = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "J", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.J = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "K", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.K = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "K-N", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.K_N = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "L", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.L = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "M_N", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.M_N = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "O-Q", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.O_Q = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "O-U", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.O_U = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "R-U", wstatus = "EMP")), 
            by = c("geo","time")) %>%
  rename(employment.R_U = values) %>%
  left_join(get_eurostat(id = "nama_10r_3empers", time_format = "num", filters = list(unit = "THS", nace_r2 = "TOTAL", wstatus = "EMP")), 
            by = c("geo","time")) %>% 
  rename(employment.TOTAL = values) %>% 
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "A")), by = c("geo","time")) %>%
  rename(gva.A = values) %>%  
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "B-E")), by = c("geo","time")) %>%
  rename(gva.B_E = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "C")), by = c("geo","time")) %>%
  rename(gva.C = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "F")), by = c("geo","time")) %>%
  rename(gva.F = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "G-I")), by = c("geo","time")) %>%
  rename(gva.G_I = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "G-J")), by = c("geo","time")) %>%
  rename(gva.G_J = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "J")), by = c("geo","time")) %>%
  rename(gva.J = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "K")), by = c("geo","time")) %>%
  rename(gva.K = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "K-N")), by = c("geo","time")) %>%
  rename(gva.K_N = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "L")), by = c("geo","time")) %>%
  rename(gva.L = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "M_N")), by = c("geo","time")) %>%
  rename(gva.M_N = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "O-Q")), by = c("geo","time")) %>%
  rename(gva.O_Q = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "O-U")), by = c("geo","time")) %>%
  rename(gva.O_U = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "R-U")), by = c("geo","time")) %>%
  rename(gva.R_U = values) %>%
  left_join(get_eurostat(id = "nama_10r_3gva", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "TOTAL")), by = c("geo","time")) %>%
  rename(gva.TOTAL = values) %>% mutate(gva.B_E_F = gva.B_E + gva.F, na.rm = TRUE) %>% mutate(gva.B_E_F_share = gva.B_E_F/gva.TOTAL) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "B-S_X_K642")), 
            by = c("geo","time")) %>% rename(business.B_S_X_K642  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "B-E")), 
          by = c("geo","time")) %>% rename(business.B_E  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "F")), 
            by = c("geo","time")) %>% rename(business.F  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "G")), 
            by = c("geo","time")) %>% rename(business.G  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "H")), 
            by = c("geo","time")) %>% rename(business.H  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "M_N")), 
            by = c("geo","time")) %>% rename(business.M_N  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "I")), 
            by = c("geo","time")) %>% rename(business.I  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "J")), 
            by = c("geo","time")) %>% rename(business.J  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "K_L_X_K642")), 
            by = c("geo","time")) %>% rename(business.K_L_X_K642  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "P_Q")), 
            by = c("geo","time")) %>% rename(business.P_Q  = values) %>%
  left_join(get_eurostat(id = "bd_hgnace2_r3", time_format = "num", filters = list(indic_sb = "V11910", nace_r2 = "R_S")), 
            by = c("geo","time")) %>% rename(business.R_S  = values) %>%
  left_join(get_eurostat(id = "ipr_ta_reg", time_format = "num", filters = list(unit = "NR")), 
            by = c("geo","time")) %>% rename(trade.mark  = values) %>%
  left_join(get_eurostat(id = "demo_r_pjanind3", time_format = "num", filters = list(indic_de = "DEPRATIO1", unit = "PC")), 
            by = c("geo","time")) %>% rename(dependency.ratio  = values) %>% select(-indic_de) %>%
  left_join(get_eurostat(id = "demo_r_pjanaggr3", time_format = "num", filters = list(age = "TOTAL", sex = "T", unit = "NR")),
            by = c("geo","time")) %>% rename(demo_r_pjanaggr3.TOTAL  = values) %>% select(-c("age","sex")) %>%
  left_join(get_eurostat(id = "demo_r_pjanaggr3", time_format = "num", filters = list(age = "Y15-64", sex = "T", unit = "NR")),
            by = c("geo","time")) %>% rename(demo_r_pjanaggr3.Y15_64  = values) %>% select(-c("age","sex"))                                                                                         
  
# Drop unit identifiers
data <- data %>% mutate(country = substr(geo,1,2)) %>% mutate(nuts.2 = substr(geo,1,4)) %>% 
    select(-c(grep("unit", colnames(data)), grep("wstatus", colnames(data)), 
           grep("nace_r2", colnames(data)), grep("currency", colnames(data)), grep("indic_sb", colnames(data)))) %>% select(-"landuse")

#Get additional data for Poland
added9 <- get_panel_data(unitId = c("011212000000","011212100000","011212200000","011212300000","011212400000","011216900000","012414400000","012414500000",
                                    "012414600000","012414700000","012414800000","012414900000","012415000000","012415100000","020811300000","020811400000","023015700000","023015800000",
                                    "023015900000","023016000000","023016100000","023016200000","023216300000","023216400000","023216500000","023216600000","030210100000","030210200000",
                                    "030210300000","030210400000","030210500000","031613100000","031613200000","040410600000","040410700000","040410800000","040416700000","040416800000",
                                    "042214000000","042214100000","042214200000","042214300000","042217200000","042815400000","042815500000","042815600000","051011500000","051011600000",
                                    "051011700000","051011800000","051011900000","052615200000","052615300000","060610900000","060611000000","060611100000","060611200000","061813300000",
                                    "061813400000","061813500000","061813600000","062013700000","062013800000","062013900000","071412800000","071412900000","071413000000","071422500000",
                                    "071422600000","071422700000","071427000000","071427100000","071427300000"), varId =  c("259853","259854","259849","259850","259851","259852"))
added10 <- added9 %>% mutate(unit = ifelse(unit == "011212000000", "PL214", 
                                           ifelse(unit == "011212100000", "PL213", 
                                                  ifelse(unit == "011212200000", "PL218", 
                                                         ifelse(unit == "011212300000", "PL21A", 
                                                                ifelse(unit == "011212400000", "PL217", 
                                                                       ifelse(unit == "011216900000", "PL219", 
                                                                              ifelse(unit == "012414400000", "PL225", 
                                                                                     ifelse(unit == "012414500000", "PL228", 
                                                                                            ifelse(unit == "012414600000", "PL224", 
                                                                                                   ifelse(unit == "012414700000", "PL229", 
                                                                                                          ifelse(unit == "012414800000", "PL22A", 
                                                                                                                 ifelse(unit == "012414900000", "PL227", 
                                                                                                                        ifelse(unit == "012415000000", "PL22B", 
                                                                                                                               ifelse(unit == "012415100000", "PL22C", 
                                                                                                                                      as.character(unit)))))))))))))))) %>%
  mutate(unit = ifelse(unit == "020811300000", "PL431",
                       ifelse(unit == "020811400000", "PL432", 
                              ifelse(unit == "023015700000", "PL416", 
                                     ifelse(unit == "023015800000", "PL414", 
                                            ifelse(unit == "023015900000", "PL417", 
                                                   ifelse(unit == "023016000000", "PL411", 
                                                          ifelse(unit == "023016100000", "PL418", 
                                                                 ifelse(unit == "023016200000", "PL415", 
                                                                        ifelse(unit == "023216300000", "PL426", 
                                                                               ifelse(unit == "023216400000", "PL427", 
                                                                                      ifelse(unit == "023216500000", "PL424", 
                                                                                             ifelse(unit == "023216600000", "PL428", 
                                                                                                    ifelse(unit == "030210100000", "PL515", 
                                                                                                           ifelse(unit == "030210200000", "PL516", 
                                                                                                                  as.character(unit)))))))))))))))) %>%
  mutate(unit = ifelse(unit == "030210300000", "PL517",
                       ifelse(unit == "030210400000", "PL518", 
                              ifelse(unit == "030210500000", "PL514", 
                                     ifelse(unit == "031613100000", "PL523", 
                                            ifelse(unit == "031613200000", "PL524", 
                                                   ifelse(unit == "040410600000", "PL613", 
                                                          ifelse(unit == "040410700000", "PL616", 
                                                                 ifelse(unit == "040410800000", "PL619", 
                                                                        ifelse(unit == "040416700000", "PL617", 
                                                                               ifelse(unit == "040416800000", "PL618", 
                                                                                      ifelse(unit == "042214000000", "PL634", 
                                                                                             ifelse(unit == "042214100000", "PL636", 
                                                                                                    ifelse(unit == "042214200000", "PL638", 
                                                                                                           ifelse(unit == "042214300000", "PL633", 
                                                                                                                  as.character(unit)))))))))))))))) %>%
  mutate(unit = ifelse(unit == "042217200000", "PL637",
                       ifelse(unit == "042815400000", "PL621", 
                              ifelse(unit == "042815500000", "PL623", 
                                     ifelse(unit == "042815600000", "PL622", 
                                            ifelse(unit == "051011500000", "PL712", 
                                                   ifelse(unit == "051011600000", "PL711", 
                                                          ifelse(unit == "051011700000", "PL713", 
                                                                 ifelse(unit == "051011800000", "PL714", 
                                                                        ifelse(unit == "051011900000", "PL715", 
                                                                               ifelse(unit == "052615200000", "PL721", 
                                                                                      ifelse(unit == "052615300000", "PL722", 
                                                                                             ifelse(unit == "060610900000", "PL811", 
                                                                                                    ifelse(unit == "060611000000", "PL812", 
                                                                                                           ifelse(unit == "060611100000", "PL814", 
                                                                                                                  as.character(unit)))))))))))))))) %>%
  mutate(unit = ifelse(unit == "060611200000", "PL815",
                       ifelse(unit == "061813300000", "PL821", 
                              ifelse(unit == "061813400000", "PL822", 
                                     ifelse(unit == "061813500000", "PL823", 
                                            ifelse(unit == "061813600000", "PL824", 
                                                   ifelse(unit == "062013700000", "PL841", 
                                                          ifelse(unit == "062013800000", "PL842", 
                                                                 ifelse(unit == "062013900000", "PL843", 
                                                                        ifelse(unit == "071412800000", "PL911", 
                                                                               ifelse(unit == "071412900000", "PL912", 
                                                                                      ifelse(unit == "071413000000", "PL913", 
                                                                                             ifelse(unit == "071422500000", "PL922", 
                                                                                                    ifelse(unit == "071422600000", "PL924", 
                                                                                                           ifelse(unit == "071422700000", "PL921", 
                                                                                                                  as.character(unit)))))))))))))))) %>%
  mutate(unit = ifelse(unit == "071427000000", "PL923",
                       ifelse(unit == "071427100000", "PL925", 
                              ifelse(unit == "071427300000", "PL926",   as.character(unit)))))
added10 <- added10 %>% rename(time = year) %>% rename(employment.B_F = "259849") %>% rename(employment.A = "259854") %>% rename(employment.G_J = "259850") %>% 
  rename(employment.K_L = "259851") %>% rename(employment.M_U = "259852") %>% rename(employment.TOTAL = "259853") %>% rename(geo = unit) %>% 
  select(-attributeDescription) %>% mutate_at(3:8, function(x)(x/1000)) %>% mutate(time = as.numeric(time))

data <- data %>% full_join(test2, by = c("geo", "time"), suffix = c("","_extra")) %>% split.default(str_remove(names(.), "_extra")) %>%
  map_df(~ coalesce(!!! .x)) %>% select(geo, time, everything()) 

  
  # Load data from Eurostat (Country)
  data <- data %>% left_join(get_eurostat(id = "nama_10_gdp", time_format = "num", filters = list(na_item = "B1GQ", unit = "CP_MEUR")) %>% 
            bind_cols(get_eurostat(id = "nama_10_gdp", time_format = "num", filters = list(na_item = "B1GQ", unit = "CP_MEUR"), type = "label")) %>% 
            select(-c("unit1","na_item1","time1","values1")) %>% rename(country.name = geo1), 
              by = c("country" = "geo","time")) %>% rename(gdp_country = values) %>% select(-c("unit","na_item")) %>% 
    mutate(gdp_share = gdp.MIO_EUR/gdp_country)
    
  data$country.name <- recode(data$country.name, "Germany (until 1990 former territory of the FRG)" = "Germany")
  
  # Get national TFP data from Penn World Tables
  data <- data %>% left_join(pwt9.1[ ,c("country","year","ctfp")], by = c("country.name" = "country","time" = "year"))
  
  
  # Load data from Eurostat (NUTS 2)
  data <- data %>% left_join(get_eurostat(id = "htec_emp_reg2", time_format = "num", filters = list(nace_r2 = "TOTAL", sex = "T", unit = "THS")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment_hitec.TOTAL = values) %>% select(-c("unit","sex","nace_r2")) %>%
    left_join(get_eurostat(id = "tour_occ_nin2", time_format = "num", filters = list(c_resid = "TOTAL", nace_r2 = "I551-I553", unit = "NR")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hotel_stays = values) %>% select(-c("unit","c_resid","nace_r2")) %>%
    left_join(get_eurostat(id = "tran_r_mago_nm", time_format = "num", filters = list(tra_meas = "FR_LD", unit = "THS_T")), 
            by = c("nuts.2" = "geo","time")) %>% rename(maritime_freight.load = values) %>% select(-c("unit","tra_meas")) %>%
    left_join(get_eurostat(id = "tran_r_mago_nm", time_format = "num", filters = list(tra_meas = "FR_NLD", unit = "THS_T")), 
              by = c("nuts.2" = "geo","time")) %>% rename(maritime_freight.unload = values) %>% select(-c("unit","tra_meas")) %>%
    mutate(maritime_freight = maritime_freight.load + maritime_freight.unload, na.rm = T) %>%
    left_join(get_eurostat(id = "tran_r_avgo_nm", time_format = "num", filters = list(tra_meas = "FRM_LD", unit = "THS_T")), 
              by = c("nuts.2" = "geo","time")) %>% rename(air_freight.load = values) %>% select(-c("unit","tra_meas")) %>% 
    left_join(get_eurostat(id = "tran_r_avgo_nm", time_format = "num", filters = list(tra_meas = "FRM_NLD", unit = "THS_T")), 
              by = c("nuts.2" = "geo","time")) %>% rename(air_freight.unload = values) %>% select(-c("unit","tra_meas")) %>%
    mutate(air_freight = air_freight.load + air_freight.unload, na.rm = T) %>%
    left_join(get_eurostat(id = "lfst_r_lfu3pers", time_format = "num", filters = list(age = "Y15-74", sex = "T", unit = "THS")), 
              by = c("nuts.2" = "geo","time")) %>% rename(unemployment.TOTAL = values) %>% select(-c("unit","age","sex")) %>%
    left_join(get_eurostat(id = "lfst_r_lfu3rt", time_format = "num", filters = list(age = "Y15-74", sex = "T", unit = "PC")), 
              by = c("nuts.2" = "geo","time")) %>% rename(unemployment.rate = values) %>% select(-c("unit","age","sex")) %>%
    left_join(get_eurostat(id = "jvs_a_nace2", time_format = "num", filters = 
                             list(indic_em = "JOBRATE", isco08 = "TOTAL", nace_r2 = "A-S", sizeclas = "TOTAL")), 
              by = c("nuts.2" = "geo","time")) %>% rename(job_vacancy.rate = values) %>% select(-c("indic_em","isco08","nace_r2","sizeclas")) %>%
    left_join(get_eurostat(id = "env_rwas_gen", time_format = "num", filters = list(wst_oper = "GEN", unit = "THS_T")), 
              by = c("nuts.2" = "geo","time")) %>% rename(waste = values) %>% select(-c("wst_oper","unit")) %>%
    left_join(get_eurostat(id = "ilc_peps11", time_format = "num", filters = list(unit = "PC")), 
              by = c("nuts.2" = "geo","time")) %>% rename(poverty.rate = values) %>% select(-c("unit")) %>%
    left_join(get_eurostat(id = "nama_10r_2gfcf", time_format = "num", filters = list(currency = "MIO_EUR", nace_r2 = "TOTAL")), 
            by = c("nuts.2" = "geo","time")) %>% rename(capital_fixed.TOTAL = values) %>% select(-c("currency","nace_r2")) %>%
    left_join(get_eurostat(id = "edat_lfse_04", time_format = "num", filters = list(age = "Y25-64", isced11 = "ED5-8", sex = "T", unit = "PC")), 
              by = c("nuts.2" = "geo","time")) %>% rename(tert_ed = values) %>% select(-c("age","isced11","sex","unit")) %>%
    left_join(get_eurostat(id = "nama_10r_2gdp", time_format = "num", filters = list(unit = "EUR_HAB")), 
              by = c("nuts.2" = "geo","time")) %>% rename(gdp.EUR_HAB.NUTS.2 = values) %>% select(-c("unit")) %>%
    left_join(get_eurostat(id = "tgs00102", time_format = "num", filters = list(unit = "PC", sex = "T", age = "Y20-64")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.20_64.NUTS.2 = values) %>% select(-c("unit","sex","age")) %>% 
    left_join(get_eurostat(id = "demo_r_d2jan", time_format = "num", filters = list(unit = "NR", sex = "T", age = "TOTAL")), 
              by = c("nuts.2" = "geo","time")) %>% rename(population.NUTS.2 = values) %>% select(-c("unit","sex","age")) %>% 
    left_join(get_eurostat(id = "tgs00002", time_format = "num", filters = list(unit = "KM2", landuse = "TOTAL")), 
            by = c("nuts.2" = "geo","time")) %>% rename(area.NUTS.2 = values) %>% select(-c("unit","landuse")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "TOTAL")), 
            by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.TOTAL = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "A")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.A = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "B-E")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.B_E = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "C")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.C = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "F")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.F = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "G-J")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.G_J = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "G-I")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.G_I = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "J")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.J = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "K-N")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.K_N = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "K")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.K = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "L")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.L = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "M_N")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.M_N = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "O-U")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.O_U = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "O-Q")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.O_Q = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "nama_10r_2emhrw", time_format = "num", filters = list(unit = "THS", wstatus = "EMP", nace_r2 = "R-U")), 
              by = c("nuts.2" = "geo","time")) %>% rename(hours.worked.R_U = values) %>% select(-c("unit","wstatus","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "TOTAL")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.TOTAL = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "A")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.A = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "B-E")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.B_E = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "F")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.F = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "G-I")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.G_I = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "J")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.J = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "K")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.K = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "L")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.L = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "M_N")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.M_N = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "O-Q")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.O_Q = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "R-U")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.R_U = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en2", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r2 = "NRP")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace2.NRP = values) %>% select(-c("unit","age","nace_r2")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "TOTAL")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.TOTAL = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "A_B")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.A_B = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "C-F")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace.1C_F = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "C-E")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace.1.C_E = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "F")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.F = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "G-Q")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.G_Q = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "G-I")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.G_I = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "J_K")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.J_K = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "L-Q")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.L_Q = values) %>% select(-c("unit","age","nace_r1")) %>%
    left_join(get_eurostat(id = "lfst_r_lfe2en1", time_format = "num", filters = list(unit = "THS", age = "Y_GE15", nace_r1 = "NRP")), 
              by = c("nuts.2" = "geo","time")) %>% rename(employment.o15.nace1.NRP = values) %>% select(-c("unit","age","nace_r1")) %>%
    mutate(employment.o15.TOTAL = ifelse(is.na(employment.o15.nace1.TOTAL),employment.o15.nace2.TOTAL,employment.o15.nace1.TOTAL)) %>% 
    mutate(employment.o15.F = ifelse(is.na(employment.o15.nace1.F),employment.o15.nace2.F,employment.o15.nace1.F)) %>%        
    mutate(employment.o15.G_I = ifelse(is.na(employment.o15.nace1.G_I),employment.o15.nace2.G_I,employment.o15.nace1.G_I)) %>%
    mutate(employment.o15.nace2.J_K = sum(employment.o15.nace2.J,employment.o15.nace2.K,na.rm = T)) %>%
    mutate(employment.o15.J_K = ifelse(is.na(employment.o15.nace1.J_K),employment.o15.nace2.J_K,employment.o15.nace1.J_K)) %>% 
    mutate(employment.o15.nace2.L_Q = sum(employment.o15.nace2.L,employment.o15.nace2.M_N,employment.o15.nace2.O_Q,na.rm = T)) %>%
    mutate(employment.o15.L_Q = ifelse(is.na(employment.o15.nace1.L_Q),employment.o15.nace2.L_Q,employment.o15.nace1.L_Q)) %>% 
    mutate(employment.o15.NRP = ifelse(is.na(employment.o15.nace1.NRP),employment.o15.nace2.NRP,employment.o15.nace1.NRP)) %>% 
    left_join(get_eurostat(id = "demo_r_pjanind2", time_format = "num", filters = list(unit = "PC", indic_de = "DEPRATIO1")), 
              by = c("nuts.2" = "geo","time")) %>% rename(dependency.ratio.NUTS.2 = values) %>% select(-c("unit","indic_de")) 
  
  
  # Add spatial information
added <- read.csv("Typologies.csv") %>%
  mutate(Coastal.region = ifelse(Coastal.region == "Y", 1, 0)) %>%
  mutate(Rural...urban.typology = ifelse(Rural...urban.typology == "Urban region" | Rural...urban.typology == "Intermediate region", 1, 0)) %>%
  mutate(Metro.region.corresponding.to.the.NUTS = ifelse(Metro.region.corresponding.to.the.NUTS == "Y", 1, 0))
  
added2 <- read.csv("Typologies2.csv") %>%
  select(1:11) %>% 
  mutate(Port.in.NUTS.3 = ifelse(Port.in.NUTS.3 == "Y", 1, 0)) 

added3 <- read.csv("regtyp.csv") %>%
  slice(-1) %>%
  select(1:10) %>% 
  mutate(urban.rural.including.remoteness = ifelse(urban.rural.including.remoteness %in% c(1,21,22), 1,
                                              ifelse(urban.rural.including.remoteness %in% c(31,32), 0, NA))) %>%
  mutate(metro.regions = ifelse(metro.regions %in% c(1,2,3), 1, 
                          ifelse(metro.regions == -1, NA, metro.regions))) %>%
  rename(NUTS.3.ID..2010. = NUTS.code)

added4 <- added %>% left_join(added2) %>% left_join(added3) %>% 
  mutate(ifelse(is.na(Rural...urban.typology), urban.rural.including.remoteness, Rural...urban.typology)) %>%
  mutate(ifelse(is.na(Metro.region.corresponding.to.the.NUTS), metro.regions, Metro.region.corresponding.to.the.NUTS)) %>%
  select(c(2,4:6,14)) %>% rename(geo = NUTS.3.ID..2010.)

# Join data sets and add additional spatial information
data2 <- data %>% left_join(added4) %>%
  filter(str_length(geo) > 4) %>% 
  mutate(Coastal.region = ifelse(is.na(Coastal.region) & geo %in% c("AL012","AL013","AL015","AL021","AL022","AL032","AL033","AL034","AL035","FI1D9",
  "FRD11","FRD12","FRD21","FRD22","FRE11","FRE12","FRE23","FRG01","FRG05","FRH01","FRH02","FRH03","FRH04","FRI12","FRI13","FRI15","FRI32","FRJ11",
  "FRJ12","FRJ13","FRJ15","FRL03","FRL04","FRL05","FRM01","FRM02","FRY10","FRY20","FRY30","FRY40","FRY50","IE041","IE042","IE051","IE052","IE053",
  "IE061","IE062","LT023","ME000","NL124","NL125","NL126","NL328","NL329","NL33B","NL33C","UKM71","UKM72","UKM72","UKM73","UKM75","UKM76","UKM77",
  "UKM78","UKM81","UKM82","UKM83","UKM84","UKM91","UKM92","UKM93","UKM94","UKM95","UKN06","UKN07","UKN08","UKN09","UKN10","UKN11","UKN12","UKN13",
  "UKN14","UKN15","UKN16","EL307"),1,
  ifelse(is.na(Coastal.region) & geo %in% c("AL011","AL014","AL031","DE91C","DEB1C","DEB1D","FI1D8","FRB01","FRB02","FRB03","FRB04","FRB05","FRB06",
  "FRC11","FRC12","FRC13","FRC14","FRC21","FRC22","FRC23","FRC24","FRD13","FRE21","FRE22","FRF11","FRF12","FRF21","FRF22","FRF23","FRF24","FRF31",
  "FRF32","FRF33","FRF34","FRG02","FRG03","FRG04","FRI11","FRI14","FRI21","FRI22","FRI23","FRI31","FRI33","FRI34","FRJ14","FRJ21","FRJ22","FRJ23",
  "FRJ24","FRJ25","FRJ26","FRJ27","FRJ28","FRK11","FRK12","FRK13","FRK14","FRK21","FRK22","FRK23","FRK24","FRK25","FRK26","FRK27","FRK28","FRL01",
  "FRL02","FRL06","HU110","HU120","IE063","LT011","LT021","LT022","LT024","LT025","LT026","LT027","LT028","LT029","MK001","MK002","MK003","MK004",
  "MK005","MK006","MK007","MK008","PL711","PL712","PL713","PL714","PL715","PL721","PL722","PL811","PL812","PL814","PL815","PL821","PL822","PL823",
  "PL824","PL841","PL842","PL843","PL911","PL912","PL913","PL921","PL922","PL923","PL924","PL925","PL926"),0,Coastal.region))) %>%
  mutate(Rural...urban.typology = ifelse(is.na(Rural...urban.typology) & geo %in% c("AL012","AL015","AL022","AL034","DE91C","FRB04","FRB06","FRC11",
  "FRC12","FRC21","FRC24","FRD11","FRE11","FRE12","FRE22","FRF11","FRF12","FRF22","FRF23","FRF31","FRF33","FRG01","FRG02","FRH03","FRI12","FRI15",
  "FRJ12","FRJ13","FRJ15","FRJ23","FRK14","FRK24","FRK25","FRK26","FRK27","FRK28","FRL03","FRL04","FRL05","FRL06","FRY10","FRY20","FRY30","FRY40",
  "FRY50","HU110","HU120","IE061","IE062","LT011","LT021","LT022","LT023","LT025","LT026","ME000","NL124","NL125","NL126","NL328","NL329","NL33B",
  "NL33C","PL711","PL712","PL814","PL841","PL911","PL912","PL913","PL921","UKM71","UKM72","UKM73","UKM75","UKM76","UKM77","UKM78","UKM81","UKM82",
  "UKM83","UKM84","UKM93","UKM94","UKM95","UKN06","UKN07","UKN09","UKN10","UKN13","UKN14","UKN15","EL307"),1,
  ifelse(is.na(Rural...urban.typology) & geo %in% c("AL011","AL013","AL014","AL021","AL031","AL032","AL033","AL035","DEB1C","DEB1D","FI1D8","FI1D9",
  "FRB01","FRB02","FRB03","FRB05","FRC12","FRC13","FRC14","FRC22","FRC23","FRD12","FRD13","FRD21","FRD22","FRE21","FRE23","FRF21","FRF24","FRF32",
  "FRF34","FRG03","FRG04","FRG05","FRH01","FRH02","FRH04","FRI11","FRI13","FRI14","FRI21","FRI22","FRI23","FRI31","FRI32","FRI33","FRI34","FRJ11",
  "FRJ14","FRJ21","FRJ22","FRJ24","FRJ25","FRJ26","FRJ27","FRJ28","FRK11","FRK12","FRK13","FRK21","FRK22","FRK23","FRL01","FRL02","FRM01","FRM02",
  "IE041","IE042","IE051","IE052","IE053","IE063","LT024","LT027","LT028","LT029","PL713","PL714","PL715","PL721","PL722","PL811","PL812","PL815",
  "PL821","PL822","PL823","PL824","PL842","PL843","PL922","PL923","PL924","PL925","PL926","UKM91","UKM92","UKN08","UKN11","UKN12","UKN16"),0,
  Rural...urban.typology))) %>%
  mutate(Metro.region.corresponding.to.the.NUTS = ifelse(is.na(Metro.region.corresponding.to.the.NUTS) & geo %in% c("DE91C","FRB04","FRB06","FRC11",
  "FRC21","FRD11","FRD22","FRE11","FRE23","FRF11","FRF12","FRF23","FRF31","FRG01","FRG02","FRG04","FRH02","FRH03","FRI12","FRI15","FRI23","FRI34",
  "FRJ12","FRJ13","FRJ15","FRJ23","FRK14","FRK24","FRK25","FRK26","FRK28","FRL03","FRL04","FRL05","HU110","HU120","IE053","IE061","IE062","LT011",
  "LT022","NL328","NL329","NL33C","PL711","PL712","PL721","PL814","PL823","PL841","PL911","PL912","PL913","PL921","UKM71","UKM73","UKM75","UKM78",
  "UKM81","UKM82","UKM83","UKM84","UKM95","UKN06","UKN09","UKN13","UKN14","EL307"),1,
  ifelse(is.na(Metro.region.corresponding.to.the.NUTS) & geo %in% c("AL011","AL012","AL013","AL014","AL015","AL021","AL022","AL031","AL032","AL033",
  "AL034","AL035","DEB1D","DEB1C","FI1D8","FI1D9","FRB01","FRB02","FRB03","FRB05","FRC12","FRC13","FRC14","FRC22","FRC23","FRC24","FRD12","FRD13",
  "FRD21","FRE12","FRE21","FRE22","FRF21","FRF22","FRF24","FRF32","FRF33","FRF34","FRG03","FRG05","FRH01","FRH04","FRI11","FRI13","FRI14","FRI21",
  "FRI22","FRI31","FRI32","FRI33","FRJ11","FRJ14","FRJ21","FRJ22","FRJ24","FRJ25","FRJ26","FRJ27","FRJ28","FRK11","FRK12","FRK13","FRK21","FRK22",
  "FRK23","FRK27","FRL01","FRL02","FRL06","FRM01","FRM02","FRY10","FRY20","FRY30","FRY40","FRY50","IE041","IE042","IE051","IE052","IE063","LT021",
  "LT023","LT024","LT025","LT026","LT027","LT028","LT029","ME000","NL124","NL125","NL126","NL33B","PL713","PL714","PL715","PL722","PL811","PL812",
  "PL815","PL821","PL822","PL824","PL842","PL843","PL922","PL923","PL924","PL925","PL926","UKM72","UKM76","UKM77","UKM91","UKM92","UKM93","UKM94",
  "UKN07","UKN08","UKN10","UKN11","UKN12","UKN15","UKN16"),0,Metro.region.corresponding.to.the.NUTS))) %>%
  mutate(Port.in.NUTS.3 = ifelse(is.na(Port.in.NUTS.3) & geo %in% c("EL307"),1,Port.in.NUTS.3)) %>% distinct(geo, time, .keep_all= TRUE)
  
# Enhance port dummy
port2013 <- read.csv("Ports_2013.csv") %>% rename(geo = GEO) %>% as_tibble %>% mutate(geo = as.character(geo))

data2 <- data2 %>% mutate(Port.in.NUTS.3 = ifelse(is.na(Port.in.NUTS.3) & geo %in% port2013$geo, 1,
                                                  ifelse(is.na(Port.in.NUTS.3), 0, Port.in.NUTS.3)))

# Take care of Dummies.csv
dummies <- read.csv("Dummies.csv") %>% as_tibble()
data2 <- data2 %>% left_join(dummies, by = "geo") %>% mutate(Coastal.region = ifelse(is.na(Coastal.region),coast,Coastal.region)) %>% 
  mutate(Metro.region.corresponding.to.the.NUTS = 
           ifelse(is.na(Metro.region.corresponding.to.the.NUTS),metro,Metro.region.corresponding.to.the.NUTS)) %>%
  mutate(Rural...urban.typology = ifelse(is.na(Rural...urban.typology),urban,Rural...urban.typology)) %>% select(-c("coast","urban","metro"))

# Reanme dummies
data2 <- data2 %>% rename(coast = Coastal.region) %>% rename(port = Port.in.NUTS.3) %>% rename(metro = Metro.region.corresponding.to.the.NUTS) %>%
  rename(urban = Rural...urban.typology)

# Drop discontinued NUTS-3 codes
data2 <- data2 %>% filter(!(geo %in% c("DE411","DE412","DE413","DE414","DE415","DE416","DE417","DE418","DE421","DE422","DE423","DE424","DE425","DE426",
"DE427","DE428","DE429","DE42A","DE801","DE802","DE805","DE806","DE807","DE808","DE809","DE80A","DE80B","DE80C","DE80D","DE80E","DE80F","DE80G",
"DE80H","DE80I","DE915","DE919","DEA21","DEA25","DEB16","DEB19","DED11","DED12","DED13","DED14","DED15","DED16","DED17","DED18","DED19","DED1A",
"DED1B","DED1C","DED22","DED23","DED24","DED25","DED26","DED27","DED28","DED29","DED2A","DED2B","DED31","DED32","DED33","DED34","DED35","DED36",
"EL111","EL112","EL113","EL114","EL115","EL121","EL122","EL123","EL124","EL125","EL126","EL127","EL131","EL132","EL133","EL134","EL141","EL142",
"EL143","EL144","EL211","EL212","EL213","EL214","EL221","EL222","EL223","EL224","EL231","EL232","EL233","EL241","EL242","EL243","EL244","EL245",
"EL251","EL252","EL253","EL254","EL255","EL300","EU27_2019","FI131","FI132","FI133","FI134","FI181","FI182","FI183","FI184","FI185","FI186","FI187",
"FI1A1","FI1A2","FI1A3","FI1D4","FI1D6","FR2","FR21","FR211","FR212","FR213","FR214","FR22","FR221","FR222","FR223","FR23","FR231","FR232","FR24",
"FR241","FR242","FR243","FR244","FR245","FR246","FR25","FR251","FR252","FR253","FR26","FR261","FR262","FR263","FR264","FR3","FR30","FR301","FR302",
"FR4","FR41","FR411","FR412","FR413","FR414","FR42","FR421","FR422","FR43","FR431","FR432","FR433","FR434","FR5","FR51","FR511","FR512","FR513",
"FR514","FR515","FR52","FR521","FR522","FR523","FR524","FR53","FR531","FR532","FR533","FR534","FR6","FR61","FR611","FR612","FR613","FR614","FR615",
"FR62","FR621","FR622","FR623","FR624","FR625","FR626","FR627","FR628","FR63","FR631","FR632","FR633","FR7","FR71","FR711","FR712","FR713","FR714",
"FR715","FR716","FR717","FR718","FR72","FR721","FR722","FR723","FR724","FR8","FR81","FR811","FR812","FR813","FR814","FR815","FR82","FR821","FR822",
"FR823","FR824","FR825","FR826","FR83","FR831","FR832","FRA","FRA1","FRA10","FRA2","FRA20","FRA3","FRA30","FRA4","FRA40","FRA5","FRA50","HR011",
"HR012","HR013","HR014","HR015","HR016","HR021","HR022","HR023","HR024","HR025","HR026","HR027","HR028","HU10","HU101","HU102","IE01","IE011","IE012",
"IE013","IE02","IE021","IE022","IE023","IE024","IE025","ITC45","ITD10","ITD20","ITD31","ITD32","ITD33","ITD34","ITD35","ITD36","ITD37","ITD41","ITD42",
"ITD43","ITD44","ITD51","ITD52","ITD53","ITD54","ITD55","ITD56","ITD57","ITD58","ITD59","ITE11","ITE12","ITE13","ITE14","ITE15","ITE16","ITE17",
"ITE18","ITE19","ITE1A","ITE21","ITE22","ITE31","ITE32","ITE33","ITE34","ITE41","ITE42","ITE43","ITE44","ITE45","ITF41","ITF42","LT00","LT001","LT002",
"LT003","LT004","LT005","LT006","LT007","LT008","LT009","LT00A","NL121","NL122","NL123","NL322","NL326","NL331","NL334","NL335","NL336","NL338",
"NL339","PL1","PL11","PL113","PL114","PL115","PL116","PL117","PL12","PL121","PL122","PL127","PL128","PL129","PL12A","PL12B","PL12C","PL12D","PL12E",
"PL215","PL216","PL3","PL31","PL311","PL312","PL314","PL315","PL32","PL323","PL324","PL325","PL326","PL33","PL331","PL332","PL34","PL343","PL344",
"PL345","PL422","PL423","PL425","PL521","PL522","PL614","PL615","PL631","PL635","PT113","PT114","PT115","PT116","PT117","PT118","PT161","PT162",
"PT163","PT164","PT165","PT166","PT167","PT168","PT169","PT16A","PT16C","PT171","PT172","PT182","PT183","SI011","SI012","SI013","SI014","SI015",
"SI016","SI017","SI018","SI021","SI022","SI023","SI024","UKD21","UKD22","UKD31","UKD32","UKD43","UKD51","UKD52","UKD53","UKD54","UKE43","UKF23",
"UKG34","UKG35","UKH13","UKH22","UKH33","UKI11","UKI12","UKI21","UKI22","UKI23","UKJ23","UKJ24","UKJ33","UKJ42","UKM2","UKM21","UKM22","UKM23","UKM24",
"UKM25","UKM26","UKM27","UKM28","UKM3","UKM31","UKM32","UKM33","UKM34","UKM35","UKM36","UKM37","UKM38","UKN01","UKN02","UKN03","UKN04","UKN05","BEXXX",
"DKXXX","FIXXX","ATZZZ","BEZZZ","DKZZZ","ESZZZ","FIZZZ","HUZZZ","ITZZZ","MTZZZ","NLZZZ","NOZZZ","PTZZZ","SEZZZ","UKZZZ")))


# Aggregating NUTS 3 variables to the NUTS 2 level
data2 <- data2 %>% group_by(nuts.2,time) %>% mutate(port.NUTS.2 = ifelse(mean(port, na.rm = T) > 0,1,ifelse(mean(port) == 0,0,NA))) %>% 
  mutate(urban.NUTS.2 = ifelse(mean(urban, na.rm = T) > 0,1,ifelse(mean(urban) == 0,0,NA))) %>%  
  mutate(metro.NUTS.2 = ifelse(mean(metro, na.rm = T) > 0,1,ifelse(mean(metro) == 0,0,NA))) %>%
  mutate(coast.NUTS.2 = ifelse(mean(coast, na.rm = T) > 0,1,ifelse(mean(coast) == 0,0,NA))) %>% 
  mutate(trade.mark.NUTS.2 = sum(trade.mark, na.rm = T)) %>%
  mutate(gva.TOTAL.NUTS.2 = sum(gva.TOTAL, na.rm = T)) %>% 
  mutate(road_freight.NUTS.2 = sum(road_freight, na.rm = T)) %>% mutate(road_freight.NUTS.2 = ifelse(road_freight.NUTS.2 == 0, NA, road_freight.NUTS.2)) %>% 
  mutate(gva.B_E_F.NUTS.2 = sum(gva.B_E_F, na.rm = T)) %>% mutate(gva.B_E_F_share.NUTS.2 = gva.B_E_F.NUTS.2/gva.TOTAL.NUTS.2) %>% 
  mutate(employment.TOTAL.NUTS.2 = sum(employment.TOTAL, na.rm = T)) %>% mutate(employment_hitec.TOTAL = sum(employment_hitec.TOTAL, na.rm = T)) %>%
  mutate(employment.TOTAL_capita.NUTS.2 = employment.TOTAL.NUTS.2/population.NUTS.2) %>% mutate(gva.TOTAL_capita.NUTS.2 = gva.TOTAL.NUTS.2/population.NUTS.2) %>%
  mutate(gdp.NUTS.2 = sum(gdp.MIO_EUR, na.rm = T)) %>% mutate(gdp_share.NUTS.2 = gdp.NUTS.2/gdp_country)


# Continue on setting up data
data3 <- data2  %>% mutate(gva.TOTAL_capita = gva.TOTAL*1000/population) %>% 
  mutate(employment.TOTAL_capita = employment.TOTAL/population) %>%
  mutate(gdp.EUR_HAB = as.numeric(gdp.EUR_HAB)) %>% as.data.table() %>% select(-c("na.rm")) %>% mutate(employment.TOTAL.NUTS.2.log = log(employment.TOTAL.NUTS.2)) %>%
  mutate(hours.worked.M_N.share = hours.worked.M_N/hours.worked.TOTAL) %>% mutate(hours.worked.G_I.share = hours.worked.G_I/hours.worked.TOTAL) %>%
  mutate(hours.worked.TOTAL.capita = hours.worked.TOTAL/population.NUTS.2) %>% mutate(employment.TOTAL.NUTS.2_capita = employment.TOTAL.NUTS.2/population.NUTS.2) 

data3 <- data3  %>% filter(!country == "EU")
levels(factor(data3$country))

# Create id
data3$id <- data3 %>% group_by(geo) %>% group_indices()
data3$id.NUTS.2 <- data3 %>% group_by(nuts.2) %>% group_indices()

# Creat country dummies
data3 <- data3 %>% to_dummy(country, suffix = "label") %>% bind_cols(data3) 

# Compose additional variables
data3 <- data3 %>% group_by(id) %>% mutate(pop.growth = ((population/dplyr::lead(population, order_by = id)-1)*100)) %>% 
  mutate(employment.TOTAL.growth = ((employment.TOTAL/dplyr::lead(employment.TOTAL, order_by = id)-1)*100)) %>% 
  mutate(employment.G_I.growth = ((employment.G_I/dplyr::lead(employment.G_I, order_by = id)-1)*100)) %>% 
  mutate(gva.TOTAL.growth = ((gva.TOTAL/dplyr::lead(gva.TOTAL, order_by = id)-1)*100)) %>% 
  mutate(employment.TOTAL.lag1 = dplyr::lead(employment.TOTAL, order_by = id)) %>% 
  mutate(employment.TOTAL.lag5 = dplyr::lead(employment.TOTAL, order_by = id, n = 5L)) %>% 
  mutate(employment.TOTAL.lag10 = dplyr::lead(employment.TOTAL, order_by = id, n = 10L)) %>% 
  mutate(dependency.ratio_new = (demo_r_pjanaggr3.TOTAL - demo_r_pjanaggr3.Y15_64) / demo_r_pjanaggr3.Y15_64) %>%
  mutate(employment.B.D_E = employment.B_E - employment.C) %>%
  mutate(employment.G_J = ifelse(is.na(employment.G_J), employment.G_I + employment.J, employment.G_J)) %>%
  mutate(employment.J = ifelse(is.na(employment.J), employment.G_J - employment.G_I, employment.J)) %>% 
  mutate(employment.G_J.growth = ((employment.G_J/dplyr::lead(employment.G_J, order_by = id)-1)*100)) %>%
  mutate(road_freight.growth = ((road_freight/dplyr::lead(road_freight, order_by = id)-1)*100)) %>% 
  group_by() %>%
  mutate(employment.G_I_capita = employment.G_I/population) %>%
  mutate(employment.G_J_capita = employment.G_J/population) %>% group_by(country, time) %>% 
  mutate(road_freight.country = sum(road_freight, na.rm = T)) %>%
  mutate(employment.TOTAL.country = sum(employment.TOTAL, na.rm = T)) %>% group_by()

# First graphical analysis
data3 %>% distinct(country, time, employment.TOTAL.country) %>% arrange(country, time) %>% group_by(country) %>% filter(!is.na(employment.TOTAL.country)) %>% 
  filter(!employment.TOTAL.country == 0) %>% filter(time >= 2000 & time <= 2016) %>% make.pbalanced(balance.type = "shared.individuals") %>%
  ggplot(aes(x = time, employment.TOTAL.country, color = factor(country))) + geom_line() + ylab("Employment (thousand persons)") + xlab("") + 
  labs(color = "Country code") #+ geom_vline(xintercept = 2011, linetype = "dotted")

data3 %>% group_by(country) %>% filter(time %in% 2011) %>% summarise(mean(road_freight, na.rm = T)) %>% print(n=100)

spatial <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = "2",
  year = "2016",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL
)
