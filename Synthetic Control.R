# Load necessary packages
library(eurostat)
library(dplyr)
library(stringr)
library(readxl)
library(Synth)
library(plm)
library(ggplot2)
library(data.table)
library(rgenoud)
library(MSCMT)
library(gsynth)
library(MatchIt)
library(purrr)
library(fastDummies)
library(sjmisc)
library(pwt9)
library(gghighlight)
library(lmtest)
library(sandwich)
library(robustbase)
library(clubSandwich)
library(ggpubr)
library(xtable)
library(stargazer)
library(Zelig)
library(cem)
library(mice)
library(optmatch)
library(influence.ME)

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
# port2009 <- read.csv("Ports_2009.csv") %>% rename(geo = GEO) %>% as_tibble %>% mutate(geo = as.character(geo))

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

added5 <- read.csv("NUTS_2016.csv")
added6 <- data2 %>% filter(!geo %in% added5$code) %>% select(geo) %>% distinct()
added6 <- added6[nchar(added6$geo)  == 5, ] %>% arrange(geo)
added6 %>% write.csv("added6.csv")

# Find a way to join continued and discontinued NUTS 3 codes (try a join)
# Find a way to sumarise all and keep rest
# Find a wy to join and replace NAs
# added7 <- data2 %>% filter(geo %in% added6$geo) %>% filter(geo %in% c("DE915","DE919","NO061","NO062"))   %>% 
#   mutate(geo = ifelse(geo == "DE915", "DE91C", 
#                       ifelse(geo == "DE919", "DE91C", 
#                              ifelse(geo == "NO061", "NO060", 
#                                     ifelse(geo == "NO062", "NO060", geo))))) %>% group_by(geo, time) %>% arrange(geo, time)
  

# Recode geo (continue here)
added8 <- data2 %>% filter(geo %in% added6$geo) %>%
  filter(!(geo %in% c("UKN02","UKN03","UKN03","UKN04","UKN05","DE915","DE919","NO061","NO062"))) %>% # UKN01 to UKN05 have been redrawn,; rest in added7
                     mutate(geo = ifelse(geo == "DEB16", "DEB1C", 
                                        ifelse(geo == "DEB19", "DEB1D", 
                                                               ifelse(geo == "FI1D4", "FI1D8", 
                                                                      ifelse(geo == "FI1D6", "FI1D9", 
                                                                             ifelse(geo == "FR211", "FRF21", 
                                                                                    ifelse(geo == "FR212", "FRF22", geo
                                                                                    ))))))) %>%
                      mutate(geo = ifelse(geo == "FR213", "FRF23", 
                                           ifelse(geo == "FR214", "FRF24", 
                                                  ifelse(geo == "FR221", "FRE21", 
                                                         ifelse(geo == "FR222", "FRE22", 
                                                                ifelse(geo == "FR223", "FRE23", 
                                                                       ifelse(geo == "FR231", "FRD21", 
                                                                              ifelse(geo == "FR232", "FRD22", 
                                                                                     ifelse(geo == "FR241", "FRB01", 
                                                                                            ifelse(geo == "FR242", "FRB02", 
                                                                                                   ifelse(geo == "FR243", "FRB03", 
                                                                                                          ifelse(geo == "FR244", "FRB04", 
                                                                                                                 ifelse(geo == "FR245", "FRB05", 
                                                                                                                        ifelse(geo == "FR246", "FRB06", 
                                                                                                                               ifelse(geo == "FR251", "FRD11", 
                                                                                                                                      ifelse(geo == "FR252", "FRD12",
                                                                                                                                             geo)))))))))))))))) %>%
                      mutate(geo = ifelse(geo == "FR253", "FRD13", 
                                           ifelse(geo == "FR261", "FRC11", 
                                                  ifelse(geo == "FR262", "FRC12", 
                                                         ifelse(geo == "FR263", "FRC13", 
                                                                ifelse(geo == "FR264", "FRC14", 
                                                                       ifelse(geo == "FR301", "FRE11", 
                                                                              ifelse(geo == "FR302", "FRE12", 
                                                                                     ifelse(geo == "FR411", "FRF31", 
                                                                                            ifelse(geo == "FR412", "FRF32", 
                                                                                                   ifelse(geo == "FR413", "FRF33", 
                                                                                                          ifelse(geo == "FR414", "FRF34", 
                                                                                                                 ifelse(geo == "FR421", "FRF11", 
                                                                                                                        ifelse(geo == "FR422", "FRF12", 
                                                                                                                               ifelse(geo == "FR431", "FRC21", 
                                                                                                                                      ifelse(geo == "FR432", "FRC22",
                                                                                                                                             geo)))))))))))))))) %>%
                      mutate(geo = ifelse(geo == "FR433", "FRC23", 
                                           ifelse(geo == "FR434", "FRC24", 
                                                  ifelse(geo == "FR511", "FRG01", 
                                                         ifelse(geo == "FR512", "FRG02", 
                                                                ifelse(geo == "FR513", "FRG03", 
                                                                       ifelse(geo == "FR514", "FRG04", 
                                                                              ifelse(geo == "FR515", "FRG05", 
                                                                                     ifelse(geo == "FR521", "FRH01", 
                                                                                            ifelse(geo == "FR522", "FRH02", 
                                                                                                   ifelse(geo == "FR523", "FRH03", 
                                                                                                          ifelse(geo == "FR524", "FRH04", 
                                                                                                                 ifelse(geo == "FR531", "FRI31", 
                                                                                                                        ifelse(geo == "FR532", "FRI32", 
                                                                                                                               ifelse(geo == "FR533", "FRI33", 
                                                                                                                                      ifelse(geo == "FR534", "FRI34",
                                                                                                                                             geo)))))))))))))))) %>%
                        mutate(geo = ifelse(geo == "FR611", "FRI11", 
                                           ifelse(geo == "FR612", "FRI12", 
                                                  ifelse(geo == "FR613", "FRI13", 
                                                         ifelse(geo == "FR614", "FRI14", 
                                                                ifelse(geo == "FR615", "FRI15", 
                                                                       ifelse(geo == "FR621", "FRJ21", 
                                                                              ifelse(geo == "FR622", "FRJ22", 
                                                                                     ifelse(geo == "FR623", "FRJ23", 
                                                                                            ifelse(geo == "FR624", "FRJ24", 
                                                                                                   ifelse(geo == "FR625", "FRJ25", 
                                                                                                          ifelse(geo == "FR626", "FRJ26", 
                                                                                                                 ifelse(geo == "FR627", "FRJ27", 
                                                                                                                        ifelse(geo == "FR628", "FRJ28", 
                                                                                                                               ifelse(geo == "FR631", "FRI21", 
                                                                                                                                      ifelse(geo == "FR632", "FRI22",
                                                                                                                                             geo)))))))))))))))) %>%
                        mutate(geo = ifelse(geo == "FR633", "FRI23", 
                                           ifelse(geo == "FR711", "FRK21", 
                                                  ifelse(geo == "FR712", "FRK22", 
                                                         ifelse(geo == "FR713", "FRK23", 
                                                                ifelse(geo == "FR714", "FRK24", 
                                                                       ifelse(geo == "FR715", "FRK25", 
                                                                              ifelse(geo == "FR716", "FRK26", 
                                                                                     ifelse(geo == "FR717", "FRK27", 
                                                                                            ifelse(geo == "FR718", "FRK28", 
                                                                                                   ifelse(geo == "FR721", "FRK11", 
                                                                                                          ifelse(geo == "FR722", "FRK12", 
                                                                                                                 ifelse(geo == "FR723", "FRK13", 
                                                                                                                        ifelse(geo == "FR724", "FRK14", 
                                                                                                                               ifelse(geo == "FR811", "FRJ11", 
                                                                                                                                      ifelse(geo == "FR812", "FRJ12",
                                                                                                                                             geo)))))))))))))))) %>%
                        mutate(geo = ifelse(geo == "FR813", "FRJ13", 
                                           ifelse(geo == "FR814", "FRJ14", 
                                                  ifelse(geo == "FR815", "FRJ15", 
                                                         ifelse(geo == "FR821", "FRL01", 
                                                                ifelse(geo == "FR822", "FRL02", 
                                                                       ifelse(geo == "FR823", "FRL03", 
                                                                              ifelse(geo == "FR824", "FRL04", 
                                                                                     ifelse(geo == "FR825", "FRL05", 
                                                                                            ifelse(geo == "FR826", "FRL06", 
                                                                                                   ifelse(geo == "FR831", "FRM01", 
                                                                                                          ifelse(geo == "FR832", "FRM02", 
                                                                                                                 ifelse(geo == "FRA10", "FRY10", 
                                                                                                                        ifelse(geo == "FRA20", "FRY20", 
                                                                                                                               ifelse(geo == "FRA30", "FRY30", 
                                                                                                                                      ifelse(geo == "FRA40", "FRY40",
                                                                                                                                             geo)))))))))))))))) %>%
                        mutate(geo = ifelse(geo == "FRA50", "FRY50", 
                                           ifelse(geo == "HU101", "HU110", 
                                                  ifelse(geo == "HU102", "HU120", 
                                                         ifelse(geo == "IE011", "IE041", 
                                                                ifelse(geo == "IE012", "IE063", 
                                                                       ifelse(geo == "IE013", "IE042", 
                                                                              ifelse(geo == "IE021", "IE061", 
                                                                                     ifelse(geo == "IE022", "IE062", 
                                                                                            ifelse(geo == "IE023", "IE051", 
                                                                                                   ifelse(geo == "IE024", "IE052", 
                                                                                                          ifelse(geo == "IE025", "IE053", 
                                                                                                                 ifelse(geo == "LT001", "LT021", 
                                                                                                                        ifelse(geo == "LT002", "LT022", 
                                                                                                                               ifelse(geo == "LT003", "LT023", 
                                                                                                                                      ifelse(geo == "LT004", "LT024",
                                                                                                                                             geo)))))))))))))))) %>%
                        mutate(geo = ifelse(geo == "LT005", "LT025", 
                                           ifelse(geo == "LT006", "LT026", 
                                                  ifelse(geo == "LT007", "LT027", 
                                                         ifelse(geo == "LT008", "LT028", 
                                                                ifelse(geo == "LT009", "LT029", 
                                                                       ifelse(geo == "LT00A", "LT011", 
                                                                              ifelse(geo == "NL121", "NL124", 
                                                                                     ifelse(geo == "NL122", "NL125", 
                                                                                            ifelse(geo == "NL123", "NL126", 
                                                                                                   ifelse(geo == "NL322", "NL328", 
                                                                                                          ifelse(geo == "NL326", "NL329", 
                                                                                                                 ifelse(geo == "NL338", "NL33B", 
                                                                                                                        ifelse(geo == "NL339", "NL33C", 
                                                                                                                               ifelse(geo == "PL113", "PL711", 
                                                                                                                                      ifelse(geo == "PL114", "PL712",
                                                                                                                                             geo)))))))))))))))) %>%
                        mutate(geo = ifelse(geo == "PL115", "PL713", 
                                             ifelse(geo == "PL116", "PL714", 
                                                    ifelse(geo == "PL117", "PL715", 
                                                           ifelse(geo == "PL127", "PL911", 
                                                                  ifelse(geo == "PL128", "PL921", 
                                                                         ifelse(geo == "PL129", "PL912", 
                                                                                ifelse(geo == "PL12A", "PL913", 
                                                                                       ifelse(geo == "PL12B", "PL922", 
                                                                                              ifelse(geo == "PL12C", "PL923", 
                                                                                                     ifelse(geo == "PL12D", "PL924", 
                                                                                                            ifelse(geo == "PL12E", "PL925", 
                                                                                                                   ifelse(geo == "PL311", "PL811", 
                                                                                                                          ifelse(geo == "PL312", "PL812", 
                                                                                                                                 ifelse(geo == "PL314", "PL814", 
                                                                                                                                        ifelse(geo == "PL315", "PL815",
                                                                                                                                               geo)))))))))))))))) %>%
                        mutate(geo = ifelse(geo == "PL323", "PL821", 
                                             ifelse(geo == "PL324", "PL822", 
                                                    ifelse(geo == "PL325", "PL823", 
                                                           ifelse(geo == "PL326", "PL824", 
                                                                  ifelse(geo == "PL331", "PL721", 
                                                                         ifelse(geo == "PL332", "PL722", 
                                                                                ifelse(geo == "PL343", "PL841", 
                                                                                       ifelse(geo == "PL344", "PL842", 
                                                                                              ifelse(geo == "PL345", "PL843", 
                                                                                                                   ifelse(geo == "UKM21", "UKM71", 
                                                                                                                          ifelse(geo == "UKM22", "UKM72", 
                                                                                                                                 ifelse(geo == "UKM23", "UKM73", 
                                                                                                                                        ifelse(geo == "UKM24", "UKM74",
                                                                                                                                               geo)))))))))))))) %>%
                         mutate(geo = ifelse(geo == "UKM25", "UKM75", 
                                             ifelse(geo == "UKM26", "UKM76", 
                                                    ifelse(geo == "UKM27", "UKM77", 
                                                           ifelse(geo == "UKM28", "UKM78", 
                                                                  ifelse(geo == "UKM31", "UKM81", 
                                                                         ifelse(geo == "UKM32", "UKM92", 
                                                                                ifelse(geo == "UKM33", "UKM93", 
                                                                                       ifelse(geo == "UKM34", "UKM82", 
                                                                                              ifelse(geo == "UKM35", "UKM83", 
                                                                                                     ifelse(geo == "UKM36", "UKM84", 
                                                                                                            ifelse(geo == "UKM37", "UKM94", 
                                                                                                                   ifelse(geo == "UKM38", "UKM95", 
                                                                                                                          ifelse(geo == "UKN01", "UKN06", 
                                                                                                                                 geo)))))))))))))) 

 
data2 <- data2 %>% full_join(added8, by = c("geo", "time"), suffix = c("","_extra")) %>% split.default(str_remove(names(.), "_extra")) %>%
  map_df(~ coalesce(!!! .x)) %>% select(geo, time, everything()) %>% select(1:134)


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
  mutate(gva.TOTAL.NUTS.2 = sum(gva.TOTAL, na.rm = T)) %>% mutate(road_freight.NUTS.2 = sum(road_freight, na.rm = T)) %>% 
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

# Create id
data3$id <- data3 %>% group_by(geo) %>% group_indices()
data3$id.NUTS.2 <- data3 %>% group_by(nuts.2) %>% group_indices()

# Creat country dummies
data3 <- data3 %>% to_dummy(country, suffix = "label") %>% bind_cols(data3) 

data3 <- data3 %>% group_by(id) %>% mutate(pop.growth = ((population/dplyr::lead(population, order_by = id)-1)*100)) %>% 
  mutate(gva.TOTAL.growth = ((gva.TOTAL/dplyr::lead(gva.TOTAL, order_by = id)-1)*100)) %>% 
  mutate(employment.TOTAL.lag1 = dplyr::lead(employment.TOTAL, order_by = id)) %>% 
  mutate(employment.TOTAL.lag5 = dplyr::lead(employment.TOTAL, order_by = id, n = 5L)) %>% 
  mutate(employment.TOTAL.lag10 = dplyr::lead(employment.TOTAL, order_by = id, n = 10L)) %>% 
  mutate(dependency.ratio_new = (demo_r_pjanaggr3.TOTAL - demo_r_pjanaggr3.Y15_64) / demo_r_pjanaggr3.Y15_64) %>%
  mutate(employment.B.D_E = employment.B_E - employment.C) %>%
  mutate(employment.J = employment.G_J - employment.G_I) %>% 
  mutate(employment.G_I_capita = employment.G_I/population) %>%
  mutate(employment.G_J_capita = employment.G_J/population) %>% group_by(country, time) %>% mutate(road_freight.country = sum(road_freight, na.rm = T)) %>% 
  mutate(employment.TOTAL.country = sum(employment.TOTAL, na.rm = T)) %>% group_by()


data3 %>% distinct(country, time, employment.TOTAL.country) %>% arrange(country, time) %>% group_by(country) %>% filter(!is.na(employment.TOTAL.country)) %>% 
  filter(!employment.TOTAL.country == 0) %>% filter(time >= 2000 & time <= 2016) %>% make.pbalanced(balance.type = "shared.individuals") %>%
  ggplot(aes(x = time, employment.TOTAL.country, color = factor(country))) + geom_line() + ylab("Employment (thousand persons)") + xlab("") + 
  labs(color = "Country code") #+ geom_vline(xintercept = 2011, linetype = "dotted")

test <- data3 %>% select(geo, time, road_freight, country) %>% filter(time %in% c(2000:2016)) %>% group_by(geo) %>% filter(!is.na(road_freight)) %>%
  make.pbalanced(balance.type = "shared.individuals") %>% arrange(geo, time)
nlevels(factor(test$country))

# # Evaluate different variables' behavior over time
# ggarrange(
#   data3 %>% filter(id == "584") %>% distinct(id, time, .keep_all= TRUE) %>% 
#     ggplot(aes(x = time)) + 
#       geom_line(aes(y = road_freight)) +
#       ylab(expression(atop("Road freight", paste("(thousand tonnes)")))) +
#       geom_vline(aes(xintercept = 2009), linetype = "dotted"),
#   data3 %>% filter(id == "584") %>% distinct(id, time, .keep_all= TRUE) %>% 
#     ggplot(aes(x = time)) + 
#     geom_line(aes(y = employment.TOTAL)) +
#     ylab(expression(atop("Employment", paste("(thousand persons)")))) +
#     geom_vline(aes(xintercept = 2009), linetype = "dotted"),
#   data3 %>% filter(id == "584") %>% distinct(id, time, .keep_all= TRUE) %>% 
#     ggplot(aes(x = time)) + 
#     geom_line(aes(y = employment.TOTAL/population)) +
#     ylab(expression(atop("Employment per capita", paste("(rate)")))) +
#     geom_vline(aes(xintercept = 2009), linetype = "dotted"),
#   data3 %>% filter(id == "584") %>% distinct(id, time, .keep_all= TRUE) %>% 
#     ggplot(aes(x = time)) + 
#     geom_line(aes(y = population)) +
#     ylab(expression(atop("Population", paste("thousand persons")))) +
#     geom_vline(aes(xintercept = 2009), linetype = "dotted"),
#   data3 %>% filter(id == "584") %>% distinct(id, time, .keep_all= TRUE) %>% 
#     ggplot(aes(x = time)) + 
#     geom_line(aes(y = gva.TOTAL)) +
#     ylab(expression(atop("Gross value added", paste("thousand euros")))) +
#     geom_vline(aes(xintercept = 2009), linetype = "dotted"),
#   data3 %>% filter(id == "584") %>% distinct(id, time, .keep_all= TRUE) %>% 
#     ggplot(aes(x = time)) + 
#     geom_line(aes(y = gva.TOTAL/population)) +
#     ylab(expression(atop("Gross value added per capita", paste("rate")))) +
#     geom_vline(aes(xintercept = 2009), linetype = "dotted")
# ,ncol = 2, nrow = 4)
# 
# data3 %>% filter(country == "EL") %>% ggplot(aes(x=time,colour=factor(id))) + geom_line(aes(y=employment.TOTAL)) +
#   gghighlight(geo == "EL307")
# 

# Revisit matching
# Define treatment variable
# data3 <- data3 %>% mutate(rail = ifelse(geo == "DEA12" & time >= 2011, 1,
#                                       ifelse(geo == "PL811" & time >= 2011, 1, 
#                                              ifelse(geo == "CZ053" & time >= 2012, 1,
#                                                     ifelse(geo == "PL911" & time >= 2012, 1,
#                                                            ifelse(geo == "DE600" & time >= 2013, 1,
#                                                                   ifelse(geo == "PL711" & time >= 2013, 1,
#                                                                          ifelse(geo == "ES300" & time >= 2014, 1, 0))))))))
# 
# # md.pattern(test.complete)
# # test <- data3 %>% select(c("rail",
# #                            "metro",
# #                            "urban",
# #                            "employment.TOTAL",
# #                            "coast",
# #                            "port",
# #                            "time",
# #                            "gva.B_E_F_share",
# #                            "gdp_share",
# #                            "gva.TOTAL_capita",
# #                            "population.density",
# #                            "id",
# #                            "geo")) %>% mice()
# # test.complete <- complete(test,1)
# # Prepare data for matching
# # Continue here; work out matching
# data4 <- data3 %>% select(c("rail",
#                             "metro",
#                             "population",
#                             "urban",
#                             "employment.TOTAL",
#                             "coast",
#                             "port",
#                             "time",
#                             # "gva.B_E_F_share",
#                             # "gdp_share",
#                             "dependency.ratio_new",
#                             "gva.TOTAL_capita",
#                             # "population.density",
#                             # "pop.growth",
#                             "id",
#                             "gva.TOTAL.growth",
#                             # "employment.TOTAL.lag10",
#                             # "employment.TOTAL.lag5",
#                             "geo"
#                             )) %>%
#   filter(time >= 2011 & time <= 2017) %>% group_by(id) %>% 
#   mutate(gva.TOTAL.growth = ifelse(is.na(gva.TOTAL.growth), mean(gva.TOTAL.growth, na.rm = T), gva.TOTAL.growth)) %>%
#   # mutate(employment.TOTAL = ifelse(is.na(employment.TOTAL), mean(employment.TOTAL, na.rm = T), employment.TOTAL)) %>%
#   mutate(dependency.ratio_new = ifelse(is.na(dependency.ratio_new), mean(dependency.ratio_new, na.rm = T), dependency.ratio_new)) %>%
#   # mutate(employment.TOTAL.lag1 = ifelse(is.na(employment.TOTAL.lag1), mean(employment.TOTAL.lag1, na.rm = T), employment.TOTAL.lag1)) %>%
#   mutate(gva.TOTAL_capita = ifelse(is.na(gva.TOTAL_capita), mean(gva.TOTAL_capita, na.rm = T), gva.TOTAL_capita)) %>%
#   mutate(population = ifelse(is.na(population), mean(population, na.rm = T), population)) %>%
#   # filter(!is.na(gva.TOTAL.growth)) %>%
#   # filter(!is.na(rail)) %>%
#   # filter(!is.na(metro)) %>%
#   filter(!is.na(employment.TOTAL)) %>%
#   # filter(!is.na(urban)) %>%
#   filter(!is.na(coast)) %>%
#   # # filter(!is.na(dependency.ratio_new)) %>%
#   # filter(!is.na(employment.TOTAL.lag1)) %>%
#   # filter(!is.na(employment.TOTAL)) %>%
#     # filter(!is.na(port)) %>%
#   # filter(!is.na(time)) %>%
#   # # filter(!is.na(gva.TOTAL_capita)) %>%
#   # filter(!is.na(id)) %>%
#   # filter(!is.na(geo)) %>%
#   # # filter(!is.na(gva.B_E_F_share)) %>%
#   # # filter(!is.na(gdp_share)) %>%
#   # # filter(!is.na(population.density))  %>%
#   filter(!is.na(population))  %>%
#   # # filter(!is.na(pop.growth))  %>%
#   make.pbalanced(balance.type = "shared.individuals") %>%
#   as.data.frame() %>% arrange(id, time)
# 
# # # Fit regression to justify predictor variable selection
# lm.test <- data %>% filter(time >= 2000 & time < 2017) %>% lm(log(employment.TOTAL) ~
#                           # population.density +
#                           population +
#                           coast +
#                           urban +
#                           # employment.TOTAL.lag1 +
#                           port +
#                           # gva.B_E_F_share +
#                           dependency.ratio_new +
#                           metro +
#                           gva.TOTAL_capita +
#                           # pop.growth +
#                           # gdp_share +
#                           gva.TOTAL.growth,
#                         .)
# lm.test %>% summary() %>% print() %>% bptest()
# lm.test.robust <- lm.test %>% coeftest(vcov = vcovHC(.)) %>% print()
# stargazer(lm.test.robust)
# # test.complete %>% lmrob(employment.TOTAL ~ population + population.density + coast + urban + port + metro + gva.TOTAL_capita + gva.B_E_F_share + trade.mark, .) %>% summary()
# 
# plm.test <- data4 %>% distinct(id, time, .keep_all= TRUE) %>%
#   plm(log(employment.TOTAL) ~
#         # population.density +
#         population +
#         coast +
#         urban +
#         # employment.TOTAL.lag1 +
#         port +
#         # gva.B_E_F_share +
#         dependency.ratio_new +
#         metro +
#         gva.TOTAL_capita +
#         # pop.growth +
#         # gdp_share +
#         gva.TOTAL.growth,
#       index = c("id","time"), model="within", effect="twoways", data = .)
# plm.test %>% summary() %>% print() %>% bptest()
# plm.test.robust <- plm.test %>%coeftest(., vcov=vcovHC(.,type="HC0",cluster="group")) %>% print
# stargazer(plm.test.robust)
# mean(fixef(plm.test))
# se.fixef(plm.test)
# 
# summary(data4$time)
# # Carry out matching using the match MatchIt package
# m.out <- matchit(rail ~
#                    # population.density + 
#                    population + 
#                    coast + 
#                    urban + 
#                    # employment.TOTAL.lag1 +
#                    port + 
#                    # gva.B_E_F_share +
#                    dependency.ratio_new +
#                    metro + 
#                    gva.TOTAL_capita + 
#                    # pop.growth +
#                    # gdp_share +
#                    gva.TOTAL.growth, 
#                  data = data4, method = "optimal", distance = "mahalanobis")  
# # mahalanobis
# summary(m.out)
# plot(m.out)
# m.data <- match.data(m.out, distance ="pscore")
# print(m.data)
# z.out <- zelig(log(employment.TOTAL) ~ 
#                  rail,
#                  # population.density + 
#                  # population + 
#                  # coast + 
#                  # urban + 
#                  # employment.TOTAL.lag1 +
#                  # port + 
#                  # # gva.B_E_F_share +
#                  # dependency.ratio_new +
#                  # metro + 
#                  # gva.TOTAL_capita + 
#                  # # pop.growth +
#                  # # gdp_share +
#                  # gva.TOTAL.growth,
#                data = m.out, model = "ls")
# print(z.out)
# x.out <- setx(z.out, data = match.data(m.out, "treat"), cond = TRUE)
# print(x.out)
# s.out <- sim(z.out, x = x.out)
# summary(s.out)
# stargazer(s.out)
# 
# 
data3$employment.TOTAL_capita
 # Preparing data set for synthetic control
data4 <- data3 %>% filter(time >= 2000 & time < 2017) %>% #filter(country == "DE") %>% 
  distinct(id, time, .keep_all= TRUE) %>%
  mutate(treatment = ifelse(time >= 2009, 1, 0)) %>%
  group_by(id, treatment) %>%
  # mutate(gva.B_E_F_share = ifelse(time < 2009 & is.na(gva.B_E_F_share), mean(gva.B_E_F_share, na.rm = TRUE), gva.B_E_F_share)) %>%
  # mutate(trade.mark = ifelse(time < 2009 & is.na(trade.mark), mean(trade.mark, na.rm = TRUE), trade.mark)) %>%
  # mutate(gva.TOTAL_capita = ifelse(time < 2009 & is.na(gva.TOTAL_capita), mean(gva.TOTAL_capita, na.rm = TRUE), gva.TOTAL_capita)) %>%
  # mutate(population = ifelse(time < 2009 & is.na(population), mean(population, na.rm = TRUE), population)) %>%
  # mutate(gva.TOTAL.growth = ifelse(time < 2009 & is.na(gva.TOTAL.growth), mean(gva.TOTAL.growth, na.rm = TRUE), gva.TOTAL.growth)) %>%
  # mutate(dependency.ratio_new = ifelse(time < 2009 & is.na(dependency.ratio_new), mean(dependency.ratio_new, na.rm = TRUE), dependency.ratio_new)) %>%
  # mutate(population = ifelse(time < 2009 & is.na(population), mean(population, na.rm = TRUE), population)) %>%
  # mutate(coast = ifelse(is.na(coast) & mean(coast, na.rm = T) > 0, 1, coast)) %>%
  # mutate(gdp_share = ifelse(time < 2009 & is.na(gdp_share), mean(gdp_share, na.rm = TRUE), gdp_share)) %>%
  # mutate(population.density = ifelse(time < 2009 & is.na(population.density), mean(population.density, na.rm = TRUE), population.density)) %>% group_by(id) %>%
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
  filter(!is.na(road_freight)) %>%
  filter(!is.na(geo)) %>%
  filter(!is.na(time)) %>%
  # filter(!is.na(gva.B_E_F_share)) %>%
  # filter(!is.na(population)) %>%
  # # filter(!is.na(trade.mark)) %>%
  # filter(!is.na(gva.TOTAL_capita)) %>%
  # filter(!is.na(dependency.ratio_new)) %>%
  # filter(!is.na(gva.TOTAL.growth)) %>%
  # filter(!is.na(maritime_freight)) %>%
  # filter(!is.na(coast)) %>%
  # filter(!is.na(port)) %>%
  # filter(!is.na(metro)) %>%
  # filter(!is.na(urban)) %>%
  # filter(!is.na(gdp_share)) %>%
  # filter(!is.na(population.density)) %>%
  as.data.frame() %>% 
  select(c(
           # "employment.TOTAL",
           # "employment.A",
           # "employment.B.D_E",
           # "employment.C",
           # "employment.F",
           # "employment.G_J",
           # "employment.J",
           # "employment.K",
           # "employment.L",
           # "employment.K_N",
           # "employment.O_U",
           # "employment.R_U",
           # "employment.TOTAL_capita",
           # "employment.G_J_capita",
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
           # # # "trade.mark",
           # "urban"
           "road_freight"
           # "maritime_freight"
           # "population.density",
           # "gdp_share"
           )) %>%
  distinct(id, time, .keep_all= TRUE) %>% select(id, time, everything())
data5 <- make.pbalanced(data4, balance.type = "shared.individuals")
data5 %>% filter(geo == "DEA12")
summary(data5$time)

data3 %>% filter(country == "DE") %>% select(employment.O_U) %>% summary()

# lm.test <- data5 %>% lm(log(employment.TOTAL) ~
#                                                                 # population.density +
#                                                                 population +
#                                                                 coast +
#                                                                 urban +
#                                                                 # employment.A.lag1 +
#                                                                 port +
#                                                                 # gva.B_E_F_share +
#                                                                 dependency.ratio_new +
#                                                                 metro +
#                                                                 gva.TOTAL_capita +
#                                                                 # pop.growth +
#                                                                 # gdp_share +
#                                                                 gva.TOTAL.growth,
#                                                               .)
# lm.test %>% summary() %>% print() %>% bptest()
# lm.test.robust <- lm.test %>% coeftest(vcov = vcovHC(.)) %>% print()
# stargazer(lm.test.robust)
# 
# 
# 
# plm.test <- data4 %>% distinct(id, time, .keep_all= TRUE) %>%
#   plm(log(employment.TOTAL) ~
#         # population.density +
#         population +
#         coast +
#         urban +
#         # employment.A.lag1 +
#         port +
#         # gva.B_E_F_share +
#         dependency.ratio_new +
#         metro +
#         gva.TOTAL_capita +
#         # pop.growth +
#         # gdp_share +
#         gva.TOTAL.growth,
#       index = c("id","time"), model="within", effect="twoways", data = .)
# plm.test %>% summary() %>% print() %>% bptest()
# plm.test.robust <- plm.test %>%coeftest(., vcov=vcovHC(.,type="HC0",cluster="group")) %>% print
# stargazer(plm.test.robust)
# mean(fixef(plm.test))
# se.fixef(plm.test)



# data5 %>% filter(id == "403")
data5 %>% filter(geo == "DEA1")

b <- unique(data5$id) 

# c <- b[!b %in%  c(69)]
c <- b[!b %in%  c(402:416)]
# c <- b[!b %in%  c(578:584)]

# Run synthetic control estimation and plot results
dataprep.out<-
  dataprep(
    foo = data5,
    predictors = c(
      # population.density +
      # "population",
      #   "coast",
      #   "urban",
      #   # employment.A.lag1 +
        # "port",
      #   # gva.B_E_F_share +
        # "dependency.ratio_new",
        # "metro",
        # "gva.TOTAL_capita",
        # pop.growth +
      #   # gdp_share +
        # "gva.TOTAL.growth"
                   ),
    predictors.op = "mean",
    dependent = "road_freight",
    unit.variable = c("id"),
    time.variable = c("time"),
    special.predictors = list(
      list("road_freight", 2000, "mean"),
      list("road_freight", 2001, "mean"),
      list("road_freight", 2002, "mean"),
      list("road_freight", 2003, "mean"),
      # list("road_freight", 2004, "mean"),
      list("road_freight", 2005, "mean"),
      list("road_freight", 2006, "mean"),
      list("road_freight", 2007, "mean"),
      list("road_freight", 2008, "mean"),
      list("road_freight", 2009, "mean"),
      list("road_freight", 2010, "mean")
    ),
    treatment.identifier = 403,
    controls.identifier = c,
    time.predictors.prior = c(2000:2010),
    time.optimize.ssr = c(2000:2010),
    unit.names.variable = "geo",
    time.plot = 2000:2016
  )


road_freight.out <- synth(dataprep.out) # verbose = TRUE, optimxmethod = "All")
dataprep.out$Y1plot
dataprep.out$Y0plot %*% road_freight.out$solution.w
synth.tables <- synth.tab(dataprep.res = dataprep.out,synth.res = road_freight.out)
print(synth.tables$tab.pred)
print(synth.tables$tab.v)
test <- synth.tables$tab.w %>% filter(w.weights > 0)
print(synth.tables$tab.w %>% filter(w.weights > 0))

path.plot(synth.res = road_freight.out,
          dataprep.res = dataprep.out,
          tr.intake = 2011,
          Ylab = c("Road freight (thousand tonnes)"),
          Xlab = c("year"),
          Legend = c("Duisburg","Synthetic Duisburg"),
)
xtable(synth.tables$tab.v, digits = 3)

# data3 %>% filter(id %in% test$unit.numbers | geo == "DEA1") %>% ggplot(aes(x = time, colour = factor(id))) + geom_line(aes(y = employment.TOTAL)) +
#   gghighlight(geo == "DEA1")
# data4 %>% ggplot(aes(x = time,colour=factor(id))) + geom_line(aes(y = employment.TOTAL)) +
#   gghighlight(id %in% c(578:583)) + ylab("Employment (thousand persons)") + theme(legend.position = "none") + xlab("") + xlim(2000, 2016)
# data3 %>% filter(id == "69") %>% ggplot(aes(x=time)) + geom_line(aes(y=hours.worked.A,colour="hours.worked.A")) + 
#   geom_line(aes(y=hours.worked.B_E,colour="hours.worked.B_E")) + geom_line(aes(y=hours.worked.C,colour="hours.worked.C")) + 
#   geom_line(aes(y=hours.worked.F,colour="hours.worked.F")) + geom_line(aes(y=hours.worked.A,colour="hours.worked.A")) + 
#   geom_line(aes(y=hours.worked.G_I,colour="hours.worked.G_I")) + geom_line(aes(y=hours.worked.G_J,colour="hours.worked.G_J")) + 
#   geom_line(aes(y=hours.worked.J,colour="hours.worked.J")) + geom_line(aes(y=hours.worked.K_N,colour="hours.worked.K_N")) + 
#   geom_line(aes(y=hours.worked.K,colour="hours.worked.K")) + geom_line(aes(y=hours.worked.L,colour="hours.worked.L")) + 
#   geom_line(aes(y=hours.worked.M_N,colour="hours.worked.M_N")) + geom_line(aes(y=hours.worked.O_Q,colour="hours.worked.O_Q")) + 
#   geom_line(aes(y=hours.worked.O_U,colour="hours.worked.O_U")) + geom_line(aes(y=hours.worked.R_U,colour="hours.worked.R_U"))