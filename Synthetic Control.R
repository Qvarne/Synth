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

# version
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
            by = c("geo","time")) %>% rename(business.R_S  = values)
  
# Drop unit identifiers

data <- data %>% mutate(country = substr(geo,1,2)) %>% mutate(nuts.2 = substr(geo,1,4)) %>% 
    select(-c(grep("unit", colnames(data)), grep("wstatus", colnames(data)), 
           grep("nace_r2", colnames(data)), grep("currency", colnames(data)), grep("indic_sb", colnames(data)))) %>% select(-"landuse")
data <- data %>% to_dummy(country, suffix = "label") %>% bind_cols(data) 
  
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
              by = c("nuts.2" = "geo","time")) %>% rename(dependency.ratio = values) %>% select(-c("unit","indic_de")) 
  
  
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
  mutate(Port.in.NUTS.3 = ifelse(is.na(Port.in.NUTS.3) & geo %in% c("EL307"),1,
  ifelse(is.na(Port.in.NUTS.3) & geo %in% c(),0,Port.in.NUTS.3))) 
  

# Enhance port dummy
port2009 <- read.csv("Ports_2009.csv") %>% rename(geo = GEO) %>% as_tibble %>% mutate(geo = as.character(geo))

port2013 <- read.csv("Ports_2013.csv") %>% rename(geo = GEO) %>% as_tibble %>% mutate(geo = as.character(geo))

data2 <- data2 %>% mutate(Port.in.NUTS.3 = ifelse(geo %in% port2009$geo & time >= 2009 & time < 2013, 1, 
                                            ifelse(geo %in% port2013$geo & time >=2013, 1, Port.in.NUTS.3)))

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
  mutate(urban.NUTS.2 = ifelse(mean(urban, na.rm = T) > 0,1,ifelse(mean(urban) == 0,0,NA))) %>% mutate(population.NUTS.2 = sum(population)) %>% 
  mutate(gva.TOTAL.NUTS.2 = sum(gva.TOTAL, na.rm = T)) %>% mutate(road_freight.NUTS.2 = sum(road_freight, na.rm = T)) %>% 
  mutate(gva.B_E_F.NUTS.2 = sum(gva.B_E_F, na.rm = T)) %>% mutate(gva.B_E_F_share.NUTS.2 = gva.B_E_F.NUTS.2/gva.TOTAL.NUTS.2) %>% 
  mutate(employment.TOTAL.NUTS.2 = sum(employment.TOTAL, na.rm = T)) %>% mutate(employment_hitec.TOTAL = sum(employment_hitec.TOTAL, na.rm = T)) %>%
  mutate(employment.TOTAL_capita.NUTS.2 = employment.TOTAL.NUTS.2/population.NUTS.2) %>% mutate(gva.TOTAL_capita.NUTS.2 = gva.TOTAL.NUTS.2/population.NUTS.2) %>%
  mutate(gdp.NUTS.2 = sum(gdp.MIO_EUR, na.rm = T)) %>% mutate(gdp_share.NUTS.2 = gdp.NUTS.2/gdp_country)
 

# Continue on setting up data
data3 <- data2  %>% mutate(gva.TOTAL_capita = gva.TOTAL*1000/population) %>% 
  mutate(employment.TOTAL_capita = employment.TOTAL/population) %>%
  mutate(gdp.EUR_HAB = as.numeric(gdp.EUR_HAB)) %>% as.data.table() %>% select(-c("na.rm")) %>% mutate(employment.TOTAL.NUTS.2.log = log(employment.TOTAL.NUTS.2)) %>%
  mutate(hours.worked.M_N.share = hours.worked.M_N/hours.worked.TOTAL) %>% mutate(hours.worked.G_I.share = hours.worked.G_I/hours.worked.TOTAL)
# Create id
data3$id <- data3 %>% group_by(geo) %>% group_indices()
data3$id.NUTS.2 <- data3 %>% group_by(nuts.2) %>% group_indices()

data3 %>% filter(geo == "DE600")

# Reducing sample to obserrvations sharing shock
# data3 <- data3 %>% left_join(data3 %>% arrange(id.NUTS.2,time) %>%
#                                select(id.NUTS.2,time,hours.worked.TOTAL) %>%
#                                group_by(id.NUTS.2) %>%
#                                mutate(hours.worked.TOTAL_growth = c(NA,diff(hours.worked.TOTAL))/lag(hours.worked.TOTAL, 1)) %>%
#                                select(-hours.worked.TOTAL),by = c("id.NUTS.2","time"))
# 
# data3 %>% group_by(id.NUTS.2) %>% filter(time >= 2011 & time <= 2017) %>% mutate(mean_hours.worked.TOTAL_growth = mean(hours.worked.TOTAL_growth)) %>%
#   filter(nuts.2 == "DEA1") %>% select(mean_hours.worked.TOTAL_growth)
# 
# b <- data3 %>% group_by(id.NUTS.2) %>% filter(time >= 2010 & time <= 2013) %>% mutate(mean_hours.worked.TOTAL_growth = mean(hours.worked.TOTAL_growth)) %>%
#   filter(mean_hours.worked.TOTAL_growth > 0) %>% filter(time == 2013) %>% select(id.NUTS.2) %>% unlist() %>% unique()
# 
# data3 <- data3 %>% select(-hours.worked.TOTAL_growth)

# Preparing data set for synthetic control
data4 <- data3 %>% filter(time >= 2000 & time < 2017) %>% 
  distinct(id.NUTS.2, time, .keep_all= TRUE) %>% group_by(id.NUTS.2) %>% 
  # mutate(mean_gdp.EUR_HAB = ifelse(time < 2011 & is.na(gdp.EUR_HAB),
  #                             mean(gdp.EUR_HAB, na.rm = TRUE),gdp.EUR_HAB)) %>%
  # mutate(mean_business.B_S_X_K642 = ifelse(time < 2011 & is.na(business.B_S_X_K642),
  #                                  mean(business.B_S_X_K642, na.rm = TRUE),business.B_S_X_K642)) %>%
  mutate(mean_capital_fixed.TOTAL = ifelse(time < 2011 & is.na(capital_fixed.TOTAL),
                                   mean(capital_fixed.TOTAL, na.rm = TRUE),capital_fixed.TOTAL)) %>%
  mutate(mean_dependency.ratio = ifelse(time < 2011 & is.na(dependency.ratio), 
                                           mean(dependency.ratio, na.rm = TRUE),dependency.ratio)) %>%
  mutate(mean_gdp_share.NUTS.2 = ifelse(time < 2011 & is.na(gdp_share.NUTS.2),
                                           mean(gdp_share.NUTS.2, na.rm = TRUE),gdp_share.NUTS.2)) %>%
  mutate(mean_tert_ed = ifelse(time < 2011 & is.na(tert_ed),
                                   mean(tert_ed, na.rm = TRUE),tert_ed)) %>%
  # mutate(mean_employment.TOTAL_capita = ifelse(time < 2011 & is.na(employment.TOTAL_capita),
                                          # mean(employment.TOTAL_capita, na.rm = TRUE),employment.TOTAL_capita)) %>%
  # mutate(mean_employment.TOTAL_capita.NUTS.2 = ifelse(time < 2011 & is.na(employment.TOTAL_capita.NUTS.2),
  #                                              mean(employment.TOTAL_capita.NUTS.2, na.rm = TRUE),employment.TOTAL_capita.NUTS.2)) %>%
  # mutate(mean_gdp_share = ifelse(time < 2011 & is.na(gdp_share),
                                              # mean(gdp_share, na.rm = TRUE),gdp_share)) %>%
  # mutate(mean_population.density = ifelse(time < 2011 & is.na(population.density),
  #                                     mean(population.density, na.rm = TRUE),population.density)) %>%
  # mutate(mean_gva.B_E_F_share = ifelse(time > 2011 & is.na(gva.B_E_F_share),
                                       # mean(gva.B_E_F_share, na.rm = TRUE),gva.B_E_F_share)) %>%
  mutate(mean_gva.B_E_F_share.NUTS.2 = ifelse(time > 2011 & is.na(gva.B_E_F_share.NUTS.2),
                                       mean(gva.B_E_F_share.NUTS.2, na.rm = TRUE),gva.B_E_F_share.NUTS.2)) %>%
  # mutate(mean_road_freight.NUTS.2 = ifelse(time > 2011 & is.na(road_freight.NUTS.2),
  #                                             mean(road_freight.NUTS.2, na.rm = TRUE),road_freight.NUTS.2)) %>%
  # mutate(mean_coast = ifelse(is.na(coast),
  #                                     mean(coast, na.rm = TRUE),coast)) %>%
  # mutate(mean_urban = ifelse(is.na(urban),
  #                           mean(urban, na.rm = TRUE),urban)) %>%
  # mutate(mean_metro = ifelse(is.na(metro),
  #             mean(metro, na.rm = TRUE), metro)) %>%
  # mutate(mean_port = ifelse(is.na(port),
  #                                 mean(port, na.rm = TRUE),port)) %>%
  # filter(!is.na(mean_population.density)) %>% 
  # filter(!is.na(mean_business.B_S_X_K642)) %>% 
  filter(!is.na(mean_capital_fixed.TOTAL)) %>%
  # filter(!is.na(mean_road_freight.NUTS.2)) %>%
  filter(!is.na(mean_gdp_share.NUTS.2)) %>%
  filter(!is.na(mean_tert_ed)) %>%
  filter(!is.na(mean_dependency.ratio)) %>%
  # filter(!is.na(mean_employment.TOTAL_capita)) %>%
  # filter(!is.na(mean_employment.TOTAL_capita.NUTS.2)) %>%
  # filter(!is.na(mean_gdp_share)) %>%
  # filter(!is.na(mean_coast)) %>%
  # filter(!is.na(mean_urban)) %>%
  # filter(!is.na(mean_gva.B_E_F_share)) %>%
  filter(!is.na(mean_gva.B_E_F_share.NUTS.2)) %>%
  # filter(!is.na(mean_metro)) %>%
  # filter(!is.na(gva.TOTAL_capita.NUTS.2)) %>%
  # filter(!is.na(employment.TOTAL.NUTS.2.log)) %>%
  # filter(!is.na(maritime_freight)) %>%
  # filter(!is.na(mean_port)) %>%
  # filter(!is.na(mean_gdp.EUR_HAB)) %>%
  filter(!is.na(id.NUTS.2)) %>%
  filter(!is.na(hours.worked.G_I)) %>% 
  # filter(!is.na(id)) %>% 
  filter(!is.na(nuts.2)) %>% 
  filter(!is.na(time)) %>% 
  # filter(!is.na(geo)) %>% 
  as.data.frame() %>% 
  select(c(
           # "mean_population.density",
           # "mean_urban",
           # "mean_metro",
           # "gva.TOTAL_capita.NUTS.2",
           # "employment.TOTAL.NUTS.2.log",
           # "hours.worked.G_I",
           "mean_dependency.ratio",
           "hours.worked.G_I", 
           # "employment.TOTAL.NUTS.2",
           # "id",
           "id.NUTS.2",
           # "mean_road_freight.NUTS.2",
           "time",
           # "geo",
           # "country",
           "nuts.2",
           # "mean_gva.B_E_F_share",
           "mean_gva.B_E_F_share.NUTS.2",
           # "mean_port",
           # "mean_coast",
           # "mean_business.B_S_X_K642",
           "mean_capital_fixed.TOTAL",
           "mean_gdp_share.NUTS.2",
           "mean_tert_ed"
           # "mean_employment.TOTAL_capita",
           # "mean_employment.TOTAL_capita.NUTS.2"
           # "mean_gdp.EUR_HAB",
           # "mean_gdp_share"
           )) %>%
  distinct(id.NUTS.2, time, .keep_all= TRUE) %>% select(id.NUTS.2, time, everything())

# rm(list = c("added","added2","added3","added4","data","data2","port2011","port2013"))

# Adding dependent variable growth rate

data5 <- make.pbalanced(data4, balance.type = "shared.individuals")
data5 %>% filter(id.NUTS.2 == "69")
b <- unique(data5$id.NUTS.2)
# b <- intersect(b, unique(data5$id.NUTS.2))

# Trying to restrict the control group to observation with similar behaviour after treatment
# Negative growth in all periods
# b <- data5 %>% filter(time == "2011")  %>% filter(hours.worked.G_I_growth < 0) %>% select(id.NUTS.2) %>%
#   inner_join(data5 %>% filter(time == "2010")  %>% filter(hours.worked.G_I_growth < 0) %>% select(id.NUTS.2)) %>%
#   inner_join(data5 %>% filter(time == "2011")  %>% filter(hours.worked.G_I_growth < 0) %>% select(id.NUTS.2)) %>%
#   inner_join(data5 %>% filter(time == "2012")  %>% filter(hours.worked.G_I_growth < 0) %>% select(id.NUTS.2)) %>%
#   inner_join(data5 %>% filter(time == "2013")  %>% filter(hours.worked.G_I_growth < 0) %>% select(id.NUTS.2)) %>%
#   inner_join(data5 %>% filter(time == "2014")  %>% filter(hours.worked.G_I_growth < 0) %>% select(id.NUTS.2)) %>% unlist()
# 
# Mean negative growth


# c <- c[!c %in% c(155,322,403,644,1105,1112,1123)]
# Excluding from control for Duisburg (DEA12): DEA1C, DEA1D, DEA1F, DEA11, DEA14, DEA16, DEA17. c(403,413,414,416,402,405,407,408)
# Alternative (all DEA1): c(402:416)

# DEA12, HU110, EL307
# data5 %>% select(-c("id","time","geo","nuts.2")) %>% cor()

# data5 %>% filter(geo == "DEA12") %>% select(employment.TOTAL,time)
# 
# data3 %>% filter(geo == "DEA12") %>% ggplot(aes(x=time)) + 
#   geom_line(aes(y=employment.TOTAL,colour="employment.TOTAL")) + 
#   geom_line(aes(y=employment.A,colour="employment.A")) + 
#   geom_line(aes(y=employment.B_E,colour="employment.B_E")) + 
#   geom_line(aes(y=employment.C,colour="employment.C")) + 
#   geom_line(aes(y=employment.F,colour="employment.F")) + 
#   geom_line(aes(y=employment.G_I,colour="employment.G_I")) + 
#   geom_line(aes(y=employment.G_J,colour="employment.G_J")) + 
#   geom_line(aes(y=employment.J,colour="employment.J")) + 
#   geom_line(aes(y=employment.K,colour="employment.K")) + 
#   geom_line(aes(y=employment.K_N,colour="employment.K_N")) +
#   geom_line(aes(y=employment.L,colour="employment.L")) + 
#   geom_line(aes(y=employment.M_N,colour="employment.M_N")) + 
#   geom_line(aes(y=employment.O_Q,colour="employment.O_Q")) + 
#   geom_line(aes(y=employment.O_U,colour="employment.O_U")) +
#   geom_line(aes(y=employment.R_U,colour="employment.R_U")) 

# test <- plm(
#   gva.TOTAL_capita.NUTS.2.NUTS.2 ~
#     mean_gva.B_E_F_share.NUTS.2 +
#     mean_tert_ed +
#     mean_employment.TOTAL_capita.NUTS.2 +
#     mean_capital_fixed.TOTAL +
#     mean_tert_ed +
#     mean_gdp_share.NUTS.2 ,
#     data = data5,
#     index = c("id.NUTS.2", "time"),
#     model = "within",
#     effect = "twoways")
# summary(test)

# b <- unique(data5$id.NUTS.2)
# data5 %>% filter(nuts.2=="EL30")
# data5 %>% filter(nuts.2=="EL30") %>% filter(time=="2011")
# for (val in b) {
c <- b[!b %in%  c(69)]
# c <- b[!c %in%  c(69,60)]
# Run synthetic control estimation and plot results
dataprep.out<-
  dataprep(
    foo = data5,
    predictors = c(
      # "mean_population.density",
      # "mean_urban",
      # "mean_metro",
      # "gva.TOTAL_capita.NUTS.2",
      # "mean_gva.B_E_F_share",
      "mean_gva.B_E_F_share.NUTS.2",
      # "mean_port",
      # "mean_coast",
      # "mean_business.B_S_X_K642",
      "mean_capital_fixed.TOTAL",
      "mean_dependency.ratio",
      "mean_gdp_share.NUTS.2",
      "mean_tert_ed"
      # "mean_employment.TOTAL_capita.NUTS.2"
      # "mean_gdp.EUR_HAB",
      # "gdp_share"
                   ),
    predictors.op = "mean",
    dependent = "hours.worked.G_I",
    unit.variable = c("id.NUTS.2"),
    time.variable = c("time"),
    special.predictors = list(
      list("hours.worked.G_I", 2000, "mean"),
      # list("hours.worked.G_I", 2001, "mean"),
      # list("hours.worked.G_I", 2002, "mean"),
      # list("hours.worked.G_I", 2003, "mean"),
      # list("hours.worked.G_I", 2004, "mean"),
      list("hours.worked.G_I", 2005, "mean"),
      # list("hours.worked.G_I", 2006, "mean"),
      # list("hours.worked.G_I", 2007, "mean"),
      # list("hours.worked.G_I", 2008, "mean")
      # list("hours.worked.G_I", 2009, "mean"),
      list("hours.worked.G_I", 2010, "mean")
    ),
    treatment.identifier = 69,
    controls.identifier = c,
    time.predictors.prior = c(2000:2011),
    time.optimize.ssr = c(2000:2011),
    unit.names.variable = "nuts.2",
    time.plot = 2000:2016
  )


hours.worked.G_I.out <- synth(dataprep.out, verbose = TRUE, optimxmethod = "All")
# hours.worked.G_I.out <- improveSynth(hours.worked.G_I.out,dataprep.out)

# }

synth.tables <- synth.tab(dataprep.res = dataprep.out,synth.res = hours.worked.G_I.out)
print(synth.tables$tab.pred)
print(synth.tables$tab.v)
test <- synth.tables$tab.w %>% filter(w.weights > 0)
print(synth.tables$tab.w %>% filter(w.weights > 0))

path.plot(synth.res = hours.worked.G_I.out,
          dataprep.res = dataprep.out,
          tr.intake = 2011,
          Ylab = c("Hours worked in wholesale and retail trade, transportation and storage,",
                   "accommodation and food service activities (thousands)"),
          Xlab = c("year"),
          Legend = c("Duisburg","Synthetic Duisburg"),
)

data5 %>% filter(id.NUTS.2 == "92") %>% ggplot(aes(x=time)) + geom_line(aes(y=nace_r1,colour="nace_r1"))
data3 %>% filter(id.NUTS.2 %in% test$unit.numbers | nuts.2 == "DEA1") %>% ggplot(aes(x=time,colour=factor(id.NUTS.2))) + geom_line(aes(y=hours.worked.G_I)) +
  gghighlight(nuts.2 == "DEA1")
data3 %>% filter(country == "DE") %>% ggplot(aes(x=time,colour=factor(id.NUTS.2))) + geom_line(aes(y=hours.worked.G_I)) +
  gghighlight(nuts.2 == "DEA1")
data3 %>% filter(id.NUTS.2 == "69") %>% ggplot(aes(x=time)) + geom_line(aes(y=hours.worked.A,colour="hours.worked.A")) + 
  geom_line(aes(y=hours.worked.B_E,colour="hours.worked.B_E")) + geom_line(aes(y=hours.worked.C,colour="hours.worked.C")) + 
  geom_line(aes(y=hours.worked.F,colour="hours.worked.F")) + geom_line(aes(y=hours.worked.A,colour="hours.worked.A")) + 
  geom_line(aes(y=hours.worked.G_I,colour="hours.worked.G_I")) + geom_line(aes(y=hours.worked.G_J,colour="hours.worked.G_J")) + 
  geom_line(aes(y=hours.worked.J,colour="hours.worked.J")) + geom_line(aes(y=hours.worked.K_N,colour="hours.worked.K_N")) + 
  geom_line(aes(y=hours.worked.K,colour="hours.worked.K")) + geom_line(aes(y=hours.worked.L,colour="hours.worked.L")) + 
  geom_line(aes(y=hours.worked.M_N,colour="hours.worked.M_N")) + geom_line(aes(y=hours.worked.O_Q,colour="hours.worked.O_Q")) + 
  geom_line(aes(y=hours.worked.O_U,colour="hours.worked.O_U")) + geom_line(aes(y=hours.worked.R_U,colour="hours.worked.R_U"))

data3 %>% filter(id.NUTS.2 == "69") %>% ggplot(aes(x=time)) + geom_line(aes(y=employment.o15.F,colour="employment.o15.F")) + 
  geom_line(aes(y=employment.o15.G_I,colour="employment.o15.G_I")) + geom_line(aes(y=employment.o15.J_K,colour="employment.o15.J_K")) + 
  geom_line(aes(y=employment.o15.L_Q,colour="employment.o15.L_Q"))

data3 %>% filter(id.NUTS.2 == 69) %>% select(road_freight.NUTS.2)
data5 %>% filter(geo == "EL304") 

  synth.tables$tab.w$unit.names
synth.tables$tab.w %>% filter(w.weights > 0)

gaps.plot(synth.res = hours.worked.G_I.out,
          dataprep.res = dataprep.out,
          Ylab = c("Maritime freight (thousand tonnes)"),
          Xlab = c("Time"),
          Main = c("Gaps: Treated - Synthetic"),
          tr.intake = 2011,
          Ylim = NA,
          Z.plot = FALSE)
