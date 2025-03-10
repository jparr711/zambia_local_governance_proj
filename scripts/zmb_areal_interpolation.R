# areal interpolation
# install.packages("areal")
library(areal)
require("ipumsr")
library(tidyverse)
library(sf)
library(rgdal)
library(raster)

# current districts shp
zmb_adm2 <- readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_adm_dmmu_20201124_shp/Shapefiles/zmb_admbnda_adm2_dmmu_20201124.shp")

# zambia census with old districts
zmb_ddi <- read_ipums_ddi("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ipumsi_00002.xml")
# microdata
zmb_ipums_data <- read_ipums_micro(zmb_ddi)
# shapefile of old distirict
zmb_adm2_2010 <- readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/geo2_zm2010/geo2_zm2010.shp")

# convert to sf format for areal interpolation
zmb_adm2_2010 <- sf::st_as_sf(zmb_adm2_2010)
zmb_adm2 <- sf::st_as_sf(zmb_adm2)

# Create PS frame using 2010 admin districts.
# PS frame work
# covariates for psframe: age, language, electricity, cellular, relig, sanitation, internet
zmb_ipums_data <- zmb_ipums_data %>%
  filter(YEAR == 2010)

# lAng
zmb_ipums_data %>% 
  dplyr::count(LANGZM1) %>% 
  arrange(desc(n))
as.data.frame()

zmb_ipums_data %>% 
  dplyr::group_by(GEO1_ZM2010) %>%
  dplyr::count(LANGZM1) %>% 
#  arrange(desc(n)) %>%
  mutate(n/sum(n))


# Task item - Other for langs not in survey
zmb_ipums_data <- 
  zmb_ipums_data %>% 
  mutate(language = haven::as_factor(LANGZM1),
         language = tolower(language),
         language = forcats::fct_other(language, keep = c("bemba", "english",
                                                          "kaonde", "lozi",
                                                          "lunda (luapula)", 
                                                          "lunda (northwestern)", 
                                                          "luvale",
                                                          "nyanja", 
                                                          "tonga"),
                                       other_level = "other"),
         language = ifelse(language == "lunda (luapula)"|language == "lunda (northwestern)", "lunda", as.character(language)))

# water supply - 
zmb_ipums_data %>% 
  dplyr::count(WATSUP)

# Don't use now, consider later if predictions are poor

# sanitation - 
zmb_ipums_data %>% 
  dplyr::count(SEWAGE)

# Easy enough to code, will use modified version
zmb_ipums_data <- 
  zmb_ipums_data %>%
  dplyr::mutate(sanitation = case_when(SEWAGE == 10 ~ "CONN",
                                       SEWAGE == 20 ~ "UNCONN",
                                       TRUE ~ "UNCONN"))


# Don't use now, consider later if predictions are poor


# religion 

zmb_ipums_data %>% 
  dplyr::count(RELIGIOND) %>% 
  arrange(desc(n))

zmb_ipums_data %>% 
  dplyr::count(RELIGIOND) %>% 
  arrange(desc(n))

# Combine religions to other, keep protestant, catholic, muslim
zmb_ipums_data <-
  zmb_ipums_data %>%
  dplyr::mutate(relig = haven::as_factor(RELIGIOND, levels = "label"),
                relig = tolower(relig),
                relig = stringr::word(relig, 1),
                relig = str_remove(relig, "\\,"),
                relig = forcats::fct_other(relig, keep = c("catholic", "protestant",
                                                           "muslim"), other_level = "other"))

# leave out for right now. only has Christian, nothing on Protestantism or Catholicism as in DHS.


# electricity
zmb_ipums_data %>% 
  dplyr::count(ELECTRIC)

zmb_ipums_data$electricity <- ifelse(zmb_ipums_data$ELECTRIC == 1, 1, 0)

# cell phone
zmb_ipums_data %>% 
  dplyr::count(CELL)

zmb_ipums_data$cellular <- ifelse(zmb_ipums_data$CELL == 1, 1, 0)

# internet
zmb_ipums_data %>% 
  dplyr::count(INTERNET)

zmb_ipums_data$internet <- ifelse(zmb_ipums_data$INTERNET == 2, 1, 0)


# age
table(zmb_ipums_data$AGE)

zmb_ipums_data$age <- haven::as_factor(zmb_ipums_data$AGE2, levels = "label")
zmb_ipums_data$age <- stringr::str_replace(zmb_ipums_data$age, " to ", "-")

zmb_ipums_data %>% dplyr::count(age)

zmb_ipums_data$district <- zmb_ipums_data$GEO2_ZM2010


# Create PSframe
library(sjmisc)
library(srvyr)
# establish survey characteristics

# PSframe
PSframe <- 
  zmb_ipums_data %>%
  dplyr::filter(SEX == 2) %>%
  dplyr::filter(AGE > 14 & AGE < 50) %>% # limit to females 15-49
  dplyr::count(district, age, language, electricity, cellular, relig, sanitation, internet)

# modify district vars in each file
PSframe$IPUM2010 <- sprintf("%06d",PSframe$district)

zmb_adm2$IPUM2010 <- gsub("ZM", "", zmb_adm2$ADM2_PCODE)

zmb_adm2$IPUM2010 <- sprintf("%06d", as.numeric(zmb_adm2$IPUM2010))

# convert to wide format - one district per row
PSfwide <- 
  PSframe %>%
  mutate(age = paste0("x", age),
         age = str_replace(age, "-", ".")) %>%
  distinct(., .keep_all = T) %>%
  pivot_wider(names_from = age:internet, values_from = n) %>%
  pivot_longer(-district:-IPUM2010) %>%
#  dplyr::select(-n_se) %>%
#  mutate(value = ifelse(is.na(value), 0, value)) %>%
  distinct_at(., vars(district, IPUM2010, name), .keep_all = T) %>%
  pivot_wider() %>%
  mutate_at(dplyr::vars(-"district", "IPUM2010"), ~replace(., is.na(.), 0))

# convert crs to planar format
library(sf)
zmb_adm2 <- st_transform(zmb_adm2, crs = 26915)
zmb_adm2_2010 <- st_transform(zmb_adm2_2010, crs = 26915)

# create vector for colnames that will be interpolated
psvarnames <- as.vector(names(PSfwide %>% 
                                dplyr::select(-district, -IPUM2010)))

# join PSframe (wide) to 2010 district shapefile
zmb_adm2_2010_psd <- 
  zmb_adm2_2010 %>% 
  left_join(PSfwide %>% dplyr::select(-district), by = "IPUM2010")

zmb_adm2_2010_psd <- zmb_adm2_2010_psd %>% dplyr::select(-CNTRY_NAME:-CNTRY_CODE, 
                                                         -DIST2010:-PARENT)

# structure mods
zmb_adm2_2010_psd <- st_as_sf(zmb_adm2_2010_psd)

# validate interpolation - do both formats work together?
library(areal)
ar_validate(source = zmb_adm2_2010_psd , target = zmb_adm2, 
            varList = psvarnames, method = "aw",  verbose = TRUE)
# interpolation
zmb_adm2_areal_intpol_psest  <- aw_interpolate(zmb_adm2, source = zmb_adm2_2010_psd, 
                       sid = "IPUM2010", 
               tid = "ADM2_PCODE", weight = "sum", 
               output = "tibble", 
               extensive = psvarnames)

# convert to adequate format 
PSframe_ar <- 
  zmb_adm2_areal_intpol_psest %>%
  pivot_longer(cols = x15.19_bemba_0_0_catholic_CONN_0:x45.49_tonga_1_1_protestant_UNCONN_1, 
               names_to = "strata", values_to = "n") %>%
  mutate(n = round(n, 0)) %>%
  separate(strata, sep = "_", into = c("age", "language", "electricity", "cellular",
                                       "relig", "sanitation", "internet")) %>%
  mutate(age = str_replace(age, "(?<=\\d)\\.", "-"),
         age = str_remove(age, "x"))

# write out shapefile
write.csv(PSframe_ar, file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/PSframe_ar.csv")


psfnames <- names(PSframe_ar)[c(17:23)]

psf_props <-list()

for (i in psfnames) {
  print(i)
  d <- PSframe_ar %>%
  dplyr::group_by(across(c(district, paste(i)))) %>%
  dplyr::tally(n) %>%
  dplyr::mutate(prop = n/sum(n)) %>%
  pivot_wider(-n, names_from = sym(i), values_from = prop)
  
  psf_props[[i]] <- d
  
}



psf_props <- psf_props %>% reduce(left_join, by = "district")

names(psf_props) <- c("district", "A15-19", "A20-24", "A25-29", "A30-34", "A35-39",
                      "A40-44", "A45-49", "bemba", "english", "kaonde", "lozi", "lunda",
                      "luvale", "nyanja", "other_lang", "tonga", "no_electricity", "yes_electricity",
                      "no_cell", "yes_cell", "catholic", "muslim", "other_rel", "protestant",
                      "conn_sewage", "unconn_sewage", "no_internet", "yes_internet")

write.csv(psf_props, "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/psf_props.csv")

