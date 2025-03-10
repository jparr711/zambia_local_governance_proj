library(haven)
library(tidyverse)
library(sjlabelled)
library(labelled) 
library(surveytoolbox) # install with devtools::install_github("martinctc/surveytoolbox")
library(ipumsr)
library(stringr)
library(standardize)
# C:\Users\jparr\OneDrive - DAI\Other\Zambia LIGA

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

# if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

# ddi <- read_ipums_ddi("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ipumsi_00001.dat")
# data <- read_ipums_micro(ddi)

# zmb_ipums <- read.table("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ipumsi_00001.dat", fill = T)

# FRAYM estimates
zmb_dist_risk <- read.csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/grid3_district_riskvars.csv")

# our data
zmb_dist <- read.csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/dist_zambia_liga.csv")

# PFM data
zmb_dist_debt <- read.csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zambia_liga_district_debt.csv")
# ward committees
zmb_dist_wscs <- read.csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/dist_zambia_liga_wsc.csv")

#DHS Estimates
zmb_dhs_land_2018 <- read_csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/DHS_estimates/zmb_dhs_land_2018.csv")
zmb_dhs_educ_2018 <- read_csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/DHS_estimates/zmb_dhs_educ_2018.csv")
zmb_dhs_stunting_2018 <- read_csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/DHS_estimates/zmb_dhs_stunting_2018.csv")

# Subset relevant data and join together
zmb_dhs_land_2018 <- 
  zmb_dhs_land_2018 %>% 
  select(c(1L:2L))


zmb_dhs_educ_2018 <- 
  zmb_dhs_educ_2018 %>% 
  select(c(1L:2L))


zmb_dhs_stunting_2018 <- 
  zmb_dhs_stunting_2018 %>% 
  select(c(1L:2L))

zmb_dhs_est_2018 <-
  zmb_dhs_educ_2018 %>%
  left_join(., zmb_dhs_land_2018) %>%
  left_join(., zmb_dhs_stunting_2018) 

# adjust district spellings to fit dist_risk dataset
zmb_dhs_est_2018 <-
zmb_dhs_est_2018 %>%
  dplyr::mutate(district = case_when(district == "Shiwamg'andu" ~ "Shiwan'gandu",
                                     district == "Lunte District" ~ "Lunte",
                                     district == "Milengi" ~ "Milenge",
                                     district == "Mushindano" ~ "Mushindamo",
                                     district == "Chiengi" ~ "Chienge",
                                     district == "Shang'ombo" ~ "Shangombo",
                                     district == "Chikankanta" ~ "Chikankata",
                                     TRUE ~ as.character(district)))

# join new dhs estimates to risk data
zmb_dist_risk <- 
  zmb_dist_risk %>% 
  dplyr::mutate(DistName = str_to_title(DistName)) %>% 
  dplyr::left_join(., zmb_dhs_est_2018, by = c("DistName" = "district"))


# clean names to match district data in zmb_dist data
zmb_dist_risk <- 
  zmb_dist_risk %>% 
  mutate(PovName = stringr::str_to_title(PovName),
                         DistName = stringr::str_to_title(DistName))


                         


zmb_dist %>% dplyr::distinct(District, .keep_all = T) %>% dplyr::count(Province)

zmb_dist <- 
  zmb_dist %>%
  dplyr::mutate(District = str_remove_all(District, " Town Council| Municipal Council| City Council| M Council"),
                Province = str_replace_all(Province, "Northwestern", "North-Western"))


zmb_dist <- zmb_dist %>% pivot_wider(names_from = "name", values_from = "value")


zmb_dist_wscs <- 
  zmb_dist_wscs %>% 
  dplyr::mutate(District = str_to_title(str_trim(District)),
                Province = str_to_title(str_trim(Province))) 

zmb_dist_debt <- 
  zmb_dist_debt %>% 
  dplyr::mutate(District = str_to_title(str_trim(District)),
                Province = str_to_title(str_trim(Province))) 


zmb_dist %>% 
  dplyr::mutate(District = str_trim(District),
                Province = str_trim(Province)) %>%
  anti_join(zmb_dist_risk, by = c("Province" = "PovName", "District" = "DistName")) %>%
  dplyr::distinct(District, Province) %>%
  as.data.frame()

zmb_dist_risk %>% 
  dplyr::mutate(District = str_trim(DistName),
                Province = str_trim(PovName)) %>%
  anti_join(zmb_dist, by = c("PovName" = "Province", "DistName" = "District")) %>%
  dplyr::distinct(DistName, PovName) %>%
  as.data.frame()


zmb_dist_wscs %>% 
  dplyr::mutate(District = str_trim(District),
                Province = str_trim(Province)) %>%
  anti_join(zmb_joined, by = c("Province" = "Province", "District" = "District_2")) %>%
  dplyr::distinct(District, Province) %>%
  as.data.frame()



zmb_dist_debt %>% 
  dplyr::mutate(District = str_to_title(str_trim(District)),
                Province = str_to_title(str_trim(Province))) %>%
  anti_join(zmb_dist_risk, by = c("Province" = "PovName", "District" = "DistName")) %>%
  dplyr::distinct(Province, District) %>%
  as.data.frame()


# Modify spelling in zmb_dist_risk to match data in zmb_dist
# itezhi-tezhi - in Central (as zmb_dist) or North-Western (as zmb_dist_risk)?
zmb_dist_risk %>% 
  dplyr::mutate(District = str_trim(DistName),
                Province = str_trim(PovName)) %>%
  mutate(DistName = case_when(DistName == "Itezhi-Tezhi" ~ "Itezhi tezhi",
                              DistName == "Shiwan'gandu" ~ "Shiwang'andu",
                              DistName == "Mushindamo" ~ "Mushindamo/East Solwezi",
                              DistName == "Kalumbila" ~ "Kalumbila/West Solwezi",
                              DistName == "Solwezi" ~ "Solwezi/Central Solwezi",
                              TRUE ~ as.character(DistName))) %>%
  anti_join(zmb_dist, by = c("PovName" = "Province", "DistName" = "District")) %>%
  dplyr::distinct(DistName, PovName) %>%
  as.data.frame()

# trim dist and prov names in zmb_dist
zmb_dist <-
zmb_dist %>% 
  dplyr::mutate(District = str_trim(District),
                Province = str_trim(Province))

# test joining
zmb_dist_risk %>% 
  dplyr::mutate(District = str_trim(DistName),
                Province = str_trim(PovName)) %>%
  mutate(DistName = case_when(DistName == "Itezhi-Tezhi" ~ "Itezhi tezhi",
                              DistName == "Shiwan'gandu" ~ "Shiwang'andu",
                              DistName == "Mushindamo" ~ "Mushindamo/East Solwezi",
                              DistName == "Kalumbila" ~ "Kalumbila/West Solwezi",
                              DistName == "Solwezi" ~ "Solwezi/Central Solwezi",
                              TRUE ~ as.character(DistName))) %>%
  anti_join(zmb_dist, by = c("PovName" = "Province", "DistName" = "District")) %>%
  dplyr::distinct(DistName, PovName) %>%
  as.data.frame()


zmb_joined <- 
  zmb_dist %>%
  mutate(District_2 = case_when(District == "Itezhi tezhi" ~ "Itezhi-Tezhi",
                                         District == "Shiwang'andu" ~ "Shiwan'gandu",
                                         District == "Mushindamo/East Solwezi" ~ "Mushindamo",
                                         District == "Kalumbila/West Solwezi" ~ "Kalumbila",
                                         District == "Solwezi/Central Solwezi" ~ "Solwezi",
                              TRUE ~ as.character(District))) %>%
  full_join(zmb_dist_risk, by = c("Province" = "PovName", "District_2" = "DistName"))

zmb_joined <-
  zmb_joined %>%
  dplyr::left_join(zmb_dist_debt, by = c("Province" = "Province", "District_2" = "District")) %>%
  dplyr::left_join(zmb_dist_wscs, by = c("Province" = "Province", "District_2" = "District"))
  


zmb_joined <- 
  zmb_joined %>%
  dplyr::select(-comments.x, -comments.y, -Nr, -code)


# add in new fin data

library(xlsx)

zmb_fin_data_new_cnt_much<- read.xlsx("C:\\Users\\jparr\\OneDrive - DAI\\Other\\Zambia LIGA\\Zambia District Financial Data 3-10.xlsx", sheetIndex = 1)

zmb_fin_data_new_cnt_much <-
zmb_fin_data_new_cnt_much %>%
  mutate(District. = stringr::str_remove(District., " Town Council| Municipal Council")) %>%
  dplyr::select(-Province)

zmb_joined <-
zmb_joined %>%
#  dplyr::filter(Province == "Central"|Province == "Muchinga") %>%
  dplyr::left_join(zmb_fin_data_new_cnt_much, by = c("District" = "District.")) 

# "2020.Local.Taxes"                 
# [10] "2020.Fees.and.Charges"             "2020.Other.Receipts"               "2020.LGEF"                        
# [13] "2020.Est.Revenue"                  "2020.%.of.revenue.raised.via.OSR"
# 
# "X2020.Local.Taxes"                 "X2020.Fees.and.Charges"           
# [46] "X2020.Other.Receipts."             "X2020.LGEF.and.CDF"                "X2020.Est.Revenue."

zmb_joined <-
zmb_joined %>%
  dplyr::mutate(`2020.Local.Taxes` = ifelse(Province == "Central|Muchinga", X2020.Local.Taxes, `2020.Local.Taxes`),
                `2020.Fees.and.Charges` = ifelse(Province == "Central|Muchinga", X2020.Fees.and.Charges, `2020.Fees.and.Charges`),
                `2020.Other.Receipts` = ifelse(Province == "Central|Muchinga", X2020.Other.Receipts., `2020.Other.Receipts`),
                `2020.LGEF` = ifelse(Province == "Central|Muchinga", X2020.LGEF.and.CDF, `2020.LGEF`),
                `2020.Est.Revenue` = ifelse(Province == "Central|Muchinga", X2020.Est.Revenue., `2020.Est.Revenue`)) %>%
  dplyr::select(-X2020.Local.Taxes:-NA.)

library("scales")
library(psych)

# create per capita scores
zmb_joined <- 
  zmb_joined %>%
  dplyr::mutate(osr_rate = `2020.%.of.revenue.raised.via.OSR`,
                lgef_per_inh = (`2020.LGEF`/Population.2019.projection),
                debt_per_inh = (total_debts/Population.2019.projection),
                stunt = stunt * 100,
                land = land * 100)


zmb_joined <- 
  zmb_joined %>%
  mutate_if(is.numeric, list(scale = function(x) scales::rescale(x,to = c(1, 5)), 
                             standard = as.numeric)) %>%
  mutate_at(dplyr::vars(Poverty.Incidence.3_scale,
                        regular_radio_listener_pct_scale:comm_risk_pct_scale, 
                        has_no_soap_pct_scale, 
                        has_no_drinkwater_pct_scale:WASH_risk_index_scale, stunt_scale, 
                        debt_per_inh_scale,
                        total_debts_scale), 
            function(x) scales::rescale(x,to = c(5, 1))) %>%
  mutate(Poverty.Incidence.3_standard = Poverty.Incidence.3_standard * 100,
         pct_wards_wdc_standard = pct_wards_wdc_standard * 100,
         `2020.%.of.revenue.raised.via.OSR_standard` = `2020.%.of.revenue.raised.via.OSR_standard` * 100) %>%
  select(!c(Population.2019.projection:`2020.%.of.revenue.raised.via.OSR`,
            regular_radio_listener_pct:WASH_risk_index,
            educ:stunt,
            supplies_contracts_loans:debt_per_inh,
            Shape__Area_standard,
            contains("OBJECTID_standard"),
            contains("OBJECTID_scale"),
            ProvNo_standard,
            ProvNo_scale,
            DistNo_standard,
            DistNo_scale,
            Shape__Area_scale, 
            Shape__Length_standard, 
            Shape__Length_scale))


zmb_joined <- 
  zmb_joined %>%
  dplyr::mutate(zmb_score1 = rowMeans(select(., `Population.2019.projection_scale`, 
                                             `lgef_per_inh_scale`, pct_wards_wdc_scale,
                                             debt_per_inh_scale,
                                             `2020.%.of.revenue.raised.via.OSR_scale`), na.rm = T),
                zmb_score2 =rowMeans(select(., Poverty.Incidence.3_scale, 
                                            stunt_scale,
                                            educ_scale,
                                            WASH_risk_index_scale, 
                                            female_literacy_pct_scale), na.rm = T),
                zmb_score_tot = (zmb_score1 + zmb_score2)/2)

zmb_joined <-
zmb_joined %>%
  rename_all(
    funs(
      stringr::str_to_lower(.) %>%
        stringr::str_replace_all(., '\\.', '_') %>%
        stringr::str_replace_all("2020_", "")
    )
  ) 


zmb_joined_long <- 
  zmb_joined %>%
  pivot_longer(cols = population_2019_projection_scale:zmb_score_tot, names_to = "category") %>%
  separate(category, into = c("category", "scoring"), "_(?=[^_]+$)")


zmb_joined_long <- 
zmb_joined_long %>%
  dplyr::mutate(category = case_when(scoring == "score1" ~ "zmb_score1",
                                     scoring == "score2" ~ "zmb_score2",
                                     scoring == "tot" ~ "zmb_score",
                                     TRUE ~ as.character(category)),
                scoring =  case_when(scoring == "score1" ~ "scale",
                                     scoring == "score2" ~ "scale",
                                     scoring == "tot" ~ "scale",
                                     TRUE ~ as.character(scoring)))




write_csv(zmb_joined_long, "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_joined_long.csv", na = "") 
write_csv(zmb_joined, "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_joined.csv", na = "") 

library("rjson")
library(geojsonio)
zmb_json <- rjson::fromJSON(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_adm_dmmu_20201124_shp/zmb_adm_dmmu_20201124.json")
library(sf)
library(rgdal)
library(ggmaps)

zmb_adm2 <- readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_adm_dmmu_20201124_shp/Shapefiles/zmb_admbnda_adm2_dmmu_20201124.shp")


shapefile_df <- fortify(zmb_adm2)



map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'white', size = .2) +
  theme_bw() +
  geom_text()





print(map)




topojson_write(zmb_adm2, "zmb_adm2.json")

st_write(zmb_adm2, "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_adm_dmmu_20201124_shp/zmb_adm2.json") 



#### dhis data

library(lodown)
library(survey)
library(haven)
library(vtable)

zmb_hh_dhs_2018 <- read_dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMHR71DT/ZMHR71FL.DTA")

zmb_br_dhs_2018 <- read_dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMBR71DT/ZMBR71FL.DTA")

zmb_kr_dhs_2018 <- read_dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMKR71DT/ZMKR71FL.DTA")

zmb_pr_dhs_2018 <- read_dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMPR71DT/ZMPR71FL.DTA")


zmb_br_dhs_2018$weight <- as.numeric( zmb_br_dhs_2018$v005 )

sum(zmb_br_dhs_2018)

svytotal( ~v201, zmb_br_dhs_2018)


vtable(zmb_br_dhs_2018)

vtable(zmb_kr_dhs_2018)


zmb_design <- 
  svydesign( 
    ~ v021 , 
    strata = ~v023 , 
    data = zmb_br_dhs_2018 , 
    weights = ~v005
  )

zmb_br_dhs_2018 %>%
  dplyr::count(v004)

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ipumsi_00002.xml")
data <- read_ipums_micro(ddi, verbose = F)

zmb_ipums <- data
rm(data)

zmb_ipums %>%
  dplyr::count(GEO2_ZM2010)

vtable(zmb_ipums)
vtable(zmb_br_dhs_2018)


zmb_br_dhs_2018 %>%
  dplyr::count(v141) %>% 
  as.data.frame()

zmb_shp <- readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03182021_05_156092/ZMGE71FL/ZMGE71FL.shp")


plot(zmb_shp)
