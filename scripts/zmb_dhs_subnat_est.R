### libraries ####

Start.time <- Sys.time()
library(spatstat)
require(rgdal)
require(maptools)
library(plyr)
require(rgeos)
require(splancs)
require(fields)
library(plyr)
library(foreign)
library(tidyverse)
library(readstata13)
library(RCurl)
library(Rcpp)
library(tidyverse)
library(xlsx)
library(RColorBrewer)
library(tidyverse)
library(ggstatsplot)
library(sjPlot)
library(psych)
library(broom)
library(dplyr)
library(data.table)
library(ggplot2)
library(sjmisc)
library(expss) 
library(tidybayes)
library(modelr)
require("ipumsr")
library(brms)
library(ggplot2)
library(scales)
library(corrplot)
library(MASS)
library(reshape2)
library(ggridges)
library(grid)
library(ggrepel)
library(countrycode)
library(reshape)
library(reshape2)
library(WDI)
library(ggthemes)
library(directlabels)
library(fuzzyjoin)
library(countrycode)
library(gganimate)
library(gifski)
library(Rtsne)
library(mice)
library(factoextra)
library(psycho)
library(stringr)
library(spatstat)
library(FNN)
library(gridExtra)
library(vtable)
library(haven)
library(sjlabelled)
library(lme4)
library(brms)
library(rstan)
# library(albersusa)
library(cowplot)
library(dtplyr)
library(disk.frame)
library(feather)
library(SRP)
library(vtable)

#### GEO SPATIAL #### -
### Meth C ####
#Location (Masked) of DHS clusters
EAPoints<-readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03182021_05_156092/ZMGE71FL/ZMGE71FL.shp")
proj4string(EAPoints)

#Zambia district boundaries
zmb_adm2 <- readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_adm_dmmu_20201124_shp/Shapefiles/zmb_admbnda_adm2_dmmu_20201124.shp")


#Use consistent projections
EAPoints<-spTransform(EAPoints, CRS("+proj=utm +zone=36 +datum=WGS84"))


zmb_adm2<-spTransform(zmb_adm2, CRS("+proj=utm +zone=36 +datum=WGS84"))

#Number of clusters/EAs
nEA <- dim(EAPoints)[[1]]

nEA

# Number of simulated points
nPoints <- 100


l<-0
for(i in 1:nEA) {
  cat("\n\n\n Cluster ",i,"\n") }



# Determining Polygon of Observed Point
#for(i in 1:nEA) {
ov_point<-as.data.frame(over(EAPoints, zmb_adm2))
dim(ov_point)
ov_point
given_dis <- as.character(ov_point$ADM2_EN)
given_dis


head(EAPoints)
EAOut <- as.data.frame(EAPoints)
EAOut$Mapped_district <- given_dis

as.character(EAOut$Mapped_district)


dim(EAOut)
head(EAOut)
names(EAOut)

EAOut <- EAOut[,c("DHSCLUST","Mapped_district")]
temp <- data.frame(  DHSCLUSTER = as.numeric(as.character(EAOut$DHSCLUST)),
                     district = as.character(EAOut$Mapped_district))

write.dta(temp, file="C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZMB_DHS_2018_MethC.dta")


#### Method A + B ####
#Location (Masked) of DHS clusters
EAPoints<-readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03182021_05_156092/ZMGE71FL/ZMGE71FL.shp")
proj4string(EAPoints)

#Zambia district boundaries
zmb_adm2 <- readOGR("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_adm_dmmu_20201124_shp/Shapefiles/zmb_admbnda_adm2_dmmu_20201124.shp")


#Use consistent projections
EAPoints<-spTransform(EAPoints, CRS("+proj=utm +zone=36 +datum=WGS84"))



zmb_adm2<-spTransform(zmb_adm2, CRS("+proj=utm +zone=36 +datum=WGS84"))


EAPoints@data <- 
  EAPoints@data %>%
  mutate(LATNUM = ifelse(SOURCE == "MIS", 0, LATNUM),
         LONGNUM = ifelse(SOURCE == "MIS", 0, LONGNUM))

#Number of clusters/EAs
nEA <- dim(EAPoints)[[1]]

nEA


offset<-ifelse(EAPoints$URBAN_RURA=="U", 2000, 5000)



set.seed(777)

l<-0

for(i in 1:nEA) {
  cat("\n\n\n Cluster ",i,"\n")
  
  if(EAPoints$LATNUM[i] != 0 & EAPoints$LONGNUM[i] !=0 ) {
    
    #Number of radii to use when approximating
    #probabilities for each district
    #Actual distance between radii depends on urban/rural area for each EA
    nradii <- 50
    
    #Whole country
    if(l==0) plot(zmb_adm2)
    
    #DHS Point
    points(EAPoints[i,],  pch=3, cex=.4, col="red")
    
    
    
    #Plot Buffer region
    pdsc<-disc(radius = offset[i], centre = c(coordinates(EAPoints)[i,1],coordinates(EAPoints)[i,2]))
    plot(pdsc, add=TRUE)
    
    
    
    radii <- (1:nradii)*(offset[i]/nradii)
    for(R in radii ){
      #For a given radius
      pdsc<-disc(radius = R, centre = c(coordinates(EAPoints)[i,1],coordinates(EAPoints)[i,2]))
      #Random points along circumference
      rpt<-runifpointOnLines(nPoints, edges(pdsc))
      rpt<-SpatialPoints(cbind(rpt$x, rpt$y))
      #Projections
      proj4string(rpt)<-CRS("+proj=utm +zone=36 +datum=WGS84")
      #Which districts are points mapped to
      ovRadius<-over(rpt, zmb_adm2)
      
      if(R ==radii[1])     ov <- ovRadius
      else ov <- rbind(ov, ovRadius)
    }
    
    ov<-ov[is.na(ov[,1])==0,]   #Removing Points Outside of Country
    
    
    #Calculate probabilities for each possible district
    prop<-data.frame(dhscluster= rep(EAPoints@data[i,"DHSCLUST"], length(unique(ov[,"ADM2_EN"])) ))
    prop$district <- ""
    prop$EstP <- NA
    prop$i <- i
    
    for(j in 1:length(unique(ov[,1]))){
      print(as.character(unique(ov[,"ADM2_EN"])[j]))
      #Proportion of Points in this District
      prop$EstP[j]<-mean(ov[,"ADM2_EN"]==unique(ov[,"ADM2_EN"])[j])
      #District Identifier
      prop$district[j]<-as.character(unique(ov[,"ADM2_EN"])[j])
    }
    
    
    print(prop)
    cat("\n")
    
    #Determining Polygon of Observed Point
    ov_point<-over(EAPoints[i,], zmb_adm2)
    given_dis <- ov_point["ADM2_EN"]
    
    print(given_dis)
    print(dim(given_dis))
    prop$Mapped.District <- rep(as.character(given_dis), dim(prop)[[1]])
    
    
    ##################################################################################################
    #The CPC way::: area-based
    
    pdscC<-disc(radius = offset[i], centre = c(coordinates(EAPoints)[i,1],coordinates(EAPoints)[i,2]))
    pdscC<-as(pdsc, "SpatialPolygons")
    proj4string(pdscC) <- CRS("+proj=utm +zone=36 +datum=WGS84")
    #Filling in the Buffer with Points
    #n_Approximation random points
    rptC<-csr(pdscC@polygons[[1]]@Polygons[[1]]@coords, nPoints)
    rptC<-SpatialPoints(rptC)
    proj4string(rptC)<-CRS("+proj=utm +zone=36 +datum=WGS84")
    ovC<-over(rptC, zmb_adm2)
    
    propC<-data.frame(dhscluster= rep(EAPoints@data[i,"DHSCLUST"], length(unique(ov[,"ADM2_EN"])) ))
    propC$district <- ""
    propC$EstP.CPC <- NA
    
    for(j in 1:length(unique(ov[,1]))){
      #Proportion of Points in each District
      propC$EstP.CPC[j]<-mean(ovC[,"ADM2_EN"]==unique(ovC[,"ADM2_EN"])[j])
      #District Identifier
      propC$district[j]<-as.character(unique(ovC[,"ADM2_EN"])[j])
    }
    
    
    
    #Record results for each cluster
    temp <- join(prop, propC)
    if(l==0) Out <- temp
    else {
      Out <- rbind(Out,temp)
    }
    
    rm(temp,prop,propC)
    l<-l+1
  }  #End if LAT and LONG measured
  
}

write.dta(Out, file="C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZMB_DHS_2018_MethA+B.dta")


#### Analysis ####
#### DHS prep ####


rm(list = ls())
# load in CLUSTER locations
zmb_methc <- read.dta(file="C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZMB_DHS_2018_MethC.dta")
zmb_metha_b <- read.dta(file="C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZMB_DHS_2018_MethA+B.dta")

# load in DHS survey

# ZMBR71DT - 38446 - all births
# ZMCR71DT - 5560 all couples
# ZMFW71DT - 156 rows? Don't know
# ZMHR71DT - 12831 - all households
# ZMIR71DT - 13683 - all women 15-49
# ZMKR71DT - 9959 - all children under 60 months 
# ZMMR71DT - 12132 - all men 15-49
# ZMPR71DT - 65454 - all household members

# LLoad in datasets

zmb_mr_dhs_2018 <- read.dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMMR71DT/ZMMR71FL.DTA")

#
zmb_ir_dhs_2018 <- read.dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMIR71DT/ZMIR71FL.DTA")


# zmb_pr_dhs_2018 <- read.dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMPR71DT/ZMPR71FL.DTA")


# zmb_br_dhs_2018 <- read.dta(file = "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/ZM_2018_DHS_03152021_2223_156092/ZMBR71DT/ZMBR71FL.DTA")



# vtable(zmb_ir_dhs_2018)
# naive estimate on Meth C
# join
zmb_mr_dhs_2018 <- 
  zmb_mr_dhs_2018 %>%
  dplyr::left_join(., zmb_methc, by = c("mv001" = "DHSCLUSTER"))


zmb_ir_dhs_2018 <- 
  zmb_ir_dhs_2018 %>%
  dplyr::left_join(., zmb_methc, by = c("v001" = "DHSCLUSTER"))



# response freq
zmb_ir_dhs_2018 %>%
  dplyr::count(v155)

zmb_mr_dhs_2018 %>%
  dplyr::count(mv155)

# lit var
# females
zmb_ir_dhs_2018$lit <- ifelse(zmb_ir_dhs_2018$v155 == 'able to read only parts of sentence'|zmb_ir_dhs_2018$v155 =='able to read whole sentence', 1, NA)
zmb_ir_dhs_2018$lit <- ifelse(zmb_ir_dhs_2018$v155 =='cannot read at all', 0, zmb_ir_dhs_2018$lit)

# males
zmb_mr_dhs_2018$lit <- ifelse(zmb_mr_dhs_2018$mv155 == 'able to read only parts of sentence'|zmb_mr_dhs_2018$mv155 =='able to read whole sentence', 1, NA)
zmb_mr_dhs_2018$lit <- ifelse(zmb_mr_dhs_2018$mv155 =='cannot read at all', 0, zmb_mr_dhs_2018$lit)



# naive female literacy rate for clusters
zmb_ir_dhs_2018 %>%
  dplyr::group_by(district) %>%
  dplyr::summarize(n(), sum(v155 == 'able to read only parts of sentence'|
                         v155 =='able to read whole sentence')/
                     sum(v155 == 'able to read only parts of sentence'|v155 =='able to read whole sentence'|v155 =='cannot read at all')) %>%
  as.data.frame()  %>%
  head(10L)
# Total 65% literacy among WOMEN - unweighted

# naive male literacy rate (15-49) for clusters
zmb_mr_dhs_2018 %>%
  dplyr::group_by(district) %>%
  dplyr::summarize(n(), sum(mv155 == 'able to read only parts of sentence'|mv155 =='able to read whole sentence')/
                     sum(mv155 == 'able to read only parts of sentence'|mv155 =='able to read whole sentence'|mv155 =='cannot read at all')) %>%
  as.data.frame() %>%
  head(10L)
# Total 81% literacy among MEN - unweighted


# what would we want to include as covariates?
## - AGE - v012
# - URBAN/RURAL - v102
## - LANGUAGE - V045C OR V045 (lang of interview)
# - EDUCATION - V106, V149
## - ELECTRICITY - V119
## - RELICION - V130 - NO. DOES NOT MATCH in IPUMS
## - MOBILE - V169A
## - INTERNET - V171A
# BANK ACCT - V170
# WEALTH (INDEX) - V190
# - Water Supply - V113
## - SANITATION - v116

# age - 

zmb_ir_dhs_2018$age <- zmb_ir_dhs_2018$v013

zmb_mr_dhs_2018$age <- zmb_mr_dhs_2018$mv013


# language
zmb_ir_dhs_2018 %>%
  dplyr::count(v045c)

zmb_ir_dhs_2018$language <- zmb_ir_dhs_2018$v045c

zmb_mr_dhs_2018$language <- zmb_mr_dhs_2018$mv045c




# wat sup
zmb_ir_dhs_2018 %>%
  dplyr::count(v113)

# sanit
zmb_ir_dhs_2018 %>%
  dplyr::count(v116)

# simplify to match IPUMS
zmb_ir_dhs_2018 <- 
zmb_ir_dhs_2018 %>%
  dplyr::mutate(sanitation = case_when(v116 == "flush to piped sewer system" ~ "CONN",
                                       v116 == 'flush to septic tank' ~ "CONN",
                                       v116 == 'flush to somewhere else' ~ "CONN",
                                       v116 == 'not a dejure resident' ~ "UNCONN",
                                       TRUE ~ "UNCONN"))

# MEN's RECODE does not have sanitation, must get elsewhere. HH survey?




# religion
zmb_ir_dhs_2018 %>%
  dplyr::count(v130)

zmb_ir_dhs_2018$relig <- zmb_ir_dhs_2018$v130

zmb_mr_dhs_2018$relig <- zmb_mr_dhs_2018$v130


# urb/rur
zmb_ir_dhs_2018 %>% 
  dplyr::count(v102)

# educ
zmb_ir_dhs_2018 %>%
  dplyr::count(v106)

# elec
zmb_ir_dhs_2018 %>%
  dplyr::count(v119)

zmb_ir_dhs_2018$electricity <- ifelse(zmb_ir_dhs_2018$v119 == "yes", 1, 0)

# zmb_mr_dhs_2018$electricity <- ifelse(zmb_mr_dhs_2018$mv119 == "yes", 1, 0)

# internet
zmb_ir_dhs_2018 %>%
  dplyr::count(v171a)

zmb_ir_dhs_2018 %>%
  dplyr::count(v171a)

# zmb_mr_dhs_2018 %>%
#   dplyr::count(v171a)
# not in MEN's RECODE

# modified to match IPUMS use
zmb_ir_dhs_2018$internet <- ifelse(zmb_ir_dhs_2018$v171a == "never", 0, 1)

# zmb_mr_dhs_2018$internet <- ifelse(zmb_mr_dhs_2018$v171a == "never", 0, 1)


# cell
zmb_ir_dhs_2018 %>%
  dplyr::count(v169a)

zmb_mr_dhs_2018 %>%
  dplyr::count(mv169a)


zmb_ir_dhs_2018$cellular <- ifelse(zmb_ir_dhs_2018$v169a == "yes", 1, 0)

zmb_mr_dhs_2018$cellular <- ifelse(zmb_mr_dhs_2018$mv169a == "yes", 1, 0)

# rural
zmb_ir_dhs_2018 <- 
  zmb_ir_dhs_2018 %>% 
  separate(v022, into = c("province", "urb_rur"), sep = " - ") %>%
  dplyr::mutate(rural = ifelse(urb_rur == "rural", 1, 0))

zmb_ir_dhs_2018$DHSCLUSTER <- zmb_ir_dhs_2018$v001

#### Bring in PS frame and run regressions ####
# Post-stratification frame for current districts
PSframe_ar <- read.csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/PSframe_ar.csv")


# Women's literacy

zmb_ir_dhs_2018_cut <- zmb_ir_dhs_2018 %>% dplyr::select(DHSCLUSTER, district, province, age, 
                                  language, electricity, cellular, rural,
                                  relig, sanitation, internet, age, lit)

# naive model
femlit_MRmodel_IO <-
  brm(lit ~ 1 + (1 | district),
      data   = zmb_ir_dhs_2018_cut, 
      warmup = 1000, 
      iter   = 3000, 
      chains = 2, 
      inits  = "random",
      cores  = 2)  #the cores function tells STAN to make use of 2 CPU cores simultaneously instead of just 1.

summary(femlit_MRmodel_IO)

# individual model with covariates - random effects at district.
femlit_MRmodel <-
  brm(lit ~ 1 + (1 | district) + factor(rural) + factor(province)
      + factor(language) + factor(electricity) + factor(cellular)
      + factor(relig) + factor(sanitation) + factor(internet) + factor(age),
      data = zmb_ir_dhs_2018_cut,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(1), class = sd)),
      inits = "random",
      iter = 3000, warmup = 1000, chains = 4, cores = 4)

# individual model with covariates
femlit_MRmodel_DHSclus <-
  brm(lit ~ 1 + (1 | DHSCLUSTER) + factor(rural) +
      + factor(language) + factor(electricity) + factor(cellular)
      + factor(relig) + factor(sanitation) + factor(internet) + factor(age),
      data = zmb_ir_dhs_2018_cut,
      prior = c(prior(normal(0, 0.2), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(1), class = sd)),
      inits = "random",
      iter = 3000, warmup = 1000, chains = 4, cores = 4)


summary(femlit_MRmodel)

femlit_MRmodel %>%
  gather_samples(`sd_.*`, regex=TRUE) %>%
  ungroup() %>%
  mutate(group = stringr::str_replace_all(term, c("sd_" = "","__Intercept"=""))) %>%
  ggplot(aes(y=group, x = estimate)) + 
  ggridges::geom_density_ridges(aes(height=..density..),
                                rel_min_height = 0.01, stat = "density",
                                scale=1.5) 

get_variables(femlit_MRmodel)

femlit_MRmodel$prior



# b_Intercept
# r_district

femlit_MRmodel %>%
  spread_draws(r_district[district])

femlit_MRmodel_IO %>% 
  spread_draws(b_Intercept, r_district[district]) %>%
  mean_qi(condition_mean = b_Intercept + r_district)



femlit_MRmodel %>%
  spread_draws(b_Intercept, r_district[district]) %>%
  median_qi(condition_mean = b_Intercept + r_district, .width = c(.95, .66)) %>%
  ggplot(aes(y = district, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() 

# extract district estimates
post_lit <- 
  coef(femlit_MRmodel, robust = T)$district[, , ] %>% 
  as.data.frame() %>%
  rownames_to_column("district")
  

names(post_lit)[1:5] <- c("district", "post_lit_MR_est", "post_lit_MR_err", "post_lit_MR_lower", "post_lit_MR_upper") 

post_lit_red <- (post_lit)[, c(1:5)]

post_lit_red <- post_lit_red %>% 
  mutate_if(is.numeric, inv_logit_scaled)

head(post_lit_red, 20L)

# empty model
post_lit_IO <- 
  coef(femlit_MRmodel_IO, robust = T)$district[, , ] %>% 
  as.data.frame() %>%
  rownames_to_column("district")


names(post_lit_IO)[1:5] <- c("district", "post_lit_IO_est", "post_lit_IO_err", "post_lit_IO_lower", "post_lit_IO_upper") 

post_lit_IO <- (post_lit_IO)[, c(1:5)]

post_lit_IO <- post_lit_IO %>% 
  mutate_if(is.numeric, inv_logit_scaled)

head(post_lit_IO)


post_lit_tot <- post_lit_red %>%
  left_join(post_lit_IO)

post_lit_tot %>%
  mutate(pct_diff = post_lit_MR_est - post_lit_IO_est) %>%
  dplyr::select(district, pct_diff, post_lit_MR_est, post_lit_IO_est) %>%
  arrange(desc(pct_diff))


post_lit_tot %>%
  pivot_longer(-district) %>%
  separate(name, into = c("model", "estimate"), sep="_(?=[^_]+$)") %>%
  mutate(model = str_remove(model, "post_lit_")) %>%
  pivot_wider(names_from = estimate, values_from = value) %>%
  ggplot(aes(y = district, x = est, xmin = lower, xmax = upper, color = model)) +
  geom_pointinterval() 
  

# launch_shinystan(femlit_MRmodel)
# launch_shinystan(femlit_MRmodel_IO)

waic(femlit_MRmodel)
# Estimate    SE
# elpd_waic  -7355.4  59.1
# p_waic       124.9   1.3
# waic       14710.9 118.1

waic(femlit_MRmodel_IO)

approx_sd <- broom::tidy(femlit_MRmodel) %>%
  filter(stringr::str_detect(term, "sd_"))

femlit_MRmodel %>%
  gather_samples(`sd_.*`, regex=TRUE) %>%
  ungroup() %>%
  mutate(group = stringr::str_replace_all(term, c("sd_" = "","__Intercept"=""))) %>%
  ggplot(aes(y=group, x = estimate)) + 
  ggridges::geom_density_ridges(aes(height=..density..),
                                rel_min_height = 0.01, stat = "density",
                                scale=1.5) + 
  geom_point(data=approx_sd)


# MLE Model

# femlit_MLEmodel <- lme4::lmer(as.numeric(as.character(lit)) ~ 1 + (1 | age) + (1 | language) + (1 | electricity) + (1 | cellular)
#                               + (1 | relig) + (1 | sanitation) + (1 | internet) + (1 | district), 
#                               data = zmb_ir_dhs_2018)
# 

PSframe_ar$district <- PSframe_ar$ADM2_EN
PSframe_ar$province <- tolower(PSframe_ar$ADM1_EN)

PSframe_ar$province <- ifelse(PSframe_ar$province == 'north-western', 'north western', PSframe_ar$province)
PSframe_ar$province <- ifelse(PSframe_ar$province == 'copperbelt', 'copperbel', PSframe_ar$province)

PSframe_ar %>%
  dplyr::group_by(ADM1_EN, age) %>%
  dplyr::tally(n) %>%
  mutate(n/sum(n)) %>%
  as.data.frame()

PSframe_arr <- 
  zmb_ir_dhs_2018 %>% 
   # separate(v022, into = c("province", "urb_rur"), sep = " - ") %>%
    # left_join(zmb_methc, by = c("v001" = "DHSCLUSTER")) %>% 
    ungroup() %>%
    dplyr::select(rural, district) %>%
    dplyr::group_by(district) %>% 
    dplyr::count(rural) %>%
  #  dplyr::select(-n) %>% 
    distinct() %>% 
    dplyr::group_by(district) %>%
    dplyr::summarize(rural = n/sum(n)) %>%
    distinct(district, .keep_all = T) %>%
    as.data.frame() %>%
    dplyr::mutate(rural = ifelse(rural == 1, 1, 0)) %>%
    right_join(PSframe_ar)

PSframe_arr %>%
  dplyr::group_by(ADM1_EN, age) %>%
  dplyr::tally(n) %>%
  mutate(n/sum(n)) %>%
  as.data.frame()


PSframe_arr$electricity <- as.numeric(as.character(PSframe_arr$electricity))
PSframe_arr$cellular <-    as.numeric(as.character(PSframe_arr$cellular))
# PSframe_arr$sanitation <-  as.numeric(as.character(PSframe_arr$sanitation))
PSframe_arr$internet <-    as.numeric(as.character(PSframe_arr$internet))
# PSframe_arr$language <-    as.numeric(as.character(PSframe_arr$language))
# PSframe_arr$relig <-       as.numeric(as.character(PSframe_arr$relig))
# PSframe_arr$age <-         as.numeric(as.character(PSframe_arr$age))

# PSframe_arr <- PSframe_arr %>% dplyr::filter(n != 0)

# MR model Post-stratified
# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe. 
epred_mat <- posterior_epred(femlit_MRmodel, 
                              newdata = PSframe_arr, 
                             ndraws = 3000, allow_new_levels = T)

epred_mat[is.na(epred_mat)] <- 0


mrp_estimates_vector <- 
  epred_mat %*% 
  matrix(PSframe_arr$n / sum(PSframe_arr$n))

mrp_estimate <- c(mean = mean(mrp_estimates_vector), 
                  sd = sd(mrp_estimates_vector))

cat("MRP estimate mean, sd: ", round(mrp_estimate, 3))

# empty dataframe for ps estimates
districts_df <- data.frame(
  district = unique(PSframe_arr$district),
  mrp_estimate = NA,
  mrp_estimate_se = NA
)

# Loop to populate the dataframe
for(i in 1:nrow(districts_df)) {
  # Currently, the matrix epred_mat and the poststratification table contain 12,000
  # rows. We need to filter the ones that correspond to state in row i. We do so 
  # by defining the following condition:
  filtering_condition <- which(PSframe_arr$district == districts_df$district[i])
  
  # Filtering matrix epred_mat with filtering_condition
  districts_epred_mat <- epred_mat[ ,filtering_condition]
  
  # Filtering poststratification table with filtering_condition
  k_filtered <- PSframe_arr[filtering_condition, ]$n
  
  # Poststratification step
  mrp_estimates_vector <- districts_epred_mat %*% k_filtered / sum(k_filtered)
  
  # MRP estimate for state in row i 
  districts_df$mrp_estimate[i] <- mean(mrp_estimates_vector)
  districts_df$mrp_estimate_se[i] <- sd(mrp_estimates_vector)
}

# compare MRP estimates with non-MRP estimates from femlit_MRmodel

full_estimates_df <- 
districts_df %>% 
  left_join(post_lit_tot)
  
ggplot(full_estimates_df, aes(x = post_lit_MR_est, y = mrp_estimate )) +
  geom_point() +
  geom_abline()
  
full_estimates_df[, c(1:2, 4, 8)]

# compare results with FRAYM estimates
# FRAYM estimates
zmb_dist_risk <- read.csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/grid3_district_riskvars.csv")

full_estimates_df <-
full_estimates_df %>%
  dplyr::mutate(district = case_when(district == "Shiwamg'andu" ~ "Shiwan'gandu",
                                     district == "Lunte District" ~ "Lunte",
                                     district == "Milengi" ~ "Milenge",
                                     district == "Mushindano" ~ "Mushindamo",
                                     district == "Chiengi" ~ "Chienge",
                                     district == "Shang'ombo" ~ "Shangombo",
                                     district == "Chikankanta" ~ "Chikankata",
                                     TRUE ~ as.character(district)))

fe_df <- 
  zmb_dist_risk %>% 
  dplyr::mutate(DistName = str_to_title(DistName)) %>% 
  dplyr::select(ProvNo:DistName, female_literacy_pct) %>%
  dplyr::right_join(., full_estimates_df, by = c("DistName" = "district"))

# MSE, RMSE
fe_df <- 
fe_df %>%
  dplyr::mutate(fraym_mrp_err_sq = ((female_literacy_pct/100) - mrp_estimate)^2,
                fraym_mr_err_sq = ((female_literacy_pct /100) - post_lit_MR_est)^2,
                fraym_io_err_sq = ((female_literacy_pct/100) - post_lit_IO_est)^2)
  
fe_df %>%
  dplyr::filter(!is.na(post_lit_MR_est)) %>%
  dplyr::summarize(n = n(),
                   rmse_fraym_mrp = sqrt(sum(fraym_mrp_err_sq, na.rm = T) / n()),
                   rmse_fraym_mr = sqrt(sum(fraym_mr_err_sq, na.rm = T) / n()),
                   rmse_fraym_io = sqrt(sum(fraym_io_err_sq, na.rm = T) / n()))


fe_df %>%
  dplyr::mutate(fraym_mrp_err_sq = ((female_literacy_pct/100) - mrp_estimate)^2,
                fraym_mr_err_sq = ((female_literacy_pct /100) - post_lit_MR_est)^2,
                fraym_io_err_sq = ((female_literacy_pct/100) - post_lit_IO_est)^2) %>%
  dplyr::filter(!is.na(post_lit_MR_est)) %>%
  dplyr::select(contains("err_sq"), DistName, PovName) %>%
  pivot_longer(cols = fraym_mrp_err_sq:fraym_io_err_sq) %>%
  ggplot(., aes(x = DistName, y = value, color = name)) +
  geom_point() + 
  facet_wrap(. ~ PovName,  scales="free") +
  coord_flip()

write_csv(fe_df, "full_estimates_df.csv")

zmbdata <- read_csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/zmb_joined.csv") 

fe_df %>% 
  left_join(zmbdata, by = c("DistNo" = "distno")) %>%
  dplyr::mutate(fraym_mrp_err_sq = ((female_literacy_pct/100) - mrp_estimate)^2,
                fraym_mr_err_sq = ((female_literacy_pct /100) - post_lit_MR_est)^2,
                fraym_io_err_sq = ((female_literacy_pct/100) - post_lit_IO_est)^2) %>%
  dplyr::filter(!is.na(post_lit_MR_est)) %>%
  dplyr::select(contains("err_sq"), DistName, DistNo, personal_emoluments_scale,
                contains("population"), PovName) %>%
  View(.)
# coord_flip()
  
  
  

options(scipen = 999)
fe_df %>%
  dplyr::mutate(fraym_mrp_err_sq = ((female_literacy_pct/100) - mrp_estimate)^2,
                fraym_mr_err_sq = ((female_literacy_pct /100) - post_lit_MR_est)^2,
                fraym_io_err_sq = ((female_literacy_pct/100) - post_lit_IO_est)^2) %>%
  dplyr::filter(!is.na(post_lit_MR_est)) %>%
  dplyr::select(contains("err_sq"), DistName, PovName) %>%
  dplyr::group_by(PovName) %>%
  dplyr::summarize(fraym_mrp_err_sq = mean(fraym_mrp_err_sq), 
                   fraym_mr_err_sq = mean(fraym_mr_err_sq), 
                   fraym_io_err_sq = mean(fraym_io_err_sq), 
                   )

# MR DHS Cluster model aggregation
# install.packages("unix") 

zmb_metha <- zmb_metha_b[, c(1:3)]

PSframe_arr <- 
  PSframe_arr %>% 
  left_join(zmb_metha, by = "district")


epred_mat <- posterior_epred(femlit_MRmodel_DHSclus, 
                             newdata = PSframe_arr, 
                             ndraws = 2000, allow_new_levels = T)

epred_mat[is.na(epred_mat)] <- 0


mrp_estimates_vector <- 
  epred_mat %*% 
  matrix(PSframe_arr$n / sum(PSframe_arr$n))

mrp_estimate <- c(mean = mean(mrp_estimates_vector), 
                  sd = sd(mrp_estimates_vector))

cat("MRP estimate mean, sd: ", round(mrp_estimate, 3))

# empty dataframe for ps estimates
districts_df <- data.frame(
  district = unique(PSframe_arr$district),
  mrp_estimate = NA,
  mrp_estimate_se = NA
)

# Loop to populate the dataframe
for(i in 1:nrow(districts_df)) {
  # Currently, the matrix epred_mat and the poststratification table contain 12,000
  # rows. We need to filter the ones that correspond to state in row i. We do so 
  # by defining the following condition:
  filtering_condition <- which(PSframe_arr$district == districts_df$district[i])
  
  # Filtering matrix epred_mat with filtering_condition
  districts_epred_mat <- epred_mat[ ,filtering_condition]
  
  # Filtering poststratification table with filtering_condition
  k_filtered <- PSframe_arr[filtering_condition, ]$n
  
  # Poststratification step
  mrp_estimates_vector <- districts_epred_mat %*% k_filtered / sum(k_filtered)
  
  # MRP estimate for state in row i 
  districts_df$mrp_estimate[i] <- mean(mrp_estimates_vector)
  districts_df$mrp_estimate_se[i] <- sd(mrp_estimates_vector)
}


# our data
zmb_dist <- read.csv("C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/dist_zambia_liga.csv")

zmb_dist <- 
  zmb_dist %>%
  dplyr::mutate(District = str_remove_all(District, " Town Council| Municipal Council| City Council| M Council"),
                Province = str_replace_all(Province, "Northwestern", "North-Western"))


zmb_dist <- zmb_dist %>% pivot_wider(names_from = "name", values_from = "value")

zmb_dist %>%
  dplyr::left_join(full_estimates_df, by = c("District" = "DistName")) %>%
  ggplot(., aes(x = Population.2019.projection, y = fraym_io_err_sq)) +
  geom_point()

zmb_dist %>%
  dplyr::left_join(full_estimates_df, by = c("District" = "DistName")) %>%
  ggplot(., aes(x = Population.2019.projection, y = fraym_mrp_err_sq)) +
  geom_point()


zmb_dist %>%
  dplyr::left_join(full_estimates_df, by = c("District" = "DistName")) %>%
  ggplot(., aes(x = Population.2019.projection, y = fraym_mr_err_sq)) +
  geom_point()

PSframe

# check if estimate error is correlated with population 


#### using DHS rates package #####
# define complex survey def
dhs_design <- 
  svydesign( 
    ~ v021 , 
    strata = ~strata , 
    data = dhs_df , 
    weights = ~weight
  )



dhs_design <- 
  update( 
    dhs_design , 
    
    one = 1 ,
    
    total_children_ever_born = v201 ,
    
    surviving_children = v201 - v206 - v207 ,
    surviving_children_rate = (v201 - v206 - v207)/v201,
    education_level = v106,
    child_death_rate = ifelse(b5 == 0, 1, 0),
    urban_rural = factor( v025 , labels = c( 'urban' , 'rural' )),
    district = factor(district),
    visit_health_fact = v394,
    ethnicity =
      factor( v131 , levels = c( 1:8 , 96 ) , labels =
                c( "Chewa" , "Tumbuka" , "Lomwe" , "Tonga" , 
                   "Yao" , "Sena" , "Nkonde" , "Ngoni" , "Other" ) ) ,
    
    no_formal_education = as.numeric( v149 == 0 )
    
  )

# unweighted counts
sum( weights( dhs_design , "sampling" ) != 0 )

svyby( ~ one , ~ district , dhs_design , unwtd.count )

# weighted counts
svytotal( ~ one , dhs_design )

svyby( ~ one , ~ urban_rural , dhs_design , svytotal )


svymean( ~ surviving_children_rate ,  dhs_design )

svymean( ~ child_death_rate ,  dhs_design )


svyby( ~ child_death_rate , ~ district , dhs_design , svymean, vartype=c("se","ci","ci","cv","cvpct","var") ) * 1000

svyby( ~ education_level, ~ district , dhs_design , svymean )

mean(zmb_br_dhs_2018$v394)

library(DHS.rates)


dhs_df %>%
#  dplyr::filter(!is.na(district)) %>%
  chmort(., JK = "Yes", PeriodEnd = "2018-12")


dhs_df$hw12[c(0:50)]

svyratio( 
  numerator = ~ surviving_children , 
  denominator = ~ total_children_ever_born , 
  dhs_design 
)

# stunting

zmb_und5_dhs_2018 <- zmb_pr_dhs_2018[!is.na(zmb_pr_dhs_2018$hc70) & zmb_pr_dhs_2018$hc70<9000,]

# making a variable for stunting
zmb_und5_dhs_2018$stunt <- ifelse(zmb_und5_dhs_2018$hc70 < -200, 1, 0)


zmb_und5_dhs_2018 <- zmb_und5_dhs_2018 %>%
  dplyr::left_join(., temp, by = c("hv004" = "DHSCLUSTER"))

# create weight variable
zmb_und5_dhs_2018$wt <- zmb_und5_dhs_2018$hv005/1000000

# survey settings
data.w <- svydesign(id = ~ hv001, 
                    district = factor(district),
                    data = zmb_und5_dhs_2018, 
                    weight = ~ wt, 
                    strata = ~ hv022)

# tabulate stunting by sex
zmb_dhs_stunting_2018 <- svyby(~stunt, ~ district, data.w, svymean, vartype=c("se","ci","ci","cv","cvpct","var") )


# education

zmb_adult_dhs_2018 <-
  zmb_pr_dhs_2018 %>% 
  dplyr::filter(hv105 > 15 & hv105 < 49)

# remove NAs and non-applicables
zmb_adult_dhs_2018 <- zmb_adult_dhs_2018[!is.na(zmb_adult_dhs_2018$hv106) & zmb_adult_dhs_2018$hv106<8,]

zmb_adult_dhs_2018$educ <- as.numeric(zmb_adult_dhs_2018$hv106)

# create weight variable
zmb_adult_dhs_2018$wt <- zmb_adult_dhs_2018$hv005/1000000

# join district data
zmb_adult_dhs_2018 <- zmb_adult_dhs_2018 %>%
  dplyr::left_join(., temp, by = c("hv004" = "DHSCLUSTER"))


# survey settings
data.e <- svydesign(id = ~ hv001, 
                    district = factor(district),
                    data = zmb_adult_dhs_2018, 
                    weight = ~ wt, 
                    strata = ~ hv022)

# tabulate stunting by sex
zmb_dhs_educ_2018 <- svyby(~educ, ~ district, na.rm = T,
                           data.e, svymean, vartype=c("se","ci","ci","cv","cvpct","var") )

zmb_dhs_educ_2018


# land
zmb_landsubset_dhs_2018 <-
  zmb_br_dhs_2018 

# remove NAs and non-applicables
zmb_landsubset_dhs_2018 <- zmb_landsubset_dhs_2018[!is.na(zmb_landsubset_dhs_2018$v745a),]

zmb_landsubset_dhs_2018$land <- ifelse(zmb_landsubset_dhs_2018$v745a == 0, 0, 1)
zmb_landsubset_dhs_2018$land <- as.numeric(zmb_landsubset_dhs_2018$land)

# create weight variable
zmb_landsubset_dhs_2018$wt <- zmb_landsubset_dhs_2018$v005/1000000

# join district data
zmb_landsubset_dhs_2018$district = zmb_landsubset_dhs_2018$district


# survey settings
data.l <- svydesign(id = ~caseid, 
                    district = factor(district),
                    data = zmb_landsubset_dhs_2018, 
                    weight = ~ wt, 
                    strata = ~ v023)

# tabulate stunting by sex
zmb_dhs_land_2018 <- svyby(~land, ~ district, na.rm = T,
                           data.l, svymean, vartype=c("se","ci","ci","cv","cvpct","var") )

zmb_dhs_land_2018


write_csv(zmb_dhs_land_2018, "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/DHS_estimates/zmb_dhs_land_2018.csv", na = "")
write_csv(zmb_dhs_educ_2018, "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/DHS_estimates/zmb_dhs_educ_2018.csv", na = "")
write_csv(zmb_dhs_stunting_2018, "C:/Users/jparr/OneDrive - DAI/Other/Zambia LIGA/DHS_estimates/zmb_dhs_stunting_2018.csv", na = "")


