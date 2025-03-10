infix using "C:\COUNTRIES\ZM71\ZMFW71FL.dct"

label variable fw101    "Fieldworker code"
label variable fw000    "Country code and phase"
label variable fw102    "Fieldworker region of residence"
label variable fw103    "Fieldworker type of place of residence"
label variable fw104    "Fieldworker age"
label variable fw105    "Fieldworker sex"
label variable fw106    "Fieldworker marital status"
label variable fw107    "Fieldworker number of living children"
label variable fw108    "Fieldworker ever had a child who died"
label variable fw109    "Highest level of school attended by fieldworker"
label variable fw110    "Highest grade/form/year at that level completed by fieldworker"
label variable fw111    "Fieldworker religion"
label variable fw112    "Fieldworker ethnicity"
label variable fw113a   "English"
label variable fw113b   "Bemba"
label variable fw113c   "Kaonde"
label variable fw113d   "Lozi"
label variable fw113e   "Lunda"
label variable fw113f   "Luvale"
label variable fw113g   "Nyanja"
label variable fw113h   "Tonga"
label variable fw113x   "Other language"
label variable fw113y   "No other language"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115    "Has fieldworker ever worked on a DHS"
label variable fw116    "Has fieldworker ever worked on any other survey"
label variable fw117    "Was fieldworker working for agency at the time employed for DHS"
label variable fw118    "Is fieldworker a permanent or temporary employee of agency"

#delimit ;
label define FW102   
     1 "Central"
     2 "Copperbelt"
     3 "Eastern"
     4 "Luapula"
     5 "Lusaka"
     6 "Muchinga"
     7 "Northern"
     8 "North Western"
     9 "Southern"
    10 "Western"
    96 "Outside of Zambia"
    99 "MISSING"
;
label define FW103   
     1 "CIty"
     2 "Town"
     3 "Rural"
     9 "MISSING"
;
label define FW104   
    99 "MISSING"
;
label define FW105   
     1 "Male"
     2 "Female"
     9 "MISSING"
;
label define FW106   
     1 "Currently married"
     2 "Living with a man/woman"
     3 "Widowed"
     4 "Divorced"
     5 "Separated"
     6 "Never married or lived with a man/woman"
     9 "MISSING"
;
label define FW107   
    99 "MISSING"
;
label define FW108   
     1 "Yes"
     2 "No"
     9 "MISSING"
;
label define FW109   
     1 "Primary"
     2 "Secondary"
     3 "Higher"
     9 "MISSING"
;
label define FW110   
     0 "Less than one year at level"
    99 "MISSING"
;
label define FW111   
     1 "Catholic"
     2 "Protestant"
     3 "Muslim"
     6 "Other"
    99 "MISSING"
;
label define FW112   
     1 "African"
     2 "Indian"
    99 "MISSING"
;
label define FW114   
     1 "English"
     2 "Bemba"
     3 "Kaonde"
     4 "Lozi"
     5 "Lunda"
     6 "Luvale"
     7 "Nyanja"
     8 "Tonga"
     9 "Other"
    99 "MISSING"
;
label define FW115   
     1 "Yes"
     2 "No"
     9 "MISSING"
;
label define FW116   
     1 "Yes"
     2 "No"
     9 "MISSING"
;
label define FW117   
     1 "Yes, [Implementing agency 1]"
     2 "Yes, [Implementing agency 2]"
     3 "No"
     9 "MISSING"
;
label define FW118   
     1 "Permanent"
     2 "Temporary"
     9 "MISSING"
;

#delimit cr
label values fw102    FW102   
label values fw103    FW103   
label values fw104    FW104   
label values fw105    FW105   
label values fw106    FW106   
label values fw107    FW107   
label values fw108    FW108   
label values fw109    FW109   
label values fw110    FW110   
label values fw111    FW111   
label values fw112    FW112   
label values fw114    FW114   
label values fw115    FW115   
label values fw116    FW116   
label values fw117    FW117   
label values fw118    FW118   
