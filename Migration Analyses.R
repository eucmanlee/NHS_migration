########################################################
#                                                      #
# Analyses on trust SMR data + ONS migration data      #
# Areas and NHS Trusts (Forti E., Lee E. 2015)         #
#                                                      # 
########################################################

### Clear workspace (remove all objects!)

rm(list = ls())

# ### Load required libraries ---------------------------------------------

# install.packages("maps")
# install.packages("mapdata")
# install.packages("plm")
# install.packages("sqldf")
# install.packages("ggmap")
# install.packages("foreign")
# install.packages("DataCombine")
library(fields)       # Computes geodesic distances
library(googlesheets) # Support for Google Sheets
library(stringr)      # String operations
library(magrittr)     # Pipe operators
library(dplyr)        # Data wrangling
library(tidyr)        # Data cleaning
library(plm)          # Panel data model
library(sqldf)        # Data 
library(foreign)      # Data Analysis
library(ggplot2)      # Graphics and charts
library(DataCombine)  # Easy to lag variables

# library(maps)       # Provides functions that let us plot the maps
# library(mapdata)    # Hi-resolution points that mark out the countries.
# library(ggmap)

# ### Set working directory ??? CHANGE IF NECESSARY! ------------------------

# setwd("~/Google Drive/0.Work/0_Research/CFLR")
setwd("/home/eucman/nhs_migration")

### Import data

# N.B. Google Sheets needs to e authorized for first use! Follow steps explained in https://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html#authorization-using-oauth2

# gs_auth(new_user = TRUE) # Only use to restart the GS authorization process!

my_sheets <- gs_ls() # Load all my Google Sheets
my_sheets %>% glimpse() # See names of my Google Sheets
CFLR <- gs_title("Migration Data Local Authorities Batch Geocode + NHS Trusts Geocode") # Load the main data

# Load matched trusts and migration areas
CFLR_0 <- CFLR %>%
  gs_read(ws = "0. Areas-Trusts")

# Load Migration Flows
CFLR_1 <- CFLR %>%
  gs_read(ws = "1. Migration Flows ONS + GeoCode")

# Load HSMR data
CFLR_2 <- CFLR %>%
  gs_read(ws = "2. HSMR - DrFoster")

# Load trusts sheet and select only trust code, trust name, latitude and longitude
CFLR_4 <- CFLR %>%
  gs_read(ws = "4. NHS Trusts GeoCode")

##########################################
##                                      ##
### ANALYSES
##                                      ##
##########################################

### Prepare migration flow dataset

names(CFLR_1)

## 0. Prepare intermediate table with the Net Migration variable --------
# 0. Migration Data ?????? Remove unwanted years, reshape long, keep Net Growth

CFLR_1_long_20032011_net_growth <- CFLR_1 %>%
  select(Area.Code, Area.Name, contains("Net_Growth"), -contains("2012.2013"), -contains("2013.2014")) %>% # Select only variables of interest
  gather(key = Time, value = Net_Growth, 3:11) %>% # Reshape wide to long
  mutate(Time = gsub("Net_Growth_","",Time)) %>% # Clean the time variable
  mutate(Year = str_sub(Time, start = -4,end = -1)) %>% # Creates the variable Year
  mutate(AC_yr = paste0(Area.Code,"_",Year)) # Creates concat ID AC_yr

## 0.1. Prepare intermediate table with the Population Estimates varia --------
# 0.1. Migration Data ?????? Remove unwanted years, reshape long, keep Population

CFLR_1_long_20032011_population <- CFLR_1 %>%
  select(Area.Code, Area.Name, contains("Population"), -contains("2013"), -contains("2014")) %>% # Select only variables of interest
  gather(key = Time, value = pop_estimate, 3:11) %>% # Reshape wide to long
  mutate(Time = gsub(".Population.Estimate","",Time)) %>% # Clean the time variable
  mutate(Time = gsub("Mid.","",Time)) %>% # Clean the time variable
  mutate(Year = gsub(".Population.Estimate","",Time)) %>% # Clean the time variable
  mutate(Year = gsub("Mid.","",Year)) %>% # Creates the variable Year
  mutate(AC_yr = paste0(Area.Code,"_",Year)) # Creates concat ID AC_yr

## 0.2. Prepare intermediate table with the Net LTIM variable -------- --------
# 0.2. Migration Data ?????? Remove unwanted years, reshape long, keep Net_LTIM

CFLR_1_long_20032011_NetLTIM <- CFLR_1 %>%
  select(Area.Code, Area.Name, contains("Net_LTIM"), -contains("2012.2013"), -contains("2013.2014")) %>% # Select only variables of interest
  gather(key = Time, value = Net_LTIM, 3:11) %>% # Reshape wide to long
  mutate(Time = gsub("Net_LTIM_","",Time)) %>% # Clean the time variable
  mutate(Year = str_sub(Time, start = -4,end = -1)) %>% # Creates the variable Year
  mutate(AC_yr = paste0(Area.Code,"_",Year)) # Creates concat ID AC_yr

## 0.3. Prepare intermediate table with the Net_IM variable -------- ---------- 
# 0.3. Migration Data ?????? Remove unwanted years, reshape long, keep Net_IM

CFLR_1_long_20032011_NetIM <- CFLR_1 %>%
  select(Area.Code, Area.Name, contains("Net_IM"), -contains("2012.2013"), -contains("2013.2014")) %>% # Select only variables of interest
  gather(key = Time, value = Net_IM, 3:11) %>% # Reshape wide to long
  mutate(Time = gsub("Net_IM_","",Time)) %>% # Clean the time variable
  mutate(Year = str_sub(Time, start = -4,end = -1)) %>% # Creates the variable Year
  mutate(AC_yr = paste0(Area.Code,"_",Year)) # Creates concat ID AC_yr

# 0.4. Take all migration variables and create single table --------------------
## Take the results of the operations above and
## create full migration data table in long format with:
## Population Estimates, Net Migration growth, Net_LTIM, Net_IM

# Join Net migration growth + Population

CFLR_5 <- left_join(
  CFLR_1_long_20032011_net_growth,
    select(CFLR_1_long_20032011_population, pop_estimate, AC_yr),
  by="AC_yr")

# Add Net_LTIM

CFLR_5 <- left_join(
  CFLR_5,
  select(CFLR_1_long_20032011_NetLTIM, Net_LTIM, AC_yr),
  by="AC_yr")

# Add Net_IM

CFLR_5 <- left_join(
  CFLR_5,
  select(CFLR_1_long_20032011_NetIM, Net_IM, AC_yr),
  by="AC_yr")

# 0.5. Match trusts to migration areas data ------------------------------------
## Create dataset with trusts matched to Migration Areas

CFLR_6 <- full_join(CFLR_0, CFLR_5, by = "Area.Code") %>%
  mutate(trust_yr = paste0(trust_ID,"_",Year)) # Creates concat ID AC_yr

# Create dada frame with Trust HSMR data summarized at the year level

CFLR_2_yr_SMRsum <- CFLR_2 %>%
  mutate(trust_yr = paste0(trust_id,"_",yr)) %>% # Concat id trust/year for grouping
  group_by(trust_yr) %>% # Compute summary SMR variables for each trust/year
  summarise(avg_smr = mean(SMR),
                      min_smr = min(SMR),
                      max_smr = max(SMR),
                      sd_smr = sd(SMR),
                      median_smr = median(SMR))

# 0.6. Create final dataset with trust SMR matched to Migration data --------
## Create dataset with trusts matched to migration areas and mortality data

CFLR_7 <- inner_join(CFLR_6, CFLR_2_yr_SMRsum, by = "trust_yr")

CFLR_7 %>% select(trust_id_recode) %>% distinct()

summary(CFLR_7)

#Data verification

Trusts0<-sqldf("SELECT DISTINCT NHS_CODE FROM CFLR_4")  #The number of trusts in sheet
Trusts1<-sqldf("SELECT DISTINCT nearTRUST FROM CFLR_7") #The number of trusts in the final
numTrusts0 <- nrow (Trusts0) #347
numTrusts1 <- nrow (Trusts1) #123

#Trust - demand (sum of pop_estimate by year) : 

TArea0 <- sqldf("SELECT trust_id_recode as trust, year, count('Area.Code') as area, sum(pop_estimate) as pop
                ,AVG(avg_smr) as smr, SUM(NET_LTIM) as NET_LTIM
                FROM CFLR_7 
                GROUP BY trust_id_recode, year")

numTrust2 <- sqldf('SELECT COUNT(DISTINCT trust) 
                   FROM TArea0') #116 trusts.

plot(TArea0$NET_LTIM, TArea0$smr)

#OLS analysis
ols <-lm(smr ~ NET_LTIM, data=TArea0)
summary(ols)
yhat <- ols$fitted
plot(TArea0$NET_LTIM, TArea0$smr, pch=19, xlab="net immigration", ylab="mortality")
abline(lm(TArea0$smr~TArea0$NET_LTIM),lwd=3, col="red")

#fixed effect
fixed.dum <-lm(smr ~ NET_LTIM + pop + factor(trust) - 1, data=TArea0)
summary(fixed.dum)
fixed0 <- plm(smr ~ NET_LTIM       , data=TArea0, index=c("trust", "Year"), model="within")
fixed1 <- plm(smr ~ pop            , data=TArea0, index=c("trust", "Year"), model="within")
fixed2 <- plm(smr ~ NET_LTIM + pop , data=TArea0, index=c("trust", "Year"), model="within")
summary(fixed0)
summary(fixed1)
summary(fixed2)

#Let's create lag variables. I am using 'slide' a custom function. 
#Not lear whether observations with NA due to the time shift drops 
TASlid1 <- slide(TArea0, Var = "NET_LTIM", GroupVar = "trust",slideBy = -1)
TASlid2 <- slide(TArea0, Var = "NET_LTIM", GroupVar = "trust",slideBy = -2)
TASlid3 <- slide(TArea0, Var = "pop",    GroupVar = "trust",slideBy = -1)
TASlid4 <- slide(TArea0, Var = "pop",    GroupVar = "trust",slideBy = -2)
TArea0$NET_LTIM_1LAG <- TASlid1$`NET_LTIM-1`   #Put new names on the lag variables
TArea0$NET_LTIM_2LAG <- TASlid2$`NET_LTIM-2`
TArea0$pop_1LAG      <- TASlid3$`pop-1`
TArea0$pop_2LAG      <- TASlid4$`pop-2`

#time varying controls are important for fixed effect analysis
#Potential controls may include year dummy, size of the hospital (i.e. # of employees), 
#number of patients and total number of services that hospitals provide.


TArea0$Year_dum1 <- as.factor(TArea0$Year)  #A quick google search tells me that converting var into factor is a reasonable way
TArea0$Year      <- as.numeric(TArea0$Year) #Let's make sure that plm understands what it means by 'Year'.

fixed3 <- plm(smr ~ pop + pop_1LAG + pop_2LAG                                                          , data=TArea0, index=c("trust", "Year"), model="within")
fixed4 <- plm(smr ~                             NET_LTIM + NET_LTIM_1LAG + NET_LTIM_2LAG               , data=TArea0, index=c("trust", "Year"), model="within")
fixed5 <- plm(smr ~ pop + pop_1LAG + pop_2LAG + NET_LTIM + NET_LTIM_1LAG + NET_LTIM_2LAG               , data=TArea0, index=c("trust", "Year"), model="within")
fixed6 <- plm(smr ~ pop + pop_1LAG + pop_2LAG + NET_LTIM + NET_LTIM_1LAG + NET_LTIM_2LAG + Year_dum1   , data=TArea0, index=c("trust", "Year"), model="within")

summary(fixed3)
summary(fixed4)
summary(fixed5)
summary(fixed6)

#Wow, "fixed6' shows that NET_LTIM impact the mortality.
#The direction is opposit to the expecte. They are negatively correlated; 
#i.e.increased immigration reduces mortality;

#issues ------------
# 1.missing Trusts: 347-123 = 224 



# Panel regression models -------------------------------------------------

##########################################
##                                      ##
## Map and check NHS Trusts coordinates ##
##                                      ##
##########################################

# getting the map
ukmap <- get_map(location = c(lon = mean(CFLR_1$Longitude), lat = mean(CFLR_1$Latitude)), zoom = 6, maptype = "terrain")

# plotting the map with NHS Trusts coordinates
ggmap(ukmap) +
  geom_point(data = CFLR_4,
             aes(x = Longitude, y = Latitude,
                 fill = "red", alpha = 0.8), size = 2, shape = 21) +
  guides(fill=F, alpha=F, size=F) +

# Adding migration areas coordinates 
  geom_point(data = CFLR_1,
             aes(x = Longitude, y = Latitude,
                 fill = "red", alpha = 0.8), size = 1, shape = 19) +
  guides(fill=F, alpha=F, size=F)


#########################################
#
# UNUSED CODE AND EXPERIMENTS
#
#########################################

## Uncomment and execute with code above to get summary stats by area for the net growth variable

# %>%
#   group_by(Area.Code) %>%
#   summarise(avg_Net_Growth = mean(as.integer(Net_Growth)),
#             sd_Net_Growth = sd(as.integer(Net_Growth)),
#             min_Net_Growth = mean(as.integer(Net_Growth)), 
#             max_Net_Growth = mean(as.integer(Net_Growth)),
#             median_Net_Growth = mean(as.integer(Net_Growth)))


# 
# CFLR_1_20032011 <- CFLR_1 %>%
#   select(Area.Code,
#          Area.Name,
#          contains("Net_Growth"),
#          contains("Population"),
#          contains("IM_"),
#          contains("LTIM"),
#          -contains("2012.2013"), -contains("2013.2014"))