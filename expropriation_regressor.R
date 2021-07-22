# Robbing Peter to Purge Paul - by Timothy Liptrot, Bhavya Srivastava and Claire Lee

# start - packages
library(tidyverse)
library(haven)
library(readr)

# data - international expropriations. There are three files because the Kobrin's original data was expanded by subsequent authors. I have contacted all three for permissions.
expr_h <- read_csv("Data/working_data/international_expropriations/exprop-Hajzler-2008-03-09.csv")
expr_m <- read_csv("Data/working_data/international_expropriations/exprop-Minor-2007-10-22.csv")
expr_k <- read_csv("Data/working_data/international_expropriations/exprop-Kobrin-2007-10-22.csv")

# The variable names are not consistent across the three documents, this changes them all to lower
names(expr_h)<-tolower(names(expr_h))
names(expr_m)<-tolower(names(expr_m))
names(expr_k)<-tolower(names(expr_k))

#brings them all together
expr_mk <- full_join(expr_m, expr_k)
expr_all <- full_join(expr_mk, expr_h)

# This entire section standardizes country names 
expr_all$country <- tolower(expr_all$country)
expr_all$type <- tolower(expr_all$type)
expr_all$sector <- tolower(expr_all$sector)
expr_all$region <- tolower(expr_all$region)

expr_all <- expr_all %>% 
  mutate_at("country", funs(str_replace(country, "abu dhabi", "united arab emirates"))) %>%
  mutate_at("country", funs(str_replace(country, "srilanka", "sri lanka"))) %>%
  mutate_at("country", funs(str_replace(country, "elsalvador", "el salvador"))) %>%
  mutate_at("country", funs(str_replace(country, "burma", "myanmar"))) %>%
  mutate_at("country", funs(str_replace(country, "antigua", "antigua and barbuda"))) %>%
  mutate_at("country", funs(str_replace(country, "antgua", "antigua"))) %>%
  mutate_at("country", funs(str_replace(country, "congopr", "congo-brz"))) %>%
  mutate_at("country", funs(str_replace(country, "malagasay", "madagascar"))) %>%
  mutate_at("country", funs(str_replace(country, "s arabia", "saudi arabia"))) %>%
  mutate_at("country", funs(str_replace(country, "trinidad", "trinidad and tobago"))) %>%
  mutate_at("country", funs(str_replace(country, "zaire", "zimbabwe"))) %>%
  mutate_at("country", funs(str_replace(country, "costarica", "costa rica"))) %>%
  mutate_at("country", funs(str_replace(country, "^car$", "cen african rep"))) %>%
  mutate_at("region", funs(str_replace(region, "mide", "middle east"))) %>%
  mutate_at("region", funs(str_replace(region, "^mid$", "middle east"))) %>%
      mutate(overt = type=="nat")

industry_key <- c(pet = "extractive", agr = "manufacturing", min = "extractive", util = 'utilities', bank = 'services', trad = 'services', svc ='services', tran = 'services', mfg = 'manufacturing',cons='services',meda='extractive',com='services',tourism='services',food='manufacturing',trans='services', ins ='manufacturing')

expr_all$sector_sim <- recode(expr_all$sector, !!!industry_key)

## The Reign dataset provides the most detailed record of when new leaders came into office. We might change out leaders for regimes in the final version (a regime is a leader-selection device with a constant support group like the CCP or the Saudi Royal Family, which can span multiple leaders)

reign <- read_csv("REIGN_2021_4.csv")
reign$country <- tolower(reign$country)



small_reign_lag <- filter(reign, month==1 | country != lag(country) | leader != lag(leader))
small_reign_no_lag <- filter(reign, month==1 | country != lag(country))# | leader != lag(leader))

# Join reign and expr

int_expr <- left_join(expr_all, small_reign_no_lag, by = c("country","year")) %>%
  mutate(tenure_years = tenure_months/12,
         decade = year - (year %% 10))


glimpse(int_expr)

#makes a histogram of the years by type. Note the massive drop off in 1980, when basically all DC assets were taken. The increase seince 1990 has accelerated, but I have not integrated the new data yet
ggplot(data=filter(df3,!is.na(overt)), aes(x=year, fill=overt)) + 
  geom_histogram(binwidth = 2, alpha=.5, position="identity") +
  facet_grid(rows = vars(overt)) +
  scale_fill_discrete(name = "Type", labels = c("Covert", "Overt")) +
  theme(strip.text.y = element_blank())

#load the IPE big data

load("~/GitHub_reformrat/expropriation/Data/working_data/IPE_data/dataverse_files/3. Graham_Tucker_IPE_v4.rdata")

ipe_v4 <- filter(ipe_v4, year > 1949)

int_expr <- left_join(int_expr, ipe_v4, by = c("ccode","year"))

#great!

# So far we have all the data to run regressions on method of expropriation, conditional on expropriation. But we cannot run a hazard model on the decision to expropriate, because we do not know when foreign investment existed to start with. Tomz and Wright have a dataset of all country years with any foreign investment



tomz <- read_dta("Data/working_data/tomz_wright_2012/TomzWright2010.dta")

tomz <- tomz %>% 
  rename(
    ccode = ctrycode
  )  


ipe_tomz <- left_join(ipe_v4, tomz, by = c("ccode","year"))

ipe_tomz <- left_join(ipe_tomz, small_reign_no_lag, by = c("ccode","year")) %>%
  mutate(tenure_years = tenure_months/12,
         decade = year - (year %% 10))

#I have a problem where the expr_all doesn't have country codes, so I have to merge them with reign first
reign_ccode <- small_reign_no_lag %>%
  select(country,ccode,year)

df5 <- left_join(expr_all, reign_ccode, by = c("country","year"))

full <- left_join(ipe_tomz, df5, by = c("ccode","year"))

# converts the Overt - Covert coding into three dummy variables. Int = intervention (when a non-state actor takes and government does not uphold the previous ownership rights)
full <- full %>%
  filter(fdi==1) %>%
  mutate(expr = !is.na(overt),
         covert_dummy = ifelse(is.na(overt),0,ifelse(overt==0,1,0)),
         overt_dummy = ifelse(is.na(overt),0,ifelse(overt==1,1,0)),
         int_dummy = ifelse(is.na(overt),0,ifelse(type=="int",1,0))
  )

# adds personalism data
Personalism_Data_2 <- read_dta("Data/working_data/Personalism Data (2).dta")
glimpse(Personalism_Data_2)

filter(full, country=="cuba")

full_pers_right <- right_join(full, Personalism_Data_2, by=c('year'="year","countryname_raw_GE"="country"))

# adds personalism data to int_expr

int_expr_pers <- left_join(int_expr, Personalism_Data_2, by=c('year'="year","countryname_raw_GE"="country"))


# Saving the datasets
write.csv(full_pers_right, "Data/working_data/full_pers_extra.csv")

write.csv(int_expr_pers, "Data/working_data/int_expr_extra.csv")

# Alternative coding of non-democracies
#eri_aut <- filter(eri, gov_democracy==0)

# Analysis - the old analysis I did, using only tenure in office.

m1 <- glm(data=eri_aut, overt ~ log(tenure_years) + polity2_P4 + gdppc_WDI_PW + bitstodate_BIT + under_BCG, family = binomial(link = "probit"))

summary(m1)

m2 <- glm(data=eri_aut, overt ~ log(tenure_years) + polity2_P4 + gdppc_WDI_PW +  + bitstodate_BIT+ under_BCG + factor(sector_sim) + factor(region), family = binomial(link = "probit"))

summary(m2)

m3 <- glm(data=eri_aut, overt ~ log(tenure_years) + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2  + bitstodate_BIT+ under_BCG + factor(sector_sim) + factor(region) + factor(decade), family = binomial(link = "probit"))


summary(m3)

fivenum(eri_aut$gdppc_WDI_PW)

summary(m6)

## Yay we got positive results on the first test

#translate it into latex

library(stargazer)
stargazer(m1,m2,m3, type="latex",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

stargazer(m4,m5,m6, type="latex",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

## Now I look at propensity to expropriate

v_dem <- readRDS("~/GitHub/expropriation/vdem/Country_Year_V-Dem_Core_R_v11.1/V-Dem-CY-Core-v11.1.rds")

eti_aut_vd <- left_join(eti_aut, v_dem, by = c("countryname_raw_VDEM"="country_name","year"="year"))

mb1 <- glm(data=eti_aut_vd, expr ~ couprisk + polity2_P4 + lngdppc_WDI_PW + v2x_veracc + factor(decade), family = binomial(link = "probit"))

mb2 <- glm(data=eti_aut_vd, overt_dummy ~ couprisk + polity2_P4 + lngdppc_WDI_PW +v2x_veracc + factor(decade), family = binomial(link = "probit"))

mb3 <- glm(data=eti_aut_vd, covert_dummy ~  couprisk + polity2_P4 + lngdppc_WDI_PW + v2x_veracc + factor(decade), family = binomial(link = "probit"))

mb4 <- glm(data=eti_aut_vd, int_dummy ~ couprisk + polity2_P4 + lngdppc_WDI_PW + v2x_veracc + factor(decade), family = binomial(link = "probit"))

stargazer(mb1,mb2,mb3,mb4, type="text",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","No","NO","No"),c("Region Fixed Effects?","No","No","No","No"),c("Decade Fixed Effects?","Yes","Yes","Yes","No")))

ggplot()

count(eti_aut, int_dummy)

## Now let us look at the propensity to expropriate. First, we need data on who actually has anything worth stealing, sneaky or otherwise

library(readxl)
ext_wealth <- read_excel("FDI positions/EWN 1970-2015 (1).xls", sheet = "Data")

glimpse(ext_wealth)

# This data appears very incomplete. I have found a better dataset in Tomz & Wright 2010 and am currently emailing them for sweet sweet data. Will check back soon.

# Legislative constraisn
eri_aut$v2xlg_legcon_VDEM

m31 <- lm(data=eri_aut, overt ~ log(tenure_years) + v2xlg_legcon_VDEM+ polity2_P4 + gdppc_WDI_PW)

summary(m31)

m32 <- lm(data=eri_aut, overt ~ tenure_years + v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region))

summary(m32)

m33 <- lm(data=eri_aut, overt ~ tenure_years + v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region) + factor(decade))

summary(m33)

# Stargazer it

stargazer(m31,m32,m33, type="latex",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

# Check against local elections

eri_aut <- eri_aut %>%
  mutate(subnat_elec_VDEM = pmax(v2xel_locelec_VDEM,v2xel_regelec_VDEM,na.rm=TRUE))

m41 <- lm(data=eri_aut, overt ~ tenure_years + subnat_elec_VDEM + v2xlg_legcon_VDEM+ polity2_P4 + gdppc_WDI_PW)

summary(m41)

m42 <- lm(data=eri_aut, overt ~ tenure_years + subnat_elec_VDEM + v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region))

summary(m42)

m43 <- lm(data=eri_aut, overt ~ tenure_years + subnat_elec_VDEM + v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region) + factor(decade))

summary(m43)

# stargazer

stargazer(m41,m42,m43, type="latex",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

## Audience costs

# First I have to construct a new dataset with a vertical accountability index 

v_dem <- readRDS("~/GitHub/expropriatoin/vdem/Country_Year_V-Dem_Core_R_v11.1/V-Dem-CY-Core-v11.1.rds")

eri_aut <- left_join(eri_aut, v_dem, by = c("countryname_raw_VDEM"="country_name","year"="year"))

m51 <- lm(data=eri_aut, overt ~ log(tenure_years) + v2x_veracc + polity2_P4 + gdppc_WDI_PW)

summary(m51)

m52 <- lm(data=eri_aut, overt ~ tenure_years + v2x_veracc + v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region))

summary(m52)

m53 <- lm(data=eri_aut, overt ~ tenure_years + v2x_veracc + v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region) + factor(decade))

summary(m53)

# stargazer it

stargazer(m51,m52,m53, type="latex",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

# Check military accountability?

v_dem$v2x_ex

m61 <- lm(data=eri_aut, overt ~ tenure_years + v2x_ex_military + polity2_P4 + gdppc_WDI_PW)

summary(m61)

m62 <- lm(data=eri_aut, overt ~ tenure_years + v2x_veracc + v2x_ex_military+  polity2_P4 + gdppc_WDI_PW)

summary(m62)

m62 <- lm(data=eri_aut, overt ~ tenure_years + v2x_ex_military + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region))

summary(m62)

m63 <- lm(data=eri_aut, overt ~ tenure_years + v2x_veracc + v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region) + factor(decade))

summary(m63)

# stargazer it

stargazer(m61,m62,m63, type="text",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

# Checking on neopatrimonialism

m71 <- lm(data=eri_aut, overt ~ tenure_years + v2x_neopat + polity2_P4 + gdppc_WDI_PW)

summary(m71)

m72 <- lm(data=eri_aut, overt ~ tenure_years + v2x_neopat + v2x_ex_military +  polity2_P4 + gdppc_WDI_PW)

summary(m62)

m72 <- lm(data=eri_aut, overt ~ tenure_years + v2x_ex_neopat + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region))

summary(m72)

m73 <- lm(data=eri_aut, overt ~ tenure_years + v2x_neopat+ v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region) + factor(decade))

summary(m73)

stargazer(m71,m72,m73, type="text",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

# what about a probit model?

#install.packages('aod')
library(aod)
library(stargazer)

m81 <- glm(data=eri_aut, overt ~  tenure_years + v2x_ex_military +  v2xlg_legcon_VDEM+ polity2_P4 + gdppc_WDI_PW)

summary(m81)

m82 <- glm(data=eri_aut, overt ~  tenure_years + v2x_ex_military+   v2xlg_legcon_VDEM+ polity2_P4 + gdppc_WDI_PW)

summary(m82)

m83 <- glm(data=eri_aut, overt ~ tenure_years + v2x_ex_military +  v2xlg_legcon_VDEM + polity2_P4 + gdppc_WDI_PW + gdppc_WDI_PW^2 + factor(sector_sim) + factor(region))

summary(m83)

stargazer(m81,m82,m83, type="text",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

m91 <- glm(data=eri_aut, overt ~  log(tenure_years) + v2x_veracc + v2x_ex_military +  v2xlg_legcon_VDEM+ polity2_P4 + log(gdppc_WDI_PW))

summary(m91)

m92 <- glm(data=eri_aut, overt ~  tenure_years + v2x_veracc + v2x_ex_military+   v2xlg_legcon_VDEM+ polity2_P4 + log(gdppc_WDI_PW))

summary(m92)

m93 <- glm(data=eri_aut, overt ~ tenure_years + v2x_veracc + v2x_ex_military +  v2xlg_legcon_VDEM + polity2_P4 + log(gdppc_WDI_PW) + factor(sector_sim) + factor(region))

summary(m93)

stargazer(m91,m92,m93, type="text",
          omit = c("sector_sim","region","decade"),
          add.lines=list(c("Sector Fixed Effects?","No","Yes","Yes"),c("Region Fixed Effects?","No","Yes","Yes"),c("Decade Fixed Effects?","No","No","Yes")))

#when does vdem vertical accountability start?
ipe_v4$v2xel_locelec_VDEM
ggplot(data=ipe_v4, aes(x=year,y=v2xel_locelec_VDEM)) + geom_point()

df6 <- ipe_v4 %>% group_by(year) %>%
  summarize(mean_loc = mean(v2xel_locelec_VDEM,na.rm=TRUE),
            mean_na = mean(is.na(v2xel_locelec_VDEM)),
            obs =n())

#view(df6)

ggplot(data=df6,aes(x=year,y=mean_loc)) + geom_point()
ggplot(data=df6,aes(x=year,y=mean_na, color='blue')) + geom_point()

filter(ipe_v4,year == 1789)$country

eri$coup

summary(m2)

library(stargazer)
stargazer(m1)

mean(is.na(eri$))
glimpse(reign)



unique(reign$country)
     
ggplot(data=small_reign_lag,aes(x=tenure_months,y=couprisk)) + geom_point()

glimpse(expr_all)

filter(reign,country=="mozambique")

df4 <- filter(df3,!is.na(overt))

#view(expr_all %>% group_by(country) %>%
#  summarize(exprs = n()) %>%
#  arrange(desc(exprs)))

sort(unique(expr_all$country))

ggplot(data=df3, aes(x=year, fill=overt, color = overt)) + 
  geom_histogram(binwidth = 1, alpha=.5, position="identity")


# Personalism section

library(haven)
GWF <- read_dta("~/GitHub/expropriation/GWF/how_dictatoships_work_data/GWF.dta")


glimpse(GWF)

glimpse(ATH)
ATH$per
