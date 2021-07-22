library(readstata13)
library(tidyverse)
library(countrycode)
library(labelled)

#preppeddata <- "/Volumes/GoogleDrive/My Drive/Master IPE Data/Merge Spring 2020/preppeddata/"

# get all .rdata file names
fnames.rdata <- list.files(preppeddata, "*.rdata", ignore.case = T)

# identifiers that we don't want to merge on
extra_ids <- c("country", "ccode", "ifscode", "ifs", "gwabbrev")

# load the first file
ipe <- load(paste(preppeddata, fnames.rdata[1], sep = ""))
ipe <- get(ipe) %>% 
  dplyr::select(-one_of(extra_ids)) %>%
  mutate_at(vars(one_of(c("gwno","year"))), as.numeric)

# load and merge the rest of the files
for(n in fnames.rdata[2:length(fnames.rdata)]){
  
  df <- load(paste(preppeddata, n, sep = "")) # load, store reference
  
  df <- get(df) %>% # get file from reference
    dplyr::select(-one_of(extra_ids)) %>% # remove extra ids
    mutate_at(vars(one_of(c("gwno","year"))), as.numeric) # make sure gwno and year are numeric
  
  print(n) # print name to ckeck progress
  ipe <- full_join(ipe, df) # merge
 
  # check for duplicates introduced 
  n_occur <- data.frame(table(ipe$gwno, ipe$year))
  if(sum(n_occur$Freq > 1) > 0){
    print(n_occur[n_occur$Freq > 1,])
    break
  }
  
  # clean up loaded objects from environment
  rm(list=setdiff(ls(), c("ipe", "preppeddata", "rawdata", "extra_ids", "append_ids")))
}

# check that everything joined by gwno and year, except AP, CEPII, and LANG which joined by gwno only
# if any new cross-sectional datasets have been added, those should also merge by gwno only

# do the same for .dta files
fnames.dta <- list.files(preppeddata, "*.dta")

for(n in fnames.dta){
  
  df <- read.dta13(paste(preppeddata, n, sep = "")) %>%
    dplyr::select(-one_of(extra_ids)) %>%
    mutate_at(vars(one_of(c("gwno","year"))), as.numeric)
  
  print(n)
  ipe <- full_join(ipe, df)
  
  # check for duplicates introduced 
  n_occur <- data.frame(table(ipe$gwno, ipe$year))
  if(sum(n_occur$Freq > 1) > 0){
    print(n_occur[n_occur$Freq > 1,])
    break
  }
}

# check for duplicates
n_occur <- data.frame(table(ipe$gwno, ipe$year))
print(n_occur[n_occur$Freq > 1,])

# add country names back (from raw country names)
countrynames_raw <- ipe %>%
  select(starts_with('countryname_raw_')) %>%
  select(-countryname_raw_PTO, -countryname_raw_BZ) %>% # exclude these bc they are labeled, so don't work with coalesce
  mutate(countrynames = coalesce(!!!.))

ipe$country <- countrynames_raw$countrynames

# get a few countrynames that coalesce misses
ipe$country <- ifelse(is.na(ipe$country), ipe$countryname_raw_PTO, ipe$country)
ipe$country <- ifelse(is.na(ipe$country), countrycode(ipe$gwno, 'gwn', 'country.name'), ipe$country)
table(ipe$gwno[is.na(ipe$country)])
ipe$country[ipe$gwno == 711] <- "Tibet"

# this actually isn't neccesarry because append_ids will catch these
#ipe_v4 <- ipe %>% filter_all(any_vars(!is.na(.))) 

# add IDs back
ipe_v4 <- ipe %>% 
  dplyr::rename(gwno_raw_BH = gwno_raw) %>%
  append_ids(breaks = F)

# check for duplicates
n_occur <- data.frame(table(ipe_v4$country, ipe_v4$year))
print(n_occur[n_occur$Freq > 1,])

# this row contains no data
ipe_v4 <- ipe_v4[-which(ipe_v4$gwno_raw == 340 & ipe_v4$year == 2006),]

save(ipe_v4, file = paste(preppeddata,"../master_ipe_v4.rdata",sep=""))
write.table(ipe_v4, file = paste(preppeddata,"../master_ipe_v4.tab",sep=""))
