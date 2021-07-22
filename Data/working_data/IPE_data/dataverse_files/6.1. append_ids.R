############################################
############################################
##  New append_ids function 
##  Miriam Barnum 
##  February 5, 2019
##
##  - warnings for duplicate country years added June 2020 - MB
############################################
############################################

append_ids <- function(df,breaks=T) {
  
  #Load in the country IDs file
  load(paste(rawdata,"MasterGWNO.RDATA",sep=""))
  #load("/Volumes/GoogleDrive/My Drive/Master IPE Data/raw-data v5/MasterGWNO.RDATA")
  
  # prevents false positives for "notfound"
  ids$minyear2 <- ifelse(ids$minyear2 == -9999, 9999, ids$minyear2)
  
  ############################################
  ## Format Data                            ##  
  ## (based on original append IDS function) #
  ############################################
  
  yrexists = 0
  ctryexists = 0
  for (i in 1:length(names(df))) {
    #Find the country variable and name it country
    if (names(df)[i]=="Country" | names(df)[i]=="country" | 
        names(df)[i]=="CountryName" | names(df)[i]=="countryname" | 
        names(df)[i]=="Countryname" | names(df)[i]=="COUNTRY") {
      names(df)[i]="country"
      df$country = as.character(df$country)
      ctryexists=1
      #R will sometimes read the country name as a factor. We'll make
      #  sure it's just a string
      df$country = as.character(df$country)
    }
    #Check to see if the year variable exists
    if (names(df)[i]=="Year" | names(df)[i]=="year" | names(df)[i]=="YEAR") {
      names(df)[i] = "year"
      yrexists = 1
    }
    
    #Check to see if a gwno code already exists
    if (names(df)[i] == "GWNo" | names(df)[i]=="gwno" | 
        names(df)[i]=="GWNO") {
      names(df)[i]="gwno_raw"
    }
    
    #Check to see if a cow code already exists
    if (names(df)[i]=="COW" | names(df)[i]=="cow" | 
        names(df)[i]=="ccode" | names(df)[i]=="cowcode") {
      names(df)[i] = "ccode_raw"
    }
  }
  
  if(ctryexists == 0) {
    stop("The data must have a variable containing the country names 
         before the append ids function is called")
  }
  
  if (yrexists == 0) {
    warning("NO YEAR VARIABLE PROVIDED. If this is not intended to be a cross-sectional dataset, please ensure that the year variable is correctly named.
            GWNOs for cross-sectional datasets will be assigned based on 2015 country names.")
    #We're going to pretend that the year of every observation is 2015
    df$year = rep(2015,nrow(df))
  }
  
  # check if there are already duplicate country-years in the data
  n_occur <- data.frame(table(df$country, df$year))
  if(sum(n_occur$Freq > 1) > 0){
    warning("STOP: Duplicated country-years in data. Check and remove duplicates before running append_ids(). See list of duplicates above.")
    print(n_occur[n_occur$Freq > 1,])
  }
  rm(n_occur)
  
  
  #############################
  ## Preprocess Country Names #
  #############################
  
  df$countryname_raw <- df$country
  df$country <- tolower(df$country)
  df$country <- gsub('[[:punct:]]', '', df$country)
  df$country <- gsub('\\s', '', df$country) 
  
  ########################
  ## Append Country IDs #
  ########################
  
  # keep track of countries without gwnos
  df$notfound <- NA
  
  # merge in ids
  df <- merge(df, ids, by = "country", all.x = T)
  
  # note country names not found, or outside of min/max years
  df$notfound <- ifelse(!is.na(df$gwno),
                           !((df$minyear1<=df$year & df$maxyear1>=df$year) | (df$minyear2<=df$year & df$maxyear2>=df$year)),TRUE)
  notfound <- df$countryname_raw[df$notfound]
  df <- df[!(df$notfound),]
  
  #print(df)
  
  df[,c("minyear1", "maxyear1", "minyear2", "maxyear2","notfound")] <- NULL
  
  
  #############################
  # Display and check countries
  #  that did not get a gwno
  #############################
  notfound <- unique(notfound)
  notfound <- notfound[!(notfound %in% df$countryname_raw)]
  
  print("The following countries were not given gwno codes: ")
  for (i in 1:length(notfound)) {
    print(notfound[i])
  }
  
  if(breaks) {
    print("**Check missing GWNO values, then type 'c' when done**")
    browser()
  }
  
  ###############################
  # Clean up and return dataframe
  ###############################
  
  # make gwno name be the country name
  df$country <- NULL
  names(df)[names(df)=="gwname"] = "country"
  
  # put the id variables first
  nonids <- names(df)[!(names(df) %in% c('country','gwno','year','ccode','ifs','ifscode','gwabbrev'))]
  df <- df[,c('country','gwno','year','ccode','ifs','ifscode','gwabbrev',nonids)]
  
  # Sort the data by gwno and year
  df = df[order(df$gwno,df$year),]
  
  
  #If the year variable was created and not originally part of the data, drop it here
  if (yrexists==0) {
    #Find theyear variable
    yrloc = -1
    for (q in 1:length(names(df))) {
      if (names(df)[q]=="year") {
        yrloc = q
        break
      }
    }
    df = df[,-yrloc]
  }
  
  #unload ids file
  rm(ids)
  
  # warn about duplicates added by append_ids
  if (yrexists == 1) {
    n_occur <- data.frame(table(df$country, df$year))
    if(sum(n_occur$Freq > 1) > 0){
      warning("Duplicate country-years introduced by append_ids. Check and remove duplicates as appropriate. See list of duplicates above.")
      print(n_occur[n_occur$Freq > 1,])
    }
    rm(n_occur)
  }
  
  #return the data frame
  return(df)
}

#############################################
# Function to Add a New Country Name/Spelling
#############################################

# This function requires two inputs -- the new country name and the existing one
# e.g. add_name("IR of Afghanistan", "Afghanistan")

add_name <- function(newName, existingName){
  load(paste(rawdata,"MasterGWNO.RDATA",sep=""))
  #load("/Volumes/GoogleDrive/My Drive/Master IPE Data/raw-data v5/MasterGWNO.RDATA")
  
  if (!is.character(newName) | !is.character(existingName)) {
    stop("Names must be provided as character strings.")
  }
  
  # preprocess names
  new <- tolower(newName)
  new <- gsub('[[:punct:]]', '', new)
  new <- gsub('\\s', '', new) 
  
  ex <- tolower(existingName)
  ex <- gsub('[[:punct:]]', '', ex)
  ex <- gsub('\\s', '', ex) 
  
  if (new %in% ids$country) {
    stop(paste(newName,"already exists in the MasterGWNO.RDATA file."))
  }
  
  if (!ex %in% ids$country) {
    stop(paste(existingName,"does not exist in the IDs file. Please provide a version of the country name that currently exists in the MasterGWNO.RDATA file."))
  }
  
  # duplicate old record, add new name
  tmp <- ids[ids$country == ex,]
  tmp$country <- new
  ids <- rbind(ids, tmp)
  
  save(ids,file=paste(rawdata,"MasterGWNO.RDATA",sep=""))
  #save(ids,file="/Volumes/GoogleDrive/My Drive/Master IPE Data/MasterGWNO.RDATA")
  
  rm(ids)
}

###############################
# Function to Append Suffixes
###############################

append_suffix <- function(df,suffix) {
  ids.names = c("country","year","gwno","ccode","ifscode","ifs","gwabbrev")
  for (i in 1:length(names(df))) {
    if ((names(df)[i] %in% ids.names)==F) {
      names(df)[i] = paste(names(df)[i],suffix,sep="_")
    }
  }
  return(df)
}