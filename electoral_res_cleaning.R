#############################
## MOSCOW HOUSING: TAKE 2  ##
## SEAN NORTON             ##
## DATE BEGUN:12.18.18     ##
#############################

##LIBRARY-----------------------------------------------------------------------
library(tidyverse) #data wrangling
library(readxl) # .xls format data loading
library(magrittr) #assignment pipe

options(stringsAsFactors = FALSE)

#function to clean strings of whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

##VOTE CLEANING-----------------------------------------------------------------

#votes - sourcing needed functions
source('vote_cleaner.R', encoding = 'utf-8')

#running cleaner
votes_clean <- vote.cleaning('./data/Vote')


##need a function that can convert this to a uik level dataframe

#need to assign colnames

uik.converter <- function(df){
  
  ### this function identifies uik columns and seperates them into dataframes
  
  stopifnot(is.data.frame(df))
  
  #select uik columns
  uik_cols <- grep(x = df[1, ],
                   pattern = '^УИК',
                   ignore.case = T)
  
  #break into seperate dataframes
  uik_list <- lapply(uik_cols, function(x){
    res <- cbind.data.frame(df[ ,1], df[ ,x], df$elec_name)
    colnames(res) <- c('name', 'votes', 'elec_name')
    res
  })
  
  return(uik_list)
  
}

uik_clean <- lapply(votes_clean, uik.converter)

#now I need to clean several things:
  # 1. Get UIK number as column
  # 2. Get voter information as columns
  # 3. Calculate turnout
#will write a function to do that - requires me to flatten the list first


uik_clean <- flatten(uik_clean)

uik.cleaner  <- function(df){
  
  stopifnot(is.data.frame(df))
  
  uik_no <- df[1,2]
  
  reg_voters <- df[2, 2]
  
  valid <- df[3,2]
  
  invalid <- df[4,2]
  
  
  res <- cbind(uik_no, df[5:nrow(df), ], reg_voters, valid, invalid)
  
  return(res)
}

uik_clean <- lapply(uik_clean, uik.cleaner)

save(uik_clean, file = 'tmp_uik.RData')

#now I need to clean the names of the digits

uik_clean <- lapply(X = uik_clean, function(df){
  df$name <- gsub(pattern = "[\\d][[:punct:]]*", 
                       replacement = "", 
                       x = df$name, 
                       perl = TRUE)
  df$name <- gsub(pattern = "[[:punct:]]", x = df$name, 
                       replacement = "", perl = TRUE)
  df
}
)

#lets clean the UIK number

uik_clean <- lapply(X = uik_clean, function(df){
  df$uik_no <- str_remove(df$uik_no, pattern = '^УИК №')
  df
})

#done - saving
save(uik_clean, file = './data/uik_clean.RData')

#CANDIDATE CLEAN----------------------------------------------------------------
source('candidate_clean.R')

#get filenames
candidate_files <- list.files('./data/Candidates', full.names = T)

candidate_clean <- lapply(candidate_files, 
                          FUN = candidate.cleaner)

#bind into one dataframe
candidate_clean <- do.call(rbind.data.frame, candidate_clean)

#save
save(candidate_clean, file = './data/candidate_clean.RData')

#make a new candidate key
write.csv(unique(candidate_clean$party), 'party_key.csv')

#read cleaned candidate key back in
party_key <- read.csv('party_key.csv')

#remove junk column
party_key[ ,1] <- NULL

#replace nas with zeroes
party_key %<>% replace_na(replace = list(opp = 0, comm = 0, ind= 0))

#write it back out to save
write.csv(party_key, 'party_key.csv')

#merge with candidates
candidate_clean <- left_join(candidate_clean, party_key, by = 'party')

#remove not registered candidates
not_reg <- which(candidate_clean$status != 'выдвинут')

candidate_clean <- candidate_clean[-not_reg, ]

#some duplicates that I need to remove
candidate_clean <- distinct(candidate_clean)

#save again
save(candidate_clean, file = './data/candidate_clean.RData')

##FINAL MERGE-------------------------------------------------------------------

#let's turn uik_clean into a real dataframe, and use group_by to create new df
uik_clean <- do.call(rbind.data.frame, uik_clean)

#merging candidate data in
uik_clean <- left_join(uik_clean, candidate_clean, 
                       by = c('name', 'elec_name' = 'election'))

#fixing typing
uik_clean$votes <- as.numeric(uik_clean$votes)
uik_clean$valid <- as.numeric(uik_clean$valid)
uik_clean$invalid <- as.numeric(uik_clean$invalid)
uik_clean$reg_voters <- as.numeric(uik_clean$reg_voters)

save(uik_clean, file = './data/uik_clean.RData')

##OPP CALCULATION---------------------------------------------------------------

#need to calculate vote %s for opp, comm, ind, and opp + ind
#will source a function to do so

opp.calculating <- function(x){
  
  #safety check
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(x$votes))
  
  #need total votes as local variable (in case 0 candidates in category)
  total_votes <- sum(x$votes)
  
  #need opp
  opp_cand_df <- x[x$opp == 1, ]
  opp_vote_pct <- sum(opp_cand_df$votes, na.rm = T)/total_votes
  
  #need comm
  comm_cand_df <- x[x$comm == 1, ]
  comm_vote_pct <- sum(comm_cand_df$votes, na.rm = T)/total_votes
  
  #need ind
  ind_cand_df <- x[x$ind == 1, ]
  ind_vote_pct <- sum(ind_cand_df$votes, na.rm = T)/total_votes
  
  #turnout
  turnout <- (x$valid + x$invalid)/x$reg_voters
  
  #elec_name
  elec_name <- x$elec_name[1]
  
  #creating return df
  uik_no <- x$uik_no
  
  res <- data.frame(uik_no = uik_no[1],
                    turnout = turnout[1],
                    opp_vote_pct = opp_vote_pct[1],
                    comm_vote_pct = comm_vote_pct[1],
                    ind_vote_pct = ind_vote_pct[1],
                    elec_name = elec_name)
  
  return(res)
  
}

#now I unfortunately need to group everything back into a list
uik_clean$uik_no <- as.factor(uik_clean$uik_no)
uik_list <- split(uik_clean, f = uik_clean$uik_no)

#lapply function
uik_list <- lapply(uik_list, opp.calculating)

#bring it back together
uik_df <- do.call(rbind.data.frame, uik_list)

#sanity checks - nas
which(is.na(uik_df$opp_vote_pct) & is.na(uik_df$comm_vote_pct) & is.na(uik_df$ind_vote_pct))
apply(uik_df, 2, function(x) sum(is.na(x)))
#no NAs - we are good to go

#sanity checks - values > 1 or <0
summary(uik_df$turnout) #good
summary(uik_df$opp_vote_pct) #good
summary(uik_df$ind_vote_pct) #good
summary(uik_df$comm_vote_pct) #good

#good to go - saving
save(uik_df, file = './data/uik_df.RData')

##MAYORAL CLEANING--------------------------------------------------------------

#mayor files
mayor_files <- list.files('./data/mayoral/Выборы Мэра Москвы/')

#running cleaner
mayor_clean <- vote.cleaning('./data/mayoral/Выборы Мэра Москвы/')

#getting results at uik level
mayor_clean <- lapply(mayor_clean, uik.converter)

mayor_clean <- flatten(mayor_clean)

mayor_clean <- lapply(mayor_clean, uik.cleaner)

#now I need to clean the names of the digits

mayor_clean <- lapply(X = mayor_clean, function(df){
  df$name <- gsub(pattern = "[\\d][[:punct:]]*", 
                  replacement = "", 
                  x = df$name, 
                  perl = TRUE)
  df$name <- gsub(pattern = "[[:punct:]]", x = df$name, 
                  replacement = "", perl = TRUE)
  df
}
)

#lets clean the UIK number

mayor_clean <- lapply(X = mayor_clean, function(df){
  df$uik_no <- str_remove(df$uik_no, pattern = '^УИК №')
  df
})

#save tmp
save(mayor_clean, file = 'mayor_temp.RData')

#can code the candidates directly here
may_cans <- as.data.frame(unique(mayor_clean[[1]]$name))

#intialize columns
colnames(may_cans)[1] <- 'name'
may_cans$opp <- 0
may_cans$comm <- 0
may_cans$ind <- 0

#code
may_cans[1, 'opp'] <- 1 #ldpr
may_cans[2, 'opp'] <- 1 #sr
may_cans[3, 'comm'] <- 1 #kprf
may_cans[4, 'opp'] <- 1 #yabloko
may_cans[5, 'opp'] <- 1 #navalny
#sobyanin (row 6) is regime candidate, despite running as ind

#merging in - will convert one dataframe to make sure merge is good
mayor_clean <- do.call(rbind.data.frame, mayor_clean)

#21582 observations
mayor_clean <- left_join(mayor_clean, may_cans, by = 'name')

#fixing typing issues
mayor_clean$valid <- as.numeric(mayor_clean$valid)
mayor_clean$votes <- as.numeric(mayor_clean$votes)
mayor_clean$invalid <- as.numeric(mayor_clean$invalid)
mayor_clean$reg_voters <- as.numeric(mayor_clean$reg_voters)

#good merge - splitting than calculating opposition
mayor_clean$uik_no <- as.factor(mayor_clean$uik_no)

mayor_clean <- split(mayor_clean, f = mayor_clean$uik_no)

mayor_clean <- lapply(mayor_clean, opp.calculating)

#re-unlisting
mayor_clean <- do.call(rbind.data.frame,mayor_clean)

#checking data
summary(mayor_clean$turnout)
summary(mayor_clean$opp_vote_pct)
summary(mayor_clean$comm_vote_pct)
#looks good

save(mayor_clean, file = './data/mayor_clean.RData')

## DUMA CLEANING----------------------------------------------------------------

# majoritarian first

#cleaning into seperate dataframes
maj_duma_clean <- vote.cleaning('./data/duma/majvote')

maj_duma_clean <- lapply(maj_duma_clean, uik.converter)

maj_duma_clean <- flatten(maj_duma_clean)

maj_duma_clean <- lapply(maj_duma_clean, uik.cleaner)

#removing special characters

maj_duma_clean <- lapply(X = maj_duma_clean, function(df){
  df$name <- gsub(pattern = "[\\d][[:punct:]]*", 
                  replacement = "", 
                  x = df$name, 
                  perl = TRUE)
  df$name <- gsub(pattern = "[[:punct:]]", x = df$name, 
                  replacement = "", perl = TRUE)
  df
}
)

#lets clean the UIK number

maj_duma_clean <- lapply(X = maj_duma_clean, function(df){
  df$uik_no <- str_remove(df$uik_no, pattern = '^УИК №')
  df
})

#candidate data
maj_duma_can <- candidate.cleaner('./data/duma/candidates Выборы депутатов Государственной Думы Федерального Собрания Российск.xls')

#write parties to key
write.csv(unique(maj_duma_can$party), 'duma_key.csv')

#read key back in
maj_duma_key <- read.csv('duma_key.csv')

#clean NAs into one hot
maj_duma_key %<>% replace_na(replace = list(opp = 0, comm = 0, ind= 0))

#let's do the merge again
maj_duma_clean <- do.call(rbind.data.frame, maj_duma_clean)

#fix typing
maj_duma_clean$uik_no <- as.factor(maj_duma_clean$uik_no)
maj_duma_clean$votes <- as.numeric(maj_duma_clean$votes)
maj_duma_clean$valid <- as.numeric(maj_duma_clean$valid)
maj_duma_clean$invalid <- as.numeric(maj_duma_clean$invalid)
maj_duma_clean$reg_voters <- as.numeric(maj_duma_clean$reg_voters)

#merge
maj_duma_can <- left_join(maj_duma_can, maj_duma_key, by = 'party')

#drop non-registered
non_reg <- which(maj_duma_can$status != 'выдвинут')
maj_duma_can <- maj_duma_can[-non_reg, ]

#41811
maj_duma_clean <- left_join(maj_duma_clean, maj_duma_can, by = 'name')
#good merge - let's calculate opposition

#opp calc
maj_duma_clean <- split(maj_duma_clean, f = maj_duma_clean$uik_no)

maj_duma_clean <- lapply(maj_duma_clean, opp.calculating)

maj_duma_clean <- do.call(rbind.data.frame, maj_duma_clean)

#checks
summary(maj_duma_clean$turnout)
summary(maj_duma_clean$opp_vote_pct)
summary(maj_duma_clean$comm_vote_pct)
summary(maj_duma_clean$ind_vote_pct)
#all good

#save
save(maj_duma_clean, file = './data/maj_duma_clean.RData')

##party list vote
p_duma_clean <- vote.cleaning('./data/duma/partyvote')

#get at uik level
p_duma_clean <- lapply(p_duma_clean, uik.converter)

p_duma_clean <- flatten(p_duma_clean)

p_duma_clean <- lapply(p_duma_clean, uik.cleaner)

#clean digits
p_duma_clean <- lapply(X = p_duma_clean, function(df){
  df$name <- gsub(pattern = "[\\d][[:punct:]]*", 
                  replacement = "", 
                  x = df$name, 
                  perl = TRUE)
  df$name <- gsub(pattern = "[[:punct:]]", x = df$name, 
                  replacement = "", perl = TRUE)
  df
}
)

#lets clean the UIK number

p_duma_clean <- lapply(X = p_duma_clean, function(df){
  df$uik_no <- str_remove(df$uik_no, pattern = '^УИК №')
  df
})

#write out key
p_duma_clean <- do.call(rbind.data.frame, p_duma_clean)

write.csv(unique(p_duma_clean$name), 'duma_p_key.csv')

#read in key
duma_p_key <- read.csv('duma_p_key.csv')
duma_p_key %<>% replace_na(replace = list(opp = 0, comm = 0, ind= 0))
duma_p_key$X <- NULL #forgot to delete this

#now let's clean the names in the dataset of punctuation
p_duma_clean$name <- gsub(pattern = "[\\d][[:punct:]]*", 
                replacement = "", 
                x = p_duma_clean$name, 
                perl = TRUE)
p_duma_clean$name <- gsub(pattern = "[[:punct:]]", x = p_duma_clean$name, 
                replacement = "", perl = TRUE)

#and merge
p_duma_clean <- left_join(p_duma_clean, duma_p_key, by = 'name')

#fix typing
p_duma_clean$uik_no <- as.factor(p_duma_clean$uik_no)
p_duma_clean$votes <- as.numeric(p_duma_clean$votes)
p_duma_clean$valid <- as.numeric(p_duma_clean$valid)
p_duma_clean$invalid <- as.numeric(p_duma_clean$invalid)
p_duma_clean$reg_voters <- as.numeric(p_duma_clean$reg_voters)

#resplit
p_duma_clean <- split(p_duma_clean, f = p_duma_clean$uik_no)

#opp calc
p_duma_clean <- lapply(p_duma_clean, opp.calculating)

p_duma_clean <- do.call(rbind.data.frame, p_duma_clean)

#checks
summary(p_duma_clean$turnout)
summary(p_duma_clean$opp_vote_pct)
summary(p_duma_clean$comm_vote_pct)
summary(p_duma_clean$ind_vote_pct)

#ind vote column is useless here as well - no ind votes
p_duma_clean$ind_vote_pct <- NULL

#saving
save(p_duma_clean, file = 'p_duma_clean.RData')

##FINAL DATASET OF ELECTORAL RESULTS--------------------------------------------
load('./data/maj_duma_clean.RData') 
load('./data/p_duma_clean.RData')
load('./data/mayor_clean.RData') 
load('./data/uik_df.RData')

#change colnames
new_m_col <- paste('m', colnames(mayor_clean), sep = '_')
colnames(mayor_clean) <- new_m_col

elecres_df <- left_join(uik_df, mayor_clean, by = c('uik_no' = 'm_uik_no'))

#mayor ind column is useless
elecres_df$m_ind_vote_pct <- NULL

#change colnames for duma maj
new_dm_col <- paste('dm', colnames(maj_duma_clean), sep = '_')
colnames(maj_duma_clean) <- new_dm_col

elecres_df <- left_join(elecres_df, maj_duma_clean, by = c('uik_no' = 'dm_uik_no'))

#change colnames for duma party
new_dp_col <- paste('dp', colnames(p_duma_clean), sep = '_')
colnames(p_duma_clean) <- new_dp_col

#final merge
elecres_df <- left_join(elecres_df, p_duma_clean, by = c('uik_no' = 'dp_uik_no'))

#looking for weirdness
apply(elecres_df, 2, function(x) sum(is.na(x)))

#looks fine - some missigness for mayor due to new election districts being added
save(elecres_df, file = './data/elecres_df.RData')


##ADDING CENSUS DATA------------------------------------------------------------

#first I need to get the municipal formation
#this is in the elec_name variable, so we can use our old friend
#regexes to get it out

elecres_df$elec_name <- str_remove(elecres_df$elec_name,
                                    pattern = 'Выборы депутатов Совета депутатов муниципального округа ')

elecres_df$elec_name <- str_remove(elecres_df$elec_name,
                                   pattern = ' в городе Москве')

elecres_df$elec_name <- str_remove(elecres_df$elec_name,
                                   pattern = 'Выборы депутатов Совета депутатов городского округа ')

elecres_df$elec_name <- str_remove(elecres_df$elec_name,
                                   pattern = 'Выборы депутатов муниципального Собрания внутригородского муниципального образования ')

#all done - save
save(elecres_df, file = './data/elecres_df.RData')

##read in census data
load('./data/census_df.RData')

#lets do some cleaning of names to make matches easier
elecres_df$elec_name <- str_remove_all(string = elecres_df$elec_name,
                                       pattern = "[[:punct:]]")
census_df$name <- str_remove_all(string = census_df$name,
                                 pattern = "[[:punct:]]")
elecres_df$elec_name <- gsub(pattern = '[a-zA-z]*ё',
                             x = elecres_df$elec_name,
                             replacement = 'е')

elecres_df$elec_name <- tolower(elecres_df$elec_name)
census_df$name <- tolower(census_df$name)

#alright let's check
elecres_names <- unique(elecres_df$elec_name)
census_names <- unique(census_df$name)

setdiff(elecres_names, census_names)

#looks good for a merge
analysis_df <- left_join(elecres_df, census_df, by = c('elec_name' = 'name'))

#need to join in average monthly pay data
load('./data/pay_clean.RData')

#lets do some cleaning of names to make matches easier
pay.df$name <- str_remove_all(string = pay.df$name,
                                 pattern = "[[:punct:]]")
pay.df$name <- gsub(pattern = '[a-zA-z]*ё',
                             x = pay.df$name,
                             replacement = 'е')
pay.df$name <- tolower(pay.df$name)

elecres_names <- unique(analysis_df$elec_name)
pay_names <- unique(pay.df$name)

setdiff(elecres_names, census_names)

#good to go!
pay.df$pay <- as.numeric(pay.df$pay)
analysis_df <- left_join(analysis_df, pay.df, by = c('elec_name' = 'name'))

#need to join in the data on mean distance to renovated buildings
load('./data/mean_dist_ren.RData')

analysis_df <- left_join(analysis_df, mean_dist_ren, by = 'uik_no')

#saving
save(analysis_df, file = './data/final_df.RData')

## REGION LEVEL CODING ---------------------------------------------------------
## let's code up to region level to make analysis easer

#need region key
#write.csv(unique(analysis_df$elec_name), file = 'region_key.csv')

#read back in
region_key <- read.csv(file = 'region_key.csv')


#merge
analysis_df <- left_join(analysis_df, region_key, by = c('elec_name' = 'mf'))


#saving UNAGGREGATED data set for use in final section
saveRDS(analysis_df, file = './data/unagg_final_df.rds')

#need to aggregate census data at regional level

analysis_df %<>% group_by(region) %>% 
  mutate_at(c('higher_ed', 'govt_dep', 'pay'), mean, na.rm = T) %>%
  mutate_at(c('pop_total'), sum)

## MERGING IN KREMLIN DISTANCE -------------------------------------------------

load('./data/kremlin_dist.RData')

colnames(kremlin_dist)[1] <- 'uik_no'

kremlin_dist[ , 1] <- as.character(kremlin_dist[ , 1])

analysis_df <- left_join(analysis_df, kremlin_dist, by = 'uik_no')

#saving
save(analysis_df, file = './data/final_df.RData')
