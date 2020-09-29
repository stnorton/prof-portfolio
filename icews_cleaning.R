###################
## DATA CLEANING ##
## SEAN NORTON   ##
## 2.4.18        ##
###################

#this script fetches ICEWS data and cleans it into a dataframe

## LIBRARY ---------------------------------------------------------------------

library(tidyverse) #data wrangling
library(magrittr) #piping
library(DBI) #sql
library(lubridate) #date operations
library(scales) #date formats
library(readstata13)


theme_stn <- function(){
  theme_bw()+
    theme(plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
}

##FETCH FUNCTION ---------------------------------------------------------------

#define a function to fetch the countries I need from the sql database
# then select only protest, then return result

sql.fetcher <- function(country){
  
  #safety check
  stopifnot(is.character(country))
  
  #open connection
  con <- dbConnect(SQLite(), dbname = "icews.sqlite")
  
  #need country to be in quotes
  country <- paste0('"', country, '"')
  
  #build query
  query <- paste('SELECT * FROM public WHERE Country == ', country, 
                 'AND Source_Country == ', country)
  
  #send query
  res <- dbGetQuery(con, query)
  
  #error handling
  if(nrow(res) == 0){
    warning(paste(country, "was not found in database. Returning NULL."))
    return(NULL)
  }
  
  #coerce CAMEO codes to character
  res$CAMEO_Code <- as.character(res$CAMEO_Code)
  
  #create logical vector indicating protests
  protest <- startsWith(res$CAMEO_Code, '14')
  
  #subset
  res <- res[protest, ]
  
  #disconnect
  dbDisconnect(con)
  
  #return
  return(res)
  
}

# let's get unique name of countries to check against , write to text

write.csv(file = 'country_list.csv', 
          x = dbGetQuery(con, 'SELECT DISTINCT(Country) FROM public ;'))

#vector of countries
countries <- c('Australia', 'Austria', 'Belgium', 'Bulgaria', 'Canada',
               'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 
               'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland',
               'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg',
               'Malta', 'Netherlands', 'New Zealand', 'Norway', 'Poland',
               'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 
               'Sweden', 'Switzerland', 'United Kingdom')

# apply function
dataframes <- lapply(countries, sql.fetcher)

#call into one dataframe
icews_df <- do.call(rbind.data.frame, dataframes)

## CLEANING --------------------------------------------------------------------

#unfortunately we ended up with a lot of CAMEO code 014 because R removed the 
# leading zero

#will remove

policy <- which(icews_df$Event_Text == 'Consider policy option')

#removing
icews_df <- icews_df[-policy, ]

#sanity check
unique(icews_df$Event_Text)

#save raw data
write.csv(icews_df, file = './data/icews_raw.csv')

# let's get counts by year
icews_df$Event_Date <- ymd(icews_df$Event_Date)

count_df <- icews_df %>% group_by(Country, year(Event_Date)) %>% summarize(n_event = n())

colnames(count_df) <- c('country', 'year', 'n_event')

#ungroup
count_df %<>% ungroup()

#save
write.csv(count_df, file = './data/count_df.csv')

#let's graph them

count_df$year <- ymd(count_df$year, truncated = 2L)


pdf(file = './plots/all_countries_trends.pdf')

ggplot(data = count_df, aes(x = year, y = n_event)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'blue', size = 0.5) +
  theme_stn() +
  facet_wrap(~country) +
  scale_x_date(date_labels = '%y') +
  labs(x = 'Year', y = '# Events') 

dev.off()

## NB: KAITLIN MODIFIED THIS DATA USING STATA AND MERGED WITH THE RAI DATA
# File name of Stata file:

## CONTROLS---------------------------------------------------------------------

#read in
controls <- read.dta13('./cleaned_data/oecd_controlvars.dta') #Kaitlin created
protest <- read.dta13('./cleaned_data/clean_protestdata.dta') #Kaitlin created

#group protest data by year
protest_df <- protest %>% group_by(rcode, year, rai_id) %>%
  count()


#merge controls
protest_df <- left_join(controls, protest_df, by = c('rcode', 'year'))

#merge countries back in
protest_df$country <- protest$country[match(protest_df$rcode, protest$rcode)]

#need iso3 numbers for inflation
protest_df$iso3n <- protest$iso3n[match(protest_df$rcode, protest$rcode)]

#merge inflation in 
inflation <- read.dta13('./cleaned_data/inflation_merged.dta')
protest_df <- left_join(protest_df, inflation, by = c('iso3n', 'year'))

#merge surface area in
area <- read.dta13('./cleaned_data/oecd_natarea.dta')
protest_df <- left_join(protest_df, area, by = c('iso3n', 'year'))

# RAI---------------------------------------------------------------------------
rai <- read.dta13('./cleaned_data/rai_data.dta')

protest_df <- left_join(protest_df, rai, by = c('year', 'iso3n'))

# change protest NAs to zeroes
nas <- which(is.na(protest_df$n))
protest_df[nas, 'n'] <- 0

#merge countries back in

protest_df$country <- protest$country[match(protest_df$rcode, protest$rcode)]

# MISSINGNESS-------------------------------------------------------------------

#write out full in case I need it
write.csv(protest_df, file = './cleaned_data/full_yearly_df.csv')

#subest only to necessary years
protest_df <- protest_df[which(protest_df$year >= 2000 & protest_df$year <= 2010), ]

#counts per country - region years
ry_counts <- protest_df %>% group_by(rcode) %>% count()

#find regions with high degree of missingness
missing_ry <- protest_df %>% group_by(rcode) %>% summarize_all(funs(sum(is.na(.))))

high_miss <- which(missing_ry$oecd_unemp > 7)


#get country codes
co <- unique(substring(high_miss_ry, 1, 2))

#compare to protest dataframe

#cut out n_rai
miss_rai <- which(is.na(protest_df$n_RAI))
high_miss_ry <- protest_df[miss_rai, 'rcode', drop = T]

#get country codes
co <- unique(substring(high_miss_ry, 1, 2))

#cut them out
missing <- sapply(co, function(s) grep(pattern = s, x = protest_df$rcode))
missing <- unlist(missing)

#cut out missing countries
protest_df <- protest_df[-missing, ]

#what's left?
unique(protest_df$country)

#save
write.csv(protest_df, file = './cleaned_data/no_miss_data.csv')

