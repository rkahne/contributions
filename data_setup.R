library(tidyverse)
library(anytime)
library(lubridate)
library(stringr)
library(scales)
library(zoo)
library(treemap)
library(forcats)

contributions <- read_delim('contributions_janjun2014.txt', delim = ';', col_names = F) %>% 
  rbind(read_delim('contributions_juldec2014.txt', delim = ';', col_names = F)) %>% 
  rbind(read_delim('contributions_janjun2015.txt', delim = ';', col_names = F)) %>% 
  rbind(read_delim('contributions_juldec2015.txt', delim = ';', col_names = F)) %>%
  rbind(read_delim('contributions_janjun2016.txt', delim = ';', col_names = F)) %>%  
  rbind(read_delim('contributions_juldec2016.txt', delim = ';', col_names = F)) %>% 
  rbind(read_delim('contributions_full2017.txt', delim = ';', col_names = F)) %>% 
  filter(X1 != 'BCF', X13 != 'TOTAL', X13 != 'IN-KIND RECD TOTAL')

names(contributions)<-c('Transaction Type','Election Date','Office Sought','Location of Office','Name Prefix','Last Name','First Name','Middle Name','Name Suffix','Party Affiliation','Report Due Date','Statement Type','Name','Address1','Address 2','City','State','Zip','Transaction Category','Form of Transaction','Occupation','Employer','Congressional District','Transaction Amount','Date of Transaction',"Spouse's Name","Spouse's Occupation","Spouse's Employer",'Organization Name','N/A','Transaction Mode')
# write_csv(contributions, 'KREF_Contributions_2014_2017.csv')

## PARSER ABOVE SAVES FILES WITH ';' IN PLACES.  NEED TO MANUALLY FIX IN NOTEPAD++ OR ANOTHER TEXT EDITOR. ##

contributions <- read_csv('KREF_Contributions_2014_2017.csv') %>% 
  filter(!is.na(`Election Date`))  %>% 
  mutate(
    full_name = pmap_chr(list(`First Name`, `Middle Name`, `Last Name`), function(first,middle,last){
      if(is.na(middle)) paste(first, last)
      else paste(first, middle, last)
    }),
    `Election Date` = anytime(`Election Date`),
    election_type = map_chr(`Election Date`, function(i){
      mon <- month(i)
      if(mon == 5) 'Primary'
      else if(mon == 11) 'General'
      else 'Special'
    }),
    election_year = year(`Election Date`)
  ) 

write_csv(contributions, 'contribution_data_for_app.csv')
