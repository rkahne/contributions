library(tidyverse)
library(anytime)
library(lubridate)
library(stringr)
library(scales)
library(zoo)
library(treemap)
library(forcats)
library(d3Tree)
library(treemapify)

contributions <- read_delim('contributions_janjun2014.txt', delim = ';', col_names = F) %>% 
  rbind(read_delim('contributions_juldec2014.txt', delim = ';', col_names = F)) %>% 
  rbind(read_delim('contributions_janjun2015.txt', delim = ';', col_names = F)) %>% 
  rbind(read_delim('contributions_juldec2015.txt', delim = ';', col_names = F)) %>%
  rbind(read_delim('contributions_janjun2016.txt', delim = ';', col_names = F)) %>%  
  rbind(read_delim('contributions_juldec2016.txt', delim = ';', col_names = F)) %>% 
  rbind(read_delim('contributions_full2017.txt', delim = ';', col_names = F)) %>% 
  filter(X1 != 'BCF', X13 != 'TOTAL', X13 != 'IN-KIND RECD TOTAL')

names(contributions)<-c('Transaction Type','Election Date','Office Sought','Location of Office','Name Prefix','Last Name','First Name','Middle Name','Name Suffix','Party Affiliation','Report Due Date','Statement Type','Name','Address1','Address 2','City','State','Zip','Transaction Category','Form of Transaction','Occupation','Employer','Congressional District','Transaction Amount','Date of Transaction',"Spouse's Name","Spouse's Occupation","Spouse's Employer",'Organization Name','N/A','Transaction Mode')
write_csv(contributions, 'KREF_Contributions_2014_2017.csv')


contributions %>% 
  mutate(
    `Full Name` = pmap_chr(list(`Name Prefix`,`First Name`,`Last Name`), function(prefix,first,last){
      if(is.na(prefix)) paste(first, last)
      else paste(prefix, first, last)
    }),
    year = year(anytime(`Date of Transaction`))
  ) %>% 
  filter(`Office Sought` == 'STATE REPRESENTATIVE') %>% 
  group_by(year, `Full Name`, Name) %>% 
  summarize(total = sum(`Transaction Amount`)) %>% 
  filter(year == 2016, !is.na(Name), !is.na(`Full Name`), `Full Name` != 'N/A', total > 5000) %>% 
  ggplot(aes(Name, total, fill = `Full Name`, label = total)) +
  geom_bar(stat = 'identity', position = 'stack') +
  # geom_label() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  ggtitle('State House Contributors > $5,000')

contributions_m <- contributions %>% 
  mutate(
    `Full Name` = pmap_chr(list(`Name Prefix`,`First Name`,`Last Name`), function(prefix,first,last){
      if(is.na(prefix)) paste(first, last)
      else paste(prefix, first, last)
    }),
    year = year(anytime(`Date of Transaction`))
  ) 

contributions_m %>% 
  filter(year == 2016, !is.na(`Office Sought`)) %>% 
  group_by(`Office Sought`) %>% 
  summarize(Total = sum(`Transaction Amount`)) %>%
  filter(Total > 100000) %>% 
  ggplot(aes(`Office Sought`, Total, label = dollar_format()(Total))) +
  geom_bar(stat = 'identity') +
  geom_label() +
  coord_flip() +
  ggtitle('Total Amount Raised by Office Sought (>$100,000)')

name_text = 'S. COAN'


contributions_m %>% 
  filter(is.na(`Office Sought`)) %>% 
  write_csv('no_office_sought_2014_2017.csv')

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>% 
  group_by(Name, `Organization Name`, `Form of Transaction`) %>%
  summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
  arrange(`Total Given` %>% desc()) %>% 
  treemap(index = 'Name', vSize = 'Total Given')

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>% 
  group_by(Name, `Organization Name`, `Form of Transaction`) %>%
  summarize(`Total Given` = sum(`Transaction Amount`)) %>%
  ungroup() %>% 
  summarize(`Number of Donors` = n())

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>% 
  group_by(Name, `Organization Name`, `Form of Transaction`) %>%
  summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
  ungroup() %>% 
  summarize(`Mean Contributior Amount` = mean(`Total Given`))

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>% 
  group_by(Name, `Organization Name`, `Form of Transaction`) %>%
  summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
  ungroup() %>% 
  summarize(`Median Contributior Amount` = median(`Total Given`))

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>% 
  group_by(Name, `Organization Name`, `Form of Transaction`) %>%
  summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
  arrange(`Total Given` %>% desc()) %>% 
  ggplot(aes(x =`Total Given`)) + 
  geom_histogram(binwidth = 500) +
  scale_x_continuous(labels = dollar_format()) +
  labs(title = 'Total Given Histogram', x = '')

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>% 
  group_by(Name, `Organization Name`, `Form of Transaction`) %>%
  summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
  arrange(`Total Given` %>% desc()) %>% 
  ggplot(aes(x= 1, y =`Total Given`, fill = 'A')) + 
  geom_violin(show.legend = F) +
  geom_boxplot(aes(fill = 'B', alpha = 0.6), width = 0.1, show.legend = F) +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = c('dodgerblue', 'grey')) +
  labs(title = 'Total Given Violin Plot', x = '') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip()

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>%
  summarize(`Total Given` = sum(`Transaction Amount`))

contributions_m %>% 
  filter(`Full Name` == name_text, `Form of Transaction` != 'IN KIND') %>% 
  group_by(Name, `Organization Name`, `Form of Transaction`) %>%
  summarize(`Total Given` = sum(`Transaction Amount`)) %>% 
  arrange(`Total Given` %>% desc())


treemapify(df)


contributions %>% 
  filter(election_year == 2016,
         `Office Sought` == 'CITY COUNCIL MEMBER',
         `Last Name` == 'SCHLOSSER') %>% View()
