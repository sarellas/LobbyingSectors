library(tidyverse)
library(data.table)
library("tidylog")
library(here)


###### Estatísticas Descritivas

### Bases: lob_bills e lob_issue

### Bills lobbied

lob_bills <- read_delim(here("data","os_lobby","lob_bills.txt"), 
                        col_names = c("b_id", "SI_ID", "congno", "billname"),  
                        quote = "|")

lob_bills %>% 
  distinct(b_id) %>%
  tally()


colSums(is.na(lob_bills)) ### não tem NAs 

lob_bills <- fread(here("data","os_lobby","lob_bills.txt"))

### Issue

lob_issue <- read_delim(here("data","os_lobby","lob_issue.txt"),  
                        delim = ",",  
                        col_names = c("SI_ID", "uniqid", "issueID", "issue", 
                                      "specificissue", "year"),  
                        quote = "|")

colSums(is.na(lob_issue))

lob_issue %>%
  distinct(uniqid) %>%
  tally()

lob_issue %>%
  distinct(SI_ID) %>%
  tally()







