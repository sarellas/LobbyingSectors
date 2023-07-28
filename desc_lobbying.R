library(tidyverse)
library(data.table)
library("tidylog")
library(here)
options(scipen = 999)  

###### Estatísticas Descritivas (25/7/23)

### Base: lob_lobbying.txt

lob_lobbying <- read_delim(here("data","os_lobby","lob_lobbying.txt"),  
                           delim = ",",  
                           col_names = c("uniqid", "registrant_raw", "registrant", 
                                         "isfirm", "client_raw", "client", "ultorg",
                                         "amount", "catcode", "source", "self", 
                                         "includeNSFS", "use", "ind", "year", "type",
                                         "typelong", "affiliate"),  
                           quote = "|")

lob_lobbying <-    ## usar apenas obs que não são repetidas
  lob_lobbying %>%
  filter(use == "y")

lob_lobbying %>%
  distinct(uniqid) %>%
  tally()

## número de registrants em 2022: 4.9 mil

lob_lobbying %>%
  filter(year == 2022) %>%
  distinct(registrant) %>%
  count()

## número de registros em 2022: 77 mil
  
lob_lobbying %>%
  filter(year == 2022) %>%
  count()

## quantos reports foram feitos em 2022 por firmas de lobby?

lob_lobbying %>% 
  filter(year == 2022) %>%
  group_by(isfirm) %>%
  tally() %>%
  mutate(pct_isfirm = 100*(n/sum(n)))

## 85% dos reports de lobby foram feitos por firmas de lobby em 2022.


## Montante gasto em lobby por ano 

lob_lobbying %>%
  group_by(year) %>%
  summarize(amount_year = sum(amount))  ## valores estão estranhos

lob_lobbying %>%
  filter(year != 2023) %>%
  mutate(isfirm = case_when(isfirm == "y" ~ "lobbying firm", 
                            TRUE ~ "not lobbying firm")) %>%
  rename(`Firm type` = isfirm) %>%
  group_by(year, `Firm type`) %>%
  summarize(amount_year = sum(amount)) %>%
  ggplot() + 
  geom_col(aes(x = year, y = amount_year, fill = `Firm type`)) +
  ggtitle("Total amount invested in lobby by year, by type of firm") +
  ylab("Total amount (US$)") +
  theme_minimal() 

### mesmo que as firmas de lobby sejam responsáveis pela maioria dos reports
### elas não são responsáveis pela maior parte do dinheiro investido em lobby.


#### (27/07/23) Usar inner join com base lobbyist pra ver se o identificador 
# uniqid é igual

lob_lobbyist <- fread(here("data", "os_lobby", "lob_lobbyist.txt"), quote = "|")

lob_lobbyist %>% 
  rename(uniqid = V1) %>%
  inner_join(lob_lobbying, by = "uniqid")

lob_issue <- read_delim(here("data","os_lobby","lob_issue.txt"),  
                        delim = ",",  
                        col_names = c("SI_ID", "uniqid", "issueID", "issue", 
                                      "specificissue", "year"),  
                        quote = "|")

lob_lobbying %>%
  inner_join(lob_issue, by = "uniqid")

lob_lobbyist %>%
  rename(uniqid = V1) %>%
  inner_join(lob_issue, by = "uniqid") # algo errado 

