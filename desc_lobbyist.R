library(tidyverse)
library(data.table)
library("tidylog")
library(here)
options(scipen = 999)  


######## Estatísticas descritivas (24/7/23)

#### Lobistas (base: lob_lobbyist)

lob_lobbyist <- read_delim(here("data","os_lobby","lob_lobbyist.txt"),  
                           delim = ",",  
                           col_names = c("uniqid", "lobbyist_raw", "lobbyist", "lobbyist_id", "year",
                                         "offic_position", "CID", "formercongmem"),  
                           quote = "|")

lob_lobbyist<-  # corrigindo problema de notação
lob_lobbyist %>% 
  mutate(formercongmem=case_when(formercongmem==",n"~"n",
                                 TRUE~formercongmem))

lob_lobbyist %>%
  distinct(uniqid) %>%
  tally() 

lob_lobbyist %>% 
  filter(year == 2022) %>%
  group_by(formercongmem) %>% 
  tally()

lob_lobbyist %>% 
  ggplot() +
  geom_bar(aes(x = year, fill = formercongmem))

lob_lobbyist %>% 
  filter(formercongmem == "y" & year != 2023) %>%
  ggplot() +
  geom_bar(aes(x = year), colour="black", fill="orange") +
  ggtitle("Number of former congress member lobbyists by year") +
  ylab(" ") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_minimal()

lob_lobbyist %>% 
  filter(year != 2023) %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_line(aes(x = year, y=n))



