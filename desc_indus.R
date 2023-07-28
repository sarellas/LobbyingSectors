library(tidyverse)
library(data.table)
library("tidylog")
library(here)
options(scipen = 999)  
library(scales)


####### Estatísticas Descritivas

### Industries

lob_indus <- read_delim(here("data","os_lobby","lob_indus.txt"),  
                        delim = ",",  
                        col_names = c("client", "sub", "total", "year", "catcode"),  
                        quote = "|")

lob_indus %>%
  group_by(year) %>%
  distinct(client) %>%
  tally() %>%
  summarize(total = sum(n)) ### ainda não chega ao total... pq? 

lob_indus %>%
  filter(year != 2023) %>%
  group_by(year) %>%
  distinct() %>%
  tally() %>%
  ggplot() +
  geom_col(aes(x = year, y = n), colour = "black", fill = "lightblue") +
  ggtitle("Number of industries lobbying by year") +
  ylab("# industries") +
  theme_minimal()

lob_indus %>%
  filter(year != 2023) %>%
  group_by(year) %>%
  summarize(total_year = sum(total)) %>%
  ggplot() +
  geom_col(aes(x = year, y = total_year), colour = "black", fill = "gold") +
  ggtitle("Total spent by industries in lobbying by year") +
  ylab("Total (US$)") +
  scale_y_continuous(labels = label_number(suffix = " T", scale = 1e-9)) +
  theme_minimal()    # é diferente do total na tabela lobbying


# Variável catcode está nessa base e na lob_lobbying. 
file1 <- "https://www.opensecrets.org/downloads/crp/CRP_Categories.txt"
catcode <- fread(file1)

catcode %>%
  distinct(Sector)

# juntar legenda das categorias com base lob_indus

lob_indus <- 
  lob_indus %>%
  rename(Catcode = catcode) %>%
  left_join(catcode, by = "Catcode")

lob_indus %>%
  filter(year == 2022) %>%
  group_by(Sector) %>%
  tally() %>%
  ggplot() +
  geom_col(aes(x = Sector, y = n), colour = "black", fill = "coral") +
  ggtitle("Reports by industry sector in 2022") + 
  ylab("number of reports") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        axis.text = element_text(size = 8))

lob_indus %>%
  filter(year == 2022) %>%
  group_by(Sector) %>%
  summarize(total = sum(total)) %>%
  ggplot() +
  geom_col(aes(x = Sector, y = total), colour = "black", fill = "coral") +
  ggtitle("Total spent by industry sector in 2022") + 
  ylab("Total spend") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, size = 7),
        axis.text = element_text(size = 8))








