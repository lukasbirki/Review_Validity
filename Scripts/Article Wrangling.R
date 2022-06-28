install.packages("revtools")
library(revtools)
library(tidyverse)


#Screening Round 1: LitsearchR-search

lit_round_1 <- readr::read_csv("Bib_files/final_search_results.csv") %>% 
  drop_na(Abstract) %>% 
  mutate(abstract_join = tolower(Abstract))
coded_round_1 <- readr::read_csv("Bib_files/abstract_coded_round_1.csv") %>% drop_na(abstract) %>% 
  select(abstract, code = "lukas.birkenmaier@gesis.org") %>% 
  mutate(abstract_join = tolower(abstract))


names(lit_round_1)
names(coded_round_1)

dplyr::anti_join(lit_round_1, coded_round_1, by = c("Abstract"= "abstract")) -> missing_studies

#https://www.rdocumentation.org/packages/stringdist/versions/0.9.8/topics/stringdist-metrics
fuzzyjoin::stringdist_anti_join(lit_round_1, coded_round_1, by = "abstract_join", max_dist = 5) %>% select(abstract_join) -> missing_studies %>% select(abstract_join) -> s

is.na(lit_round_1$Abstract)

nrow(lit_round_1) - nrow(coded_round_1)
