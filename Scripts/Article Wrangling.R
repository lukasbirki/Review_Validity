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

#https://www.rdocumentation.org/packages/stringdist/versions/0.9.8/topics/stringdist-metrics
fuzzyjoin::stringdist_join(lit_round_1, coded_round_1, by = "abstract_join", max_dist = 5) -> df_combined


#Testing, that the same amount of positively coded studies is included
df_combined %>% 
  filter(code == 1 | code == 0) %>% 
  select(doi = "DOI Link", authors = Authors, title = "Article Title", journal = "Source Title")-> df_coded

#Get unique list of names from authors

df_coded %>% 
  mutate(authors_new = strsplit(as.character(.$`Author Full Names`), ";")) %>% 
  unnest(authors_new) %>% 
  select(authors_new) %>% 
  count(authors_new) %>% 
  arrange(desc(n)) -> df_authors #n=268 authors with 4 - 1 papers in the selected list

write.csv(df_authors, "data/list_authors.csv")


#N = 99, same as in the Excel Export


df_coded_2 %>%
  ggplot(aes(x=reorder(journal, table(journal)[journal]))) +
  geom_bar() +
  coord_flip() +
  xlab("") +
  theme_bw()

ggsave("plots/journal_distribution.jpeg")

df_combined %>% 
  select(doi = "DOI Link", authors = Authors, title = "Article Title", journal = "Source Title") %>% 
  ggplot(aes(x=reorder(journal, table(journal)[journal]))) +
  geom_bar() +
  coord_flip() +
  xlab("") +
  theme_bw()


