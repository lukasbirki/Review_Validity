library(revtools)
library(tidyverse)


#Screening Round 1: LitsearchR-search

lit_round_1 <- readr::read_csv("review/data/literature_unfiltered.csv") %>% 
  drop_na(Abstract) %>% 
  mutate(abstract_join = tolower(Abstract))

coded_round_1 <- readr::read_csv("review/data/literature_coded_abstracts.csv") %>% drop_na(abstract) %>% 
  select(abstract, code = "lukas.birkenmaier@gesis.org") %>% 
  mutate(abstract_join = tolower(abstract)) %>% 
  filter(code == 1 | code == 0 )

#https://www.rdocumentation.org/packages/stringdist/versions/0.9.8/topics/stringdist-metrics
fuzzyjoin::stringdist_left_join(coded_round_1, lit_round_1, by = "abstract_join", max_dist = 5) -> df_combined

write_csv(df_combined, "Review/data/list_doi_import_zotero.csv")

#Note: 16 (positively coded) papers were not matched by the algorithm and need to be manually imported in Zotero
df_combined %>% 
  filter(is.na(Authors)) -> overview_missing_studies



##################################################

#To calculate summary statistics, the bib file of the zotero-export will be used (containing the 16 manually imported studies)

df_coded <- read.csv("review/data/Bib_files/complete_list_coded.csv", sep = ",", encoding = "UTF-8")

#Get unique list of names from authors

df_coded %>% 
  select(Author) %>% 
  tidyr::separate_rows(Author, sep = ";") %>% 
  mutate(authors_new = trimws(Author),
         authors_new = tolower(authors_new)) %>% #Removing leading and/or trailing white spaces
  select(authors_new) %>% 
  count(authors_new) %>% 
  arrange(desc(n)) -> df_authors #n=419 authors with 1-5 papers in the selected list

write.csv(df_authors, "Interviews/list_authors.csv")

#Plots

readr::read_csv("Review/data/literature_coded_abstracts.csv") %>% drop_na(abstract) %>% 
  select(abstract, code = "lukas.birkenmaier@gesis.org",journal) %>% 
  mutate(abstract_join = tolower(abstract)) %>%
  ggplot(aes(x=reorder(journal, table(journal)[journal]))) +
  geom_bar() +
  coord_flip() +
  xlab("") +
  theme_bw()

ggsave("Review/plots/v0_journal_distribution.jpeg")

df_coded  %>% 
  select(Publication.Title) %>% 
  ggplot(aes(x=reorder(Publication.Title, table(Publication.Title)[Publication.Title]))) +
  geom_bar() +
  coord_flip() +
  xlab("") +
  theme_bw()

ggsave("Review/plots/v1_journal_distribution.jpeg")

PRISMAstatement::prisma(found = 912,
                        found_other = 0,
                        no_dupes = T, 
                        screened = 912, 
                        screen_exclusions = 0, 
                        full_text = 0,
                        full_text_exclusions = 0, 
                        qualitative = 0, 
                        quantitative = 0,
                        width = 800, height = 800)
ggsave("./Figures/PRISMA.png")

