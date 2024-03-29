library(litsearchr)
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)  
library(PRISMAstatement)
library(bibliometrix)

#Resources
#https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html#Network_analysis

#Keywords for the naive search

### ((Politi* OR Party OR Govern*) AND text*

#Importing Search Results

##From Keywords

naiveresults <- litsearchr::import_results(file = "./Bib_files/litsearchR/naive_search.ris", verbose = TRUE) %>% 
  litsearchr::remove_duplicates(., field = "title", method = "string_osa")

nrow(naiveresults)
  
sum(is.na(naiveresults[, "keywords"]))

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

title_keywords <- litsearchr::extract_terms(text=naiveresults[, "title"], method="fakerake", min_freq=3, min_n=2)


all_keywords <- unique(append(taggedkeywords, title_keywords))

###Note: For now, getting search terms from Keywords appears to be sufficient

## Network Analysis of Search terms

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 2,
    min_occ = 2
  )

cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .80,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

ggraph(reducedgraph, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE) 

ggsave("./plots/network_graph.png")

strengths <- strength(reducedgraph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths

ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

ggsave("./plots/strengts_cutoffs.png")

selected_terms_without_keywords <- get_keywords(reducedgraph)

##Adding single keywords
extra_terms <- c(
  "text",
  "scaling"
, "dictionary",
"plenary debate")

selected_terms <- c(selected_terms_without_keywords, extra_terms)

selected_terms

# Writing new search query
## MANUAL!!!!
grouped_terms <-list(
  Political_Science_Reserach=selected_terms[c(6, 7, 8,11,15,21 )],
  text_analysis=selected_terms[c(1,2,3,4,5,9, 10,12,13,14,16,17,18,19,20 )])

grouped_terms

#Writing Search

write_search(
  grouped_terms,
  languages="English",
  exactphrase=TRUE,
  stemming=T,
  closure="left",
  writesearch=TRUE
)

cat(read_file("./search_terms.txt"))

#Generate PRISMA Flow Chart from Naive Search Results

PRISMAstatement::prisma(found = nrow(naiveresults),
       found_other = 123,
       no_dupes = 0, 
       screened = 0, 
       screen_exclusions = 0, 
       full_text = 0,
       full_text_exclusions = 0, 
       qualitative = 0, 
       quantitative = 0,
       width = 800, height = 800)
ggsave("./Figures/PRISMA.png")