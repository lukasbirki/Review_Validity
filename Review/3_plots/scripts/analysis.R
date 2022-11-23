library(tidyverse)
library(gridExtra)

df <- readxl::read_xlsx("Review/3_plots/data/results_corrected.xlsx") %>% 
  filter(`E-Mail-Adresse` == "lukas.birkenmaier@outlook.de") %>% 
  mutate(id = paste(stringr::word(.$`This is the title of the Study`,1),
                    stringr::word(.$`These are the Authors`,1),sep = "_")) %>% 
  dplyr::rename(source = `What is the data source?`,
          language = `What is the data language?`,language_others = `If others: Name all languages in the data? If there is more than one language assessed, please separate languages by a comma (,)` ,
          method_type = `What Type(s) of method are initially applied?`,
          method_name = `What is the name of the method? If there is more than one method applied, please separate methods by a comma (,)`) %>% 
  drop_na(source)

#No dublicates
length(unique(df$id)) == nrow(df)

#Creating variable for count of validation steps per publication

df_validation <- df %>% select(ends_with("_v1"))
df$n_validation_steps <- rowSums(!is.na(df_validation))

#Changing into long format

df %>%  
  mutate(across(a_v1:f_v4, as.character)) %>% 
  pivot_longer(cols = a_v1:f_v4)  %>%  
  mutate(location = sub('.*(a|b|c|d|e|f).*', '\\1', name), 
         name  = sub('_?(a|b|c|d|e|f)_?', '', name)) %>% 
  pivot_wider() -> df_plot

#Plots

## Data Source
df_plot %>% 
  distinct(id, source) %>% 
  separate_rows(source,sep = ",") %>% mutate(source = trimws(source)) %>% 
  ggplot(aes(x=reorder(source, table(source)[source]))) +
  geom_bar() +
  coord_flip() +
  labs(title = "Data Sources", x = "Data", y = "n") +
  scale_colour_Publication()+
  theme_Publication() -> barchart_source

ggsave(plot =barchart_source ,filename = "Review/3_plots/output/barchart_source.png",width = 13, height = 6)

## Language

df_plot %>% 
  mutate(language_combined = case_when(
    language == "English"~ language,
    T ~ language_others)) %>% 
  distinct(id, language_combined) %>% 
  separate_rows(language_combined,sep = ",") %>% mutate(language_combined = trimws(language_combined)) %>% 
  ggplot(aes(x=reorder(language_combined, table(language_combined)[language_combined]),fill = factor(language_combined))) +
  geom_bar() +
  coord_flip() +
  labs(title = "Languages Analzed", x = "OOLanguage", y = "n", fill = "Language") +
  theme_Publication() ->barchart_language

ggsave(plot =barchart_language,"Review/3_plots/output/barchart_language.png", width = 13, height = 6)

## Method Type
df_plot %>% 
  distinct(id, method_type) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
                               T ~ "Others")) %>% 
  ggplot(aes(x=reorder(method_short, table(method_short)[method_short]),fill = factor(method_short))) +
  geom_bar() +
  coord_flip() +
  labs(title = "Method Type", x = "Type", y = "n", fill = "Method") +
  theme_Publication()+theme(axis.title.y = element_blank()) -> barchart_method

ggsave(plot = barchart_method,"Review/3_plots/output/barchart_method.png", width = 13, height = 6)

#Summary Plot

grid.arrange(barchart_source,barchart_method,nrow=1) -> arranged
ggsave(plot = arranged,"Review/3_plots/output/arranged_method_language.png", width = 13, height = 6)

#Count of validation steps

df_plot %>% 
  distinct(id, n_validation_steps) %>% 
  ggplot(aes(x=n_validation_steps,fill = factor(n_validation_steps))) +
  geom_bar()+
  labs(title = "Number of Validation Steps", x = "Validation Steps", y = "n", fill = "Number of Validation Steps") +
  theme_Publication() + theme(axis.title.y = element_blank())

ggsave("Review/3_plots/output/barchart_validation_count.png", width = 13, height = 6)

#Method and Validation

df_plot %>% 
  drop_na(v1) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ NA_character_)) %>% 
  count(method_short,v3) %>% 
  drop_na(method_short) %>% 
 ggplot(aes(method_short,v3, fill= n)) + 
  geom_tile() +  labs(title = "Heatmap of CTAM Method and Validation Type", x = "Method", y = "Validation Type", fill = "n") +
  theme_Publication() + scale_fill_gradient2() +
  theme(legend.direction = "vertical",legend.position = "right",legend.key.size= unit(0.5, "cm"))

ggsave("Review/3_plots/output/heatmap_methodxtype.png", width = 13, height = 6)

#

df_plot %>% 
  group_by(method_type) %>% 
  summarize(n = n(),
            amount_method )

