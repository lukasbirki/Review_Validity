library(tidyverse)
library(gridExtra)

df <- readxl::read_xlsx("Review/3_plots/data/Results_final.xlsx") %>% 
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

#Paper Plots ----
#Plots
df_plot %>% 
  distinct(id, source) %>% 
  separate_rows(source,sep = ",") %>% mutate(source = trimws(source)) %>% 
  mutate(group = case_when(
    startsWith(as.character(source),"Party") ~ "Party Politics",
    startsWith(as.character(source),"Social") ~ "Social Media",
    T ~ "Others")) %>% 
  count(group, source, sort = T) %>% arrange(group,-n) %>% rowid_to_column() %>%
  mutate(source = str_remove_all(source, c("Party Politics: ","Social Media: ")))  %>% 
  ggplot(aes(x=reorder(source,-rowid), y = n, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Data Sources", x = "Data", y = "n") +
  scale_fill_Publication(name = "Source",labels = c("Social Media", "Party Politics","Others"))+
  theme_Publication() -> barchart_source

ggsave(plot =barchart_source ,filename = "Review/3_plots/output/barchart_source.png",width = 13, height = 6)

## Data Source
df_plot %>% 
  distinct(id, source) %>% 
  separate_rows(source,sep = ",") %>% mutate(source = trimws(source)) %>% 
  mutate(group = case_when(
    startsWith(as.character(source),"Party") ~ "Party Politics",
    startsWith(as.character(source),"Social") ~ "Social Media",
    T ~ "Others")) -> df_source_multiple 

## 1: Social Media
df_source_multiple %>% 
  filter(group == "Social Media") %>% 
  mutate(source = str_remove(source, pattern = "Social Media: ")) %>% 
  ggplot(aes(x=reorder(source, table(source)[source]), fill = source)) +
  geom_bar() +
  labs(y = "n") +
  ylim(0,20)+
  coord_flip() +
  scale_fill_Publication()+
  theme_Publication() +
  theme(axis.title.y = element_blank(),legend.position = "none")-> p1

df_source_multiple %>% 
  filter(group == "Party Politics") %>% 
  mutate(source = str_remove(source, pattern = "Party Politics: ")) %>% 
  ggplot(aes(x=reorder(source, table(source)[source]),fill = source)) +
  geom_bar() +
  labs(y = "n") +
  ylim(0,20)+
  scale_x_discrete(labels = label_wrap(15))+
  coord_flip() +
  scale_fill_Publication()+
  theme_Publication() +
  theme(axis.title.y = element_blank(),legend.position = "none")-> p2

df_source_multiple %>% 
  filter(group == "Others") %>% 
  ggplot(aes(x=reorder(source, table(source)[source]),fill = source)) +
  geom_bar(width = 0.5) +
  labs(y = "n") +
  ylim(0,20)+
  scale_x_discrete(labels = label_wrap(15))+
  coord_flip() +
  scale_fill_Publication()+
  theme_Publication() +
  theme(axis.title.y = element_blank(),legend.position = "none")-> p3

ggpubr::ggarrange(p1,p2,p3,labels = c("Social Media","Party Politics","Others"), nrow = 1) -> souce_overview

ggsave(plot =souce_overview ,filename = "Review/3_plots/output/barchart_source_overview.png",width = 12, height = 8)

## Language

df_plot %>% 
  mutate(language_combined = case_when(
    language == "English"~ language,
    T ~ language_others)) %>% 
  distinct(id, language_combined) %>% 
  separate_rows(language_combined,sep = ",") %>% mutate(language_combined = trimws(language_combined)) -> df_temp

df_temp%>% 
  count(language_combined) %>% left_join(df_temp, ., by = "language_combined") %>% 
  mutate(language_combined = case_when(
    (n > 1) ~ language_combined,
    (n == 1) ~ "Others")) %>% 
  ggplot(aes(x=reorder(language_combined, table(language_combined)[language_combined]),fill = factor(language_combined))) +
  geom_bar() +
  coord_flip() +
  labs(x = "Language", y = "n", fill = "Language") +
  theme_Publication()+
  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none") -> barchart_language

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
  labs(x = "Type", y = "n", fill = "Method") +
  theme_Publication()+  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none") -> barchart_method

ggsave(plot = barchart_method,"Review/3_plots/output/barchart_method.png", width = 13, height = 6)

ggpubr::ggarrange(p1,barchart_method,labels = c("Language","Method Type"),nrow = 1) -> souce_overview
ggsave(plot =souce_overview ,filename = "Review/3_plots/output/barchart_language_method.png",width = 17, height = 6)

#Count of validation steps

df_plot %>% 
  distinct(id, n_validation_steps) %>% 
  ggplot(aes(x=n_validation_steps,fill = factor(n_validation_steps))) +
  geom_bar()+
  labs(title = "Number of Validation Steps", x = "Validation Steps", y = "n", fill = "Number of Validation Steps") +
  theme_Publication() + scale_fill_Publication() + 
  theme(axis.title.y = element_blank(), legend.position = "none")

ggsave("Review/3_plots/output/barchart_validation_count.png", width = 13, height = 6)



#Data and Code
df_plot$`Is there a link to an Appendix/additional Materials/GitHub repository?`

df_plot %>% 
  rename(link = `Is there a link to an Appendix/additional Materials/GitHub repository?`) %>% 
  select(id, source, link) %>% 
  distinct(id,source,link) %>% 
  ggplot(aes(x=link,fill = link)) +
  geom_bar()+
  labs(title = "Replication Materials available?",) +
  theme_Publication() + scale_fill_Publication() + 
  theme(axis.title.y = element_blank(), legend.position = "none",axis.title.x = element_blank())

ggsave("Review/3_plots/output/replication.png", width = 8, height = 8)
## Decriptive ----

### Source ----
df_plot %>% 
  distinct(id,source) %>% 
  separate_rows(source,sep = ",") %>% mutate(source = trimws(source)) %>% 
  mutate(group = case_when(
    startsWith(as.character(source),"Party") ~ "Party Politics",
    startsWith(as.character(source),"Social") ~ "Social Media",
    T ~ "Media/Others")) %>% 
  mutate(source = str_remove_all(source, c("Party Politics: ","Social Media: ")))  %>% distinct(source) 
  count(source, source, sort = T) %>%
  mutate(category = case_when(
    (n >=2) ~ source,
    (n<2) ~ paste(group,": Other"))) %>% 
  arrange(category,n) %>% rowid_to_column() %>%
  distinct(category, n)
  mutate(category_new = str_remove_all(category, c("Party Politics : "))) %>% 
  ggplot(aes(x=reorder(category,-rowid), y = n, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "n") +
  scale_fill_Publication(name = "Source",labels = c("Social Media", "Party Politics","Others"))+
  theme_Publication() +theme(axis.title.y = element_blank())-> barchart_source

### Type ----
df_plot %>% 
  distinct(id, method_type) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ "Others")) %>% 
  ggplot(aes(x=reorder(method_short, table(method_short)[method_short]),fill = factor(method_short))) +
  geom_bar() +
  labs(x = "Type", y = "n", fill = "Method") +
  theme_Publication()+  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none",axis.title.x = element_blank()) -> barchart_method

### language ----

df_plot %>% mutate(language_combined = case_when(
  language == "English"~ language,
  T ~ language_others)) %>% 
  distinct(id, language_combined) %>% 
  separate_rows(language_combined,sep = ",") %>% mutate(language_combined = trimws(language_combined)) -> df_temp
df_temp%>% 
  count(language_combined) %>% left_join(df_temp, ., by = "language_combined") %>% 
  mutate(language_combined = case_when(
    (n > 2) ~ language_combined,
    (n <=2) ~ "Others")) %>% 
  ggplot(aes(x=reorder(language_combined, table(language_combined)[language_combined]),fill = factor(language_combined))) +
  geom_bar() +
  coord_flip() +
  labs(x = "Language", y = "n", fill = "Language") +
  theme_Publication()+
  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none") -> barchart_language

ggpubr::ggarrange(barchart_source,
                  ggpubr::ggarrange(barchart_language,barchart_method,labels = c("Data Language","Method Type")),labels = c("Data Source",""),nrow = 2) -> souce_overview


ggsave(plot =souce_overview ,filename = "Review/3_plots/output/barchart_source.png",width = 10, height = 10)

## Method and Validation ----

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
  geom_tile() +  labs(title = "Heatmap of CTAM Method and Validation Type", fill = "n") +
  theme_Publication() + scale_fill_gradient2() +
  scale_x_discrete(labels = label_wrap(15))+
  scale_y_discrete(labels = label_wrap(20))+
  theme(legend.direction = "vertical",legend.position = "right",
        legend.key.size= unit(0.5, "cm"),
        axis.title.y = element_blank())

ggsave("Review/3_plots/output/heatmap_methodxtype.png", width = 10, height = 8)

## Method and Count Validation Steps ----

df_plot %>% 
  distinct(id, n_validation_steps,method_type) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ NA_character_)) %>% 
  count(method_short,n_validation_steps) %>% 
  drop_na(method_short) %>% 
  ggplot(aes(method_short,as.character(n_validation_steps), fill= n)) + 
  geom_tile() +  labs(title = "Heatmap of CTAM Method and Validation Type", fill = "n") +
  theme_Publication() + scale_fill_gradient2() +
  scale_x_discrete(labels = label_wrap(15))+
  scale_y_discrete(labels = label_wrap(20))+
  theme(legend.direction = "vertical",legend.position = "right",
        legend.key.size= unit(0.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),)

ggsave("Review/3_plots/output/heatmap_methodxn_validation.png", width = 10, height = 8)

# Backup ----

#Plots
df_plot %>% 
  distinct(id, source) %>% 
  separate_rows(source,sep = ",") %>% mutate(source = trimws(source)) %>% 
  mutate(group = case_when(
    startsWith(as.character(source),"Party") ~ "Party Politics",
    startsWith(as.character(source),"Social") ~ "Social Media",
    T ~ "Others")) %>% 
  count(group, source, sort = T) %>% arrange(group,-n) %>% rowid_to_column() %>%
  mutate(source = str_remove_all(source, c("Party Politics: ","Social Media: ")))  %>% 
  ggplot(aes(x=reorder(source,-rowid), y = n, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Data Sources", x = "Data", y = "n") +
  scale_fill_Publication(name = "Source",labels = c("Social Media", "Party Politics","Others"))+
  theme_Publication() -> barchart_source

ggsave(plot =barchart_source ,filename = "Review/3_plots/output/barchart_source.png",width = 13, height = 6)


## Data Source
df_plot %>% 
  distinct(id, source) %>% 
  separate_rows(source,sep = ",") %>% mutate(source = trimws(source)) %>% 
  mutate(group = case_when(
    startsWith(as.character(source),"Party") ~ "Party Politics",
    startsWith(as.character(source),"Social") ~ "Social Media",
    T ~ "Others")) -> df_source_multiple 

## 1: Social Media
df_source_multiple %>% 
  filter(group == "Social Media") %>% 
  mutate(source = str_remove(source, pattern = "Social Media: ")) %>% 
  ggplot(aes(x=reorder(source, table(source)[source]), fill = source)) +
  geom_bar() +
  labs(y = "n") +
  ylim(0,20)+
  coord_flip() +
  scale_fill_Publication()+
  theme_Publication() +
  theme(axis.title.y = element_blank(),legend.position = "none")-> p1

df_source_multiple %>% 
  filter(group == "Party Politics") %>% 
  mutate(source = str_remove(source, pattern = "Party Politics: ")) %>% 
  ggplot(aes(x=reorder(source, table(source)[source]),fill = source)) +
  geom_bar() +
  labs(y = "n") +
  ylim(0,20)+
  scale_x_discrete(labels = label_wrap(15))+
  coord_flip() +
  scale_fill_Publication()+
  theme_Publication() +
  theme(axis.title.y = element_blank(),legend.position = "none")-> p2

df_source_multiple %>% 
  filter(group == "Others") %>% 
  ggplot(aes(x=reorder(source, table(source)[source]),fill = source)) +
  geom_bar(width = 0.5) +
  labs(y = "n") +
  ylim(0,20)+
  scale_x_discrete(labels = label_wrap(15))+
  coord_flip() +
  scale_fill_Publication()+
  theme_Publication() +
  theme(axis.title.y = element_blank(),legend.position = "none")-> p3

ggpubr::ggarrange(p1,p2,p3,labels = c("Social Media","Party Politics","Others"), nrow = 1) -> souce_overview

ggsave(plot =souce_overview ,filename = "Review/3_plots/output/barchart_source_overview.png",width = 12, height = 8)

## Language

df_plot %>% 
  mutate(language_combined = case_when(
    language == "English"~ language,
    T ~ language_others)) %>% 
  distinct(id, language_combined) %>% 
  separate_rows(language_combined,sep = ",") %>% mutate(language_combined = trimws(language_combined)) -> df_temp

df_temp%>% 
  count(language_combined) %>% left_join(df_temp, ., by = "language_combined") %>% 
  mutate(language_combined = case_when(
    (n > 1) ~ language_combined,
    (n == 1) ~ "Others")) %>% 
  ggplot(aes(x=reorder(language_combined, table(language_combined)[language_combined]),fill = factor(language_combined))) +
  geom_bar() +
  coord_flip() +
  labs(x = "Language", y = "n", fill = "Language") +
  theme_Publication()+
  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none") -> barchart_language

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
  labs(x = "Type", y = "n", fill = "Method") +
  theme_Publication()+  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none") -> barchart_method

ggsave(plot = barchart_method,"Review/3_plots/output/barchart_method.png", width = 13, height = 6)

ggpubr::ggarrange(p1,barchart_method,labels = c("Language","Method Type"),nrow = 1) -> souce_overview
ggsave(plot =souce_overview ,filename = "Review/3_plots/output/barchart_language_method.png",width = 17, height = 6)

#Count of validation steps

df_plot %>% 
  distinct(id, n_validation_steps) %>% 
  ggplot(aes(x=n_validation_steps,fill = factor(n_validation_steps))) +
  geom_bar()+
  labs(title = "Number of Validation Steps", x = "Validation Steps", y = "n", fill = "Number of Validation Steps") +
  theme_Publication() + scale_fill_Publication() + 
  theme(axis.title.y = element_blank(), legend.position = "none")

ggsave("Review/3_plots/output/barchart_validation_count.png", width = 13, height = 6)



#Data and Code
df_plot$`Is there a link to an Appendix/additional Materials/GitHub repository?`

df_plot %>% 
  rename(link = `Is there a link to an Appendix/additional Materials/GitHub repository?`) %>% 
  select(id, source, link) %>% 
  distinct(id,source,link) %>% 
  ggplot(aes(x=link,fill = link)) +
  geom_bar()+
  labs(title = "Replication Materials available?",) +
  theme_Publication() + scale_fill_Publication() + 
  theme(axis.title.y = element_blank(), legend.position = "none",axis.title.x = element_blank())

ggsave("Review/3_plots/output/replication.png", width = 8, height = 8)
