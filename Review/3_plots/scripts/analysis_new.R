library(tidyverse)
library(gridExtra)

df <- readxl::read_xlsx("Review/3_plots/data/Results_final.xlsx") %>% 
  filter(`E-Mail-Adresse` == "lukas.birkenmaier@outlook.de") %>%

  mutate(id = paste(stringr::word(.$`This is the title of the Study`,1),
                    stringr::word(.$`These are the Authors`,1),sep = "_")) %>% 
  dplyr::rename(source = `What is the data source?`,
                title = `This is the title of the Study`,
          language = `What is the data language?`,language_others = `If others: Name all languages in the data? If there is more than one language assessed, please separate languages by a comma (,)` ,
          method_type = `What Type(s) of method are initially applied?`,
          method_name = `What is the name of the method? If there is more than one method applied, please separate methods by a comma (,)`) %>% 
  drop_na(source)

df_bib <- readr::read_csv("Review/1_literature_search/data/csv_files/complete_list_coded.csv")

left_join(df,df_bib %>% select(doi, Title, journal, year), by = c("title"="Title")) -> df

#No dublicates
length(unique(df$id)) == nrow(df)

#Creating variable for count of validation steps per publication

df_validation <- df %>% select(ends_with("_v1"))
df$n_validation <- rowSums(!is.na(df_validation))

#Changing into long format

df %>%  
  mutate(across(a_v1:f_v4, as.character)) %>% 
  pivot_longer(cols = a_v1:f_v4)  %>%  
  mutate(location = sub('.*(a|b|c|d|e|f).*', '\\1', name), 
         name  = sub('_?(a|b|c|d|e|f)_?', '', name)) %>% 
  pivot_wider() -> df_plot

#write_csv(df_plot %>% drop_na(v3), file = "Review/3_plots/data/qualitative_coding_categories_backup.csv")

#Paper Plots ----

## Publications over time----

df_time <- readxl::read_xlsx("Review/3_plots/data/search_results_CTAM_publications.xlsx")
df_categories <- readxl::read_xlsx("Review/3_plots/data/Web of Science Categories.xlsx")

df_time %>% 
  mutate(topic2 = gsub(";.*","",.$topic)) %>% 
  mutate(cat = case_when(
    (topic2 %in% df_categories$`Arts & Humanities`) ~ "Arts & Humanities",
    (topic2 %in% df_categories$`Life Sciences & Biomedicine`) ~ "Life Sciences & Biomedicine",
    (topic2 %in% df_categories$`Physical Sciences`) ~ "Physical Sciences",
    (topic2 %in% df_categories$`Social Sciences`) ~ "Social Sciences",
    (topic2 %in% df_categories$Technology) ~ "Technology",
    T ~ "No")) %>% 
  filter(`Publication Year`<= 2021) %>% 
  count(cat,`Publication Year`) %>% 
  ggplot() +
  geom_area(aes(x = `Publication Year`, y = n, fill = cat), alpha = 0.8)+
  xlim(2005,2021)+
  labs(y = "Count") +
  theme_Publication() +
  scale_fill_Publication() -> plot_time

ggsave(plot =plot_time ,filename = "Review/3_plots/output/overview_time.png",width = 12, height = 9,dpi = 300)


## Overview ----

### Source ----

df %>% 
  separate_rows(source,sep = ",") %>% mutate(source = trimws(source)) %>% 
  mutate(group = case_when(
    startsWith(as.character(source),"Party") ~ "Political System",
    startsWith(as.character(source),"Social") ~ "Social Media",
    startsWith(as.character(source),"News") ~ "Newspaper",
    T ~ "Others")) %>% 
  mutate(source = str_remove_all(source, paste(c("Party Politics: ","Social Media: "), collapse = "|"))) %>%
  count(group, source, sort = T) %>% 
  mutate(source = case_when(
    (n == 1) ~ "Others", T ~ source),
    group = case_when((source == "Others") ~ "Others",T~group))  -> df1

df1 %>% filter(n == 1) %>% count(group, source) -> df2

rbind(df1 %>% filter(n > 1),df2) %>% 
  arrange(group,-n) %>% 
  bind_rows(slice(., 2)) %>% slice(-2) %>% 
  rowid_to_column() %>%
  mutate(source = case_when(
    (group == "Others" & source == "Others") ~ "Various (Others)",
    (group == "Party Politics" & source == "Others") ~ "Various (Party Politics)",
    (group == "Social Media" & source == "Others")  ~ "Various (Social Media)",
    T ~ source)) %>% 
  ggplot(aes(x=reorder(source,-rowid), y = n, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_discrete(labels = label_wrap(15))+
  labs(title = "a) Text Source", y = "n") +
  scale_fill_manual(name = "Type",values = c("#ef3b2c","#662506","#a6cee3","#fb9a99"))+
  theme_Publication() +
  theme(legend.position = "top",legend.direction="horizontal", legend.title = element_text(size = 13,face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) -> barchart_source

### Language ----

df %>% 
  mutate(language_combined = case_when(
  language == "English"~ language,
  T ~ language_others)) %>% 
  separate_rows(language_combined,sep = ",") %>% mutate(language_combined = trimws(language_combined)) -> df_temp
df_temp%>% 
  count(language_combined) %>% left_join(df_temp, ., by = "language_combined") %>% 
  mutate(language_combined = case_when(
    (n > 2) ~ language_combined,
    (n <=2) ~ "Others")) %>% 
  ggplot(aes(x=reorder(language_combined, table(language_combined)[language_combined]),fill = factor(language_combined))) +
  geom_bar() +
  coord_flip() +
  labs(title = "b) Data Languages",x = "Language", y = "n", fill = "Language") +
  theme_Publication()+
  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none") -> barchart_language

### Method Type and Topic ----

df %>% 
  separate_rows(method_type,sep = ",") %>% mutate(method_type = trimws(method_type)) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ NA_character_)) %>% 
  count(method_short,construct_new) %>% 
  drop_na(method_short) %>% 
  ggplot(aes(method_short,construct_new, fill= n)) + 
  geom_tile() +  
  geom_text(aes(label = n),size = 4,fontface  = "bold",color = "white")+
  labs(title = "c) Construct and Method Type", fill = "n") +
  theme_Publication() + scale_fill_gradient2()+
  scale_x_discrete(labels = label_wrap(15))+
  scale_y_discrete(labels = label_wrap(20))+
  theme(legend.direction = "vertical",legend.position = "right",
        legend.key.size= unit(0.5, "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())  -> tile_type_topic


ggpubr::ggarrange(ggpubr::ggarrange(barchart_source,barchart_language,nrow=2,align = "v",heights = c(1.2,1)),tile_type_topic,nrow = 1) -> souce_overview


ggsave(plot =souce_overview ,filename = "Review/3_plots/output/overview_descriptive.png",width = 12, height = 9,dpi = 300)


#Calculating Percetages of Method Type
##Note: There might be more than one 
df %>% 
  separate_rows(method_type,sep = ",") %>% 
  mutate(method_type = trimws(method_type)) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ NA_character_)) %>% 
  group_by(method_short) %>% 
  summarise(share = n()/nrow(df),
            n = n())

#Inspect which methods were not matched

df %>% 
  separate_rows(method_type,sep = ",") %>% 
  mutate(method_type = trimws(method_type)) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ NA_character_)) %>% 
  select(method_type, method_short) %>% 
  filter(is.na(method_short))

## Methods over time

#tba

## Replication ----

df %>% 
  rename(link = `Is there a link to an Appendix/additional Materials/GitHub repository?`) %>%
  count(link)%>%
  ggplot(aes(x=link,y = n, fill = link)) +
  geom_bar(stat = "identity")+
  labs(title = "Replication Materials available?",y = "Count of Publications") +
  theme_Publication() + scale_fill_Publication() + 
  theme(legend.position = "none",axis.title.x = element_blank())

ggsave("Review/3_plots/output/replication.png", width = 8, height = 8)


### Replication over Time ----

df %>% 
  rename(link = `Is there a link to an Appendix/additional Materials/GitHub repository?`) %>%
  mutate(link = case_when((link == "Yes")~1,T ~0)) %>% 
  group_by(year) %>% 
  summarise(n = n(),percent = sum(link)/n(),sd = sd(link)) %>% 
  filter(year >2017) %>% 
  ggplot(aes(x=year,y = percent,label = n)) +
  geom_line() +
  geom_text(vjust=-2)+
  geom_point() +
  theme_Publication()


## Number of Validation Steps ----

df_plot %>% 
  separate_rows(method_type,sep = ",") %>% 
  mutate(method_type = trimws(method_type)) %>% 
  distinct(id, n_validation, method_type) %>% 
  group_by(id) %>% mutate(count_methods = n()) %>% ungroup() %>% 
  mutate(n_validation_adjusted = n_validation/count_methods) %>% #Calculating the mean number of validation steps for each paper adjusted for the amount of methods applied
  distinct(id, n_validation,n_validation_adjusted,count_methods,method_type) -> df_n_validation

#Amount of studies without any validation

df_n_validation %>% 
  distinct(id, n_validation) %>% 
  count(n_validation) #14 studies without validation -> 14/96 = 14,6%

### Number of Validation (simple)----

df_n_validation %>% 
  distinct(id,n_validation_adjusted) %>% 
  ggplot(aes(x=as.character(round(n_validation_adjusted)),fill = factor(round(n_validation_adjusted)))) +
  geom_bar()+
  labs(title = "a) Distribution of Validation Steps per Publication", y = "Count of Publications\n", x = "\nNumber of Validation Steps") +
  theme_Publication() + scale_fill_Publication() + 
  theme(legend.position = "none") -> n_validation_bar

### Number of Validation (method )----

df_n_validation %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ NA_character_)) %>% 
  filter(count_methods == 1) %>% 
  count(method_short,n_validation) %>% 
  drop_na(method_short) %>% 
  ggplot(aes(method_short,as.character(n_validation), fill= n)) + 
  geom_tile() +  
  geom_text(aes(label = n),size = 4,fontface  = "bold",color = "white")+
  labs(title = "b) Distribution of Validation Steps per Method Type", fill = "Count", y = "Number of Validation Steps\n", x = "\nMethod Type") +
  theme_Publication() + scale_fill_gradient2(breaks = c(seq(1,9,1))) +
  scale_x_discrete(labels = label_wrap(15))+
  scale_y_discrete(labels = label_wrap(20))+
  theme(legend.direction = "vertical",legend.position = "right",
        legend.key.size= unit(0.8, "cm")) -> n_validation_method_heatmap


ggpubr::ggarrange(n_validation_bar,
                  n_validation_method_heatmap,
                  heights = c(6, 10),
                  nrow=2) -> n_validation_overview


ggsave(plot = n_validation_overview ,filename = "Review/3_plots/output/n_validation_overview.png",width = 10, height = 8,dpi = 300)

## Validation Type (general) ----

df_qual <- readxl::read_xlsx("Review/3_plots/data/Qualitative_Evaluation_Validation_Steps.xlsx") %>% 
  select(method_short	, Phase,category_evidence,category_specific )

df_qual %>% 
  count(Phase, method_short) %>% 
  drop_na(Phase) |> group_by(Phase) |> mutate(count_total = sum(n)) |> 
  ggplot(aes(x=factor(Phase, levels = c("Robustness Checks",
                                        "External",
                                        "Structural")), y = n, fill = method_short)) +
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  labs(y = "\nNumber of Validation Steps")+
  theme_Publication() + scale_fill_Publication(name = "Method Type") + 
  theme(legend.direction = "vertical",legend.position = "right",
        legend.key.size= unit(0.8, "cm"),
        axis.title.y = element_blank()) -> plot_validation_general

ggsave(plot = plot_validation_general ,filename = "Review/3_plots/output/plot_validation_general.png",width = 10, height = 4,dpi = 300)


## Validation Type (specific) ----

df_qual |> 
  filter(Phase == "Structural") |> 
  count(method_short,category_evidence,category_specific) |> 
  drop_na() |> 
  ggplot(df_qual, mapping = aes(x = category_specific, y = n, fill = method_short)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  facet_grid(rows = vars(category_evidence), scales = "free_y",  space = "free_y") +
  coord_flip() +
  scale_fill_Publication(name = "Method Type")+
  theme_Publication()+
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(size = 15, face = "bold"),
    strip.text.x = element_blank(),
    strip.text.y = element_text(angle = 270, face = "bold"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_blank())
 # theme(    strip.placement = "outside",
  #          strip.text.y = element_text(angle = 270, face = "bold"),
   #         panel.grid.major.y = element_blank())+
  

#Old Plot

df_qual |> 
  drop_na() |> 
  count(method_short,Phase,category_specific) |> 
  mutate(Phase = fct_relevel(Phase, "Structural"),
         category_specific = fct_reorder(category_specific, n)) %>% 
  ggplot(df_qual, mapping = aes(x = category_specific, y = n, fill = method_short)) + 
  geom_col(position = 'stack') + 
  facet_grid(rows = vars(factor(Phase,levels = c("Structural",
                                                 "External","Robustness Checks"))), scales = "free_y",  space = "free_y") +
  coord_flip() +
  scale_fill_Publication(name = "Method Type")+
  labs(y = "Count of Validation Steps")+
  theme_Publication()+
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(size = 15, face = "bold"),
    strip.text.x = element_blank(),
    strip.text.y = element_text(angle = 270, face = "bold"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()) -> plot_detailed

ggsave(plot = plot_detailed ,filename = "Review/3_plots/output/plot_validation_update.png",width = 10, height = 8,dpi = 500)




df_qual |> 
  group_by(category_specific,method_short) |> 
  mutate(n_specific = n()) |> 
  ungroup() |> 
  distinct(method_short, category_general, category_specific, n_specific) -> df_validation_specific 
  
df_validation_specific |> 
  filter(category_general == types[1]) |> 
ggplot(aes(x=reorder(category_specific,n_specific), y = n_specific, fill = method_short)) +
    geom_bar(position="stack", stat="identity")+
    coord_flip()+
  ylim(0,15)+
  labs(title = "Content Validation")+
    theme_Publication() + scale_fill_Publication(name = "Method Type") + 
    theme(legend.direction = "horizontal",legend.position = "right",
          legend.key.size= unit(0.8, "cm"),
          axis.title.y = element_blank(), 
          axis.title.x = element_blank())  -> p_content

df_validation_specific |> 
  filter(category_general == types[2]) |> 
  ggplot(aes(x=reorder(category_specific,n_specific), y = n_specific, fill = method_short)) +
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  ylim(0,15)+
  labs(title = "Output Validation: Criterion Data")+
  theme_Publication() + scale_fill_Publication(name = "Method Type") + 
  theme(legend.direction = "horizontal",legend.position = "right",
        legend.key.size= unit(0.8, "cm"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank()) -> p_criterion

df_validation_specific |> 
  filter(category_general == types[4]) |> 
  ggplot(aes(x=reorder(category_specific,n_specific), y = n_specific, fill = method_short)) +
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  ylim(0,15)+
  labs(title = "Robustness Analysis")+
  theme_Publication() + scale_fill_Publication(name = "Method Type") + 
  theme(legend.direction = "horizontal",legend.position = "right",
        legend.key.size= unit(0.8, "cm"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank()) -> p_Robust

df_validation_specific |> 
  filter(category_general == types[5]) |> 
  ggplot(aes(x=reorder(category_specific,n_specific), y = n_specific, fill = method_short)) +
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  ylim(0,60)+
  labs(title = "Output Validation: Human Labels")+
  theme_Publication() + scale_fill_Publication(name = "Method Type") + 
  theme(legend.direction = "horizontal",legend.position = "right",
        legend.key.size= unit(0.8, "cm"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank()) -> p_human

df_validation_specific |> 
  filter(category_general == types[6]) |> 
  ggplot(aes(x=reorder(category_specific,n_specific), y = n_specific, fill = method_short)) +
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  ylim(0,15)+
  labs(title = "Output Validation: CTAM Scores")+
  theme_Publication() + scale_fill_Publication(name = "Method Type") + 
  theme(legend.direction = "horizontal",legend.position = "right",
        legend.key.size= unit(0.8, "cm"),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank()) -> p_CTAM


ggpubr::ggarrange(p_human,p_content, p_criterion, p_CTAM, p_Robust,nrow=5,
                  common.legend = TRUE, legend="bottom",
                  heights = c(1.8,
                              4,
                              2,
                              2,
                              3),align = "v") -> plot_validation

ggsave(plot = plot_validation ,filename = "Review/3_plots/output/plot_validation.png",width = 12, height = 13,dpi = 500)


### Type of Validation Evidence and Method Type ----

### Type of Validation (simple)----



df_plot %>% 
  drop_na(v3) %>% 
  count(v3) %>% 
  ggplot(aes(x=factor(v3, 
                      levels = c("Unsure", 
                                 "External: Criterion data / Predictive validation",
                                 "External: Human Annotated Scores",
                                 "External: Scores from other CATM",
                                 "Content Validation")), y = n, fill = v3)) +
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "a) Distribution of Validation Type", y = "n", fill = "Number of Validation Steps") +
  theme_Publication() + scale_fill_Publication() + 
  scale_x_discrete(labels = label_wrap(20))+
  theme(axis.title.y = element_blank(), 
        legend.position = "none",
        axis.title.x = element_blank()) -> type_validation_bar

### Type of Validation (for Method type)----

df_plot %>% 
  drop_na(v1) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ NA_character_)) %>% 
  count(method_short,v3) %>% 
  drop_na(method_short) %>% 
  ggplot(aes(method_short,factor(v3, 
                                 levels = c("Unsure", 
                                            "External: Criterion data / Predictive validation",
                                            "External: Human Annotated Scores",
                                            "External: Scores from other CATM",
                                            "Content Validation")), fill= n)) + 
  geom_tile() +  
  geom_text(aes(label = n),size = 4,fontface  = "bold",color = "white")+
  labs(title = "Distribution of Validation Type per Method Type",fill = "Count") +
  theme_Publication() + scale_fill_gradient2(breaks = c(seq(1,40,5))) +
  scale_x_discrete(labels = label_wrap(18))+
  scale_y_discrete(labels = label_wrap(20))+
  theme(legend.direction = "vertical",legend.position = "right",
        legend.key.size= unit(0.8, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) -> type_validation_method_heatmap



ggpubr::ggarrange(type_validation_bar,
                  type_validation_method_heatmap,
                    heights = c(6, 10),
                    nrow=2) -> type_validation_overview


ggsave(plot = type_validation_overview ,filename = "Review/3_plots/output/type_validation_overvietw.png",width = 9, height = 10,dpi = 300)


# Backup ----

## Type ----
df_plot %>% 
  distinct(id, method_type) %>% 
  mutate(method_short = case_when(
    startsWith(as.character(method_type),"Super") ~ "Supervised",
    startsWith(as.character(method_type),"Unsuper") ~ "Unsupervised",
    startsWith(as.character(method_type),"Rule") ~ "Dictionary",
    T ~ "Others")) %>% 
  count(method_short) %>% 
  ggplot(aes(x=reorder(method_short,-n), y = n, fill = method_short)) +
  geom_bar(stat = "identity") +
  labs(x = "Type", y = "n", fill = "Method") +
  theme_Publication()+  scale_fill_Publication()+
  theme(axis.title.y = element_blank(),legend.position = "none",axis.title.x = element_blank()) -> barchart_method


### Construct ----

df %>% 
  separate_rows(construct_new,sep = ",") %>% 
  mutate(construct_new = trimws(construct_new)) %>% 
  count(construct_new) %>% 
  arrange(-n)


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



