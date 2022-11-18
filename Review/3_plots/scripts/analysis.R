library(tidyverse)

df <- readxl::read_xlsx("Review/3_plots/data/export_forms.xlsx") %>% 
  filter(`E-Mail-Adresse` == "lukas.birkenmaier@outlook.de") %>% 
  mutate(id = paste(stringr::word(.$`This is the title of the Study`,1),
                    stringr::word(.$`These are the Authors`,1),sep = "_")) %>% 
  dplyr::rename(source = `What is the data source?`,
          language = `What is the data language?`,language_others = `If others: Name all languages in the data? If there is more than one language assessed, please separate languages by a comma (,)` ,
          method_type = `What Type(s) of method are initially applied?`,
          method_name = `What is the name of the method? If there is more than one method applied, please separate methods by a comma (,)`,
          target_construct = `Please name the target construct as described by the authors)\nNote: Please use the exact wording of the authors, such as \"affective polarization\", \"semantic complexity\".\nIf there is more than one construct studied, please separate constructs by a comma (,)`) %>% 
  drop_na(source)

#Creating variable for count of validation steps per publication
names(df)
df %>% select(id, ends_with("vtext")) %>%
  mutate(dont_drop = case_when(
    (`In what paragraphs is the step of validation / robustness checks / […] reported? 
      Please copy the paragraph/description into the open field!...20` != NA) ~ "valid"
  ))
  pivot_longer(cols = 2:9, names_to = "step", values_to = "content") %>% 
  drop_na(drop) %>% 
  group_by(id) %>% mutate(no = row_number(),
                          no_abs = max(no)) %>% 
  ungroup() %>% 
  distinct(id,no_abs) %>% 
  ggplot(aes(x = no_abs))+
    geom_bar()   
  
  
ll$`In what paragraphs is the step of validation / robustness checks / […] reported? 
Please copy the paragraph/description into the open field, so that the whole validation procedure is copied!`  
ll$`In what paragraphs is the step of validation / robustness checks / […] reported? 
Please copy the paragraph/description into the open field!...20`

df$`In what paragraphs are issues of measurement validity / robustness checks / […] reported? Please copy them into the open field!...40`

df %>% select(starts_with("In what"))

#Adding number of validation steps

df %>% 
  select(id, v_para)

#Plots

## Data Source
df %>% 
  separate_rows(source,sep = ",") %>% 
  ggplot(aes(x=reorder(source, table(source)[source]))) +
  geom_bar() +
  coord_flip() +
  xlab("") +
  theme_bw()

## Language

##Target Construct

df %>% 
  separate_rows(target_construct,sep = ",") %>% 
  ggplot(aes(x=reorder(target_construct, table(target_construct)[target_construct]))) +
  geom_bar() +
  coord_flip() +
  xlab("") +
  theme_bw()

## Method Type
df %>% 
  separate_rows(method_type,sep = ",") %>% 
  ggplot(aes(x=reorder(method_type, table(method_type)[method_type]))) +
  geom_bar() +
  coord_flip() +
  xlab("") +
  theme_bw()

## Method Name

df %>% 
  separate_rows(method_type,sep = ",") %>% 
  ggplot(aes(x = method_type, y = target_construct)) +
  geom_tile(color = "black") +
  coord_fixed() +
  theme(legend.position = "none")

ggsave("Tile_plot.png")

