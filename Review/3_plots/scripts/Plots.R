df <- readxl::read_xlsx("Review/2_coding/data/export_forms.xlsx") %>% 
  filter(`E-Mail-Adresse` == "lukas.birkenmaier@outlook.de") %>% 
  mutate(id = paste(stringr::word(.$`This is the title of the Study`,1),
                    stringr::word(.$`These are the Authors`,1),sep = "_")) %>% 
  dplyr::rename(source = `What is the data source?`,
          language = `What is the data language?`,language_others = `If others: Name all languages in the data? If there is more than one language assessed, please separate languages by a comma (,)` ,
          method_type = `What Type(s) of method are initially applied?`,
          method_name = `What is the name of the method? If there is more than one method applied, please separate methods by a comma (,)`) %>% 
  drop_na(source)


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

