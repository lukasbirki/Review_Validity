df <- readxl::read_xls("C:/Users/birkenls/OneDrive - GESIS – Leibniz-Institut für Sozialwissenschaften e.V/Desktop/savedrecs.xls")
df_categories <- readxl::read_xlsx("C:/Users/birkenls/OneDrive - GESIS – Leibniz-Institut für Sozialwissenschaften e.V/Desktop/Book1.xlsx")

df %>% 
  count(`Publication Year`, topic) %>% 
  mutate(category = case_when(
    (`WoS Categories` == "Political Science") ~ `WoS Categories`,
    (`WoS Categories` == "Communication") ~ `WoS Categories`,
    (`WoS Categories` == "Management") ~ `WoS Categories`,
    (`WoS Categories` == "Knowledge Engineering") ~ `WoS Categories`,
    T ~ "Others")) %>% 
ggplot() +
  geom_area(aes(x = `Publication Year`, y = n, fill = category),alpha=0.8)+
  geom_line(data = df1, aes(x = `Publication Year`, y = n))+
  xlim(2005,2022)+
  labs(y = "Count") +
  theme_Publication() +
  scale_fill_Publication()

df$`WoS Categories`

df %>% 
  mutate(topic2 = gsub(";.*","",.$topic)) %>% 
  mutate(cat = case_when(
    (topic2 %in% df_categories$`Arts & Humanities`) ~ "Arts & Humanities",
    (topic2 %in% df_categories$`Life Sciences & Biomedicine`) ~ "Life Sciences & Biomedicine",
    (topic2 %in% df_categories$`Physical Sciences`) ~ "Physical Sciences",
    (topic2 %in% df_categories$`Social Sciences`) ~ "Social Sciences",
    (topic2 %in% df_categories$Technology) ~ "Technology",
    T ~ "No")) %>% 
  #select(cat, topic2) -> tt
  count(cat) %>% 
  count(cat,`Publication Year`) %>% 
  ggplot() +
  geom_area(aes(x = `Publication Year`, y = n, fill = cat), alpha = 0.8)+
  xlim(2005,2022)+
  labs(y = "Count") +
  theme_Publication() +
  scale_fill_Publication()


r[6,1] %in% df_categories$`Social Sciences`
  