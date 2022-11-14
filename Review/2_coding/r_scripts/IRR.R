library(tidyverse)
library(irr)

# 1. Getting IRR for eligibility ----

df_temp <- readxl::read_xlsx("Review/2_coding/data/export_forms.xlsx") %>% 
  slice(-35) %>% 
  dplyr::rename(coder = `E-Mail-Adresse`,
                eligibil = names(.)[4]) %>% 
  mutate(id = paste(stringr::word(.$`This is the title of the Study`,1),
                    stringr::word(.$`These are the Authors`,1),sep = "_")) %>% 
drop_na(coder) 

df_temp %>% 
  select(coder, id, eligibil) %>% 
  pivot_wider(names_from = coder,
                             values_from = c(eligibil)) %>% 
  drop_na() -> irr_1 #Only keep rows where both coders coded something

#http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/

irr::kappa2(irr_1[,2:3])

# 2. Getting IRR for eligibility ----

df_temp %>% 
  filter(!is.na(v1a)) %>% 
  select(id, coder, v1a:v6d) %>% 
  pivot_longer(cols = v1a:v6d,
               names_to = "question",
               values_to = "values")->  t1

df_coder_1 <- t1 %>% filter(coder == "lukas.birkenmaier@outlook.de")
df_coder_2 <- t1 %>% filter(coder == "david.gruening@gesis.org")

full_join(df_coder_1, 
          df_coder_2, 
          by = c("id","question"),
          suffix = c("_c1", "_c2")) %>% 
  select(id, question, values_c1, values_c2) -> t2

xlsx::write.xlsx(t2, "Review/2_coding/data/IRR2_coding_sheet.xlsx")



  
  

