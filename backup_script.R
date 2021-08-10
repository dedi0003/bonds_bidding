own_2015_jan <- read_xlsx("data/ownership_2015.xlsx", col_names = T, sheet = 1) %>% 
  setNames(., c('id', format(as.Date(as.numeric(names(.)[-1]), 
                                     origin = '1899-12-30'), '%m/%d/%Y'))) %>%
  as_tibble() %>% 
  pivot_longer(names_to = "date", cols = -1) %>% 
  mutate(id = tolower(sub("[*]", "", id))) %>% 
  mutate(id = gsub(".*/ ","",id)) %>% 
  filter(!(id %in% c("government institution", "non-banks", "non-bank/non-banks"))) %>% 
  mutate(id = gsub(".*/","",id), date = as.Date(gsub("[/]", "", date), format = "%m%d%Y")) %>% 
  mutate(group = factor(case_when(id == "bank indonesia" ~ "government",
                                  id == "banks" ~ "bank",
                                  id == "incl. foreign government(s) & central bank(s) *" ~ "foreign gov (part of foreign holders)",
                                  id %in% c("mutual funds", "insurance company", "foreign holders", "pension funds", "securities company", "individual", "others")~"non-bank", TRUE ~ "government"))
  )


path = "data/ownership_2016.xlsx"
library(readxl)
own <- read_xlsx(path)
own_names <- read_xlsx(path, col_names = T) %>% 
  setNames(., c('id', format(as.Date(as.numeric(names(.)[-1]), 
                                      origin = '30-12-1899'), '%m/%d/%Y'))) %>%
  as_tibble()

glimpse(own)
head(own)
names(own)
