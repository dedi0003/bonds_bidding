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


###Script for converting ownership 2016 from excel files
own_2016 <- read.xlsx(xlsxFile = "data/ownership_2016.xlsx", fillMergedCells = TRUE, colNames = F, startRow = 2) %>% 
  # pivot_longer(names_to = "date", cols = -1) %>%
  as.data.frame()

date_header <- read_xlsx("data/ownership_2016.xlsx", n_max = 1, col_names = F) %>% t() %>% as.data.frame() %>% 
  do(na.locf(.)) %>% t()

date_header[setdiff(names(own_2016), names(date_header))] <- NA
# own_2016[setdiff(names(date_header), names(own_2016))] <- NA

merge <- rbind(date_header, own_2016) %>% as_tibble() %>% 
  mutate(X63 = coalesce(X62,X63), X64 = coalesce(X63, X64))

names(merge) <- merge[1, ]

merge_test <- merge %>% 
  pivot_longer(names_to = "date", cols = c(-1)) %>% 
  rename(id = INSTITUTION) %>% 
  filter(!id %in% c("INSTITUTION", "BANK*", "Government Institution", "NON-BANK", NA))

ownership_clean <- merge_test %>%  
  mutate(type = rep(c("sun", "sbsn", "total"), nrow(merge_test)/3)) %>% 
  mutate(id = tolower(trimws(id))) %>%
  mutate(id = case_when(id == "non resident" ~ "foreign holders", 
                        str_extract(id, "net") == "net" ~ "bank indonesia",
                        str_extract(id, "net") == "net" ~ "bank indonesia",
                        
                        TRUE ~ id))%>% 
  mutate(date = ymd(date), 
         group = factor(case_when(
           str_extract(id,"net") == "net" ~ "government",
           str_extract(id, "monetary operation") == "monetary operation" ~ "government (part of BI)",
           str_extract(id, "gross") == "gross" ~ "government (part of BI)",
           id %in% c("conventional bank", "islamic bank") ~ "bank",
           
           str_extract(id, "foreign government") == "foreign government" ~ "non-bank (part of foreign holders)",
           id %in% c("mutual funds", "insurance company", "foreign holders", "pension funds", "securities company", "individual", "others")~"non-bank", TRUE ~ "government"))
  )


#not working
# own_2016_jan <- read_xlsx("data/ownership_2016.xlsx", col_names = T, sheet = 1) %>% 
#   setNames(., c('id', format(as.Date(as.numeric(names(.)[-1]), 
#                                      origin = '1899-12-30'), '%m/%d/%Y'))) %>%
#   as_tibble() %>% 
#   pivot_longer(names_to = "date", cols = 2:22) %>% 
#   mutate(id = tolower(sub("[*]", "", id))) %>% 
#   mutate(id = gsub(".*/ ","",id)) %>% 
#   filter(!(id %in% c("government institution", "non-banks", "non-bank/non-banks"))) %>% 
#   mutate(id = gsub(".*/","",id), date = as.Date(gsub("[/]", "", date), format = "%m%d%Y")) %>% 
#   mutate(group = factor(case_when(id == "bank indonesia" ~ "government",
#                                   id == "banks" ~ "bank",
#                                   id == "incl. foreign government(s) & central bank(s) *" ~ "foreign gov (part of foreign holders)",
#                                   id %in% c("mutual funds", "insurance company", "foreign holders", "pension funds", "securities company", "individual", "others")~"non-bank", TRUE ~ "government"))
#   ) %>% 
#   mutate(year = year(date), year_month = yearmonth(date))




##### bonds bidding (unused/file deleted)
bonds <- read.csv("data/FR0087.csv", header = F) %>% 
  mutate(date = as.Date(substr(bonds$V5, 1, 8), format = "%d%m%Y"), 
         series = V2, bid_no = V1, yield = V3, bid = V4, bidder = V6, bidder_code = V8, 
         inv = gsub("\\s", "", V8), group = case_when(V10 == "CB" ~ "CB", TRUE ~ "non-CB"), custody = V11, alloted_unit = V12, proceed = V13, acr_int = V14) %>% 
  mutate(ivt = sub('.*-', '', inv), type = substr(bidder_code, 1, 2)) %>% 
  mutate(location = case_when(bidder %in% c("CITIBANK  N.A.", "KC JPMORGAN CHASE BANK  N.A.", "DEUTSCHE BANK  A.G.", "STANDCHART BANK") ~ "foreign", TRUE ~ "resident"),
         investor = case_when(
           ivt == "00" ~ bidder, TRUE ~ ivt)
  ) %>% 
  mutate(investor_type = case_when(substr(bidder_code, 4, 5) == "IB" ~ "investment bank", 
                                   substr(bidder_code, 4, 5) == "PF" ~ "pension fund",
                                   substr(bidder_code, 4, 5) == "SC" ~ "securities company",
                                   substr(bidder_code, 4, 5) == "IS" ~ "insurance")) %>% 
  mutate(investor = case_when(investor == "DanareksaSeku" ~ "BRI DANAREKSA SEKURITAS",
                              tolower(substr(investor, 1, 8)) == tolower("MandiriS") ~ "PT. MANDIRI SEKURITAS",
                              tolower(investor) == tolower("Trimegah") ~ "PT. TRIMEGAH SEKURITAS INDONESIA",
                              
                              tolower(substr(investor, 1, 6)) == tolower("Bahana") ~ "PT. BAHANA SEKURITAS",
                              TRUE ~ investor)) %>% 
  mutate(bidder_type = case_when(
    bidder %in% c("PT. MANDIRI SEKURITAS", "BRI DANAREKSA SEKURITAS", "PT Bank Mandiri (Persero), Tbk.", "PT Bank Negara Indonesia (Persero), Tbk.", "PT Bank Rakyat Indonesia, Tbk.") ~ "state-owned", TRUE ~ "private"
  )) %>% 
  select(-V1, -V2, -V3, -V4, -V6, -V5, -V7, -V8, -V9, -V10, -V11, -V12, -V13, -V14, -inv, -type, -ivt) %>% 
  as_tibble()

unique(bonds$investor_type)
typeof(bonds$investor_type)
foreign <- bonds %>% filter(bidder_type == "foreign")

bonds %>% 
  select(investor_type, yield, alloted_unit) %>% 
  filter(yield > 0, alloted_unit > 0) %>% 
  ggplot(aes(x = investor_type, y = yield))+
  geom_boxplot()

bonds %>% 
  select(investor, yield, alloted_unit) %>% 
  filter(yield > 0, alloted_unit > 0) %>% 
  ggplot(aes(x = investor, y = yield))+
  geom_boxplot()+
  coord_flip()


bonds %>% 
  select(investor_type, yield, alloted_unit, location) %>%
  group_by(investor_type) %>% 
  filter(yield > 0, alloted_unit > 0) %>% 
  ggplot(aes(x = investor_type, y = yield, color = location))+
  geom_point()


bonds %>% 
  select(bidder_type, yield, alloted_unit) %>%
  group_by(bidder_type) %>% 
  filter(yield > 0, alloted_unit > 0) %>% 
  ggplot(aes(x = bidder_type, y = yield))+
  geom_boxplot()


bonds %>% 
  select(location, yield, alloted_unit) %>%
  group_by(location) %>% 
  filter(yield > 0, alloted_unit > 0) %>% 
  ggplot(aes(x = location, y = yield))+
  geom_boxplot()

bonds %>% 
  filter(investor_type == "insurance") %>% 
  summary()

securities_comp <- bonds %>% 
  filter(investor_type == "Securities Company")

bonds %>% 
  group_by(investor_type, location) %>%
  filter(yield > 0, alloted_unit > 0) %>% 
  ggplot(aes(x = investor_type, y = yield))+
  geom_density()


yield_factor %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(avg_10y = mean(domestik_10y), inflation) %>% 
  ggplot(aes(x = avg_10y, y = inflation)) + 
  geom_point()

#foreign changes
yield_factor %>% 
  mutate(week_year = yearweek(date)) %>% 
  group_by(week_year) %>% 
  summarise(avg_10y = mean(domestic_10y), avg_chg = mean(foreign_own)) %>% 
  ggplot(aes(x = avg_chg, y = avg_10y)) + 
  geom_point()
