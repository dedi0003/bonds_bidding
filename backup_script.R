#script for reading ownership 2015
read_cleansheet <- function(filename, sheet = sheet){
  x <- read_xlsx(filename, col_names = T, sheet = sheet) %>% 
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
                                    id == "incl. foreign government(s) & central bank(s) *" ~ "non-bank (part of foreign holders)",
                                    id %in% c("mutual funds", "insurance company", "foreign holders", "pension funds", "securities company", "individual", "others")~"non-bank", TRUE ~ "government"))
    )
}

own_2015_jan <- read_cleansheet("data/ownership_2015.xlsx",sheet = 1)
own_2015_feb <- read_cleansheet("data/ownership_2015.xlsx",sheet = 2)
own_2015_mar <- read_cleansheet("data/ownership_2015.xlsx",sheet = 3)
own_2015_apr <- read_cleansheet("data/ownership_2015.xlsx",sheet = 4)
own_2015_may <- read_cleansheet("data/ownership_2015.xlsx",sheet = 5)
own_2015_jun <- read_cleansheet("data/ownership_2015.xlsx",sheet = 6)
own_2015_jul <- read_cleansheet("data/ownership_2015.xlsx",sheet = 7)
own_2015_aug <- read_cleansheet("data/ownership_2015.xlsx",sheet = 8)
own_2015_sep <- read_cleansheet("data/ownership_2015.xlsx",sheet = 9)
own_2015_oct <- read_cleansheet("data/ownership_2015.xlsx",sheet = 10)
own_2015_nov <- read_cleansheet("data/ownership_2015.xlsx",sheet = 11)
own_2015_dec <- read_cleansheet("data/ownership_2015.xlsx",sheet = 12)

ownership_2015 <- bind_rows(list(own_2015_jan, own_2015_feb, own_2015_mar, own_2015_apr, own_2015_may, own_2015_jun, own_2015_jul, own_2015_aug, own_2015_sep, own_2015_oct, own_2015_nov, own_2015_dec))

ownership_2015 <- ownership_2015 %>% 
  mutate(type = "total")

# path = "data/ownership_2016.xlsx"
# library(readxl)
# own <- read_xlsx(path)
# own_names <- read_xlsx(path, col_names = T) %>% 
#   setNames(., c('id', format(as.Date(as.numeric(names(.)[-1]), 
#                                       origin = '30-12-1899'), '%m/%d/%Y'))) %>%
#   as_tibble()
# 
# glimpse(own)
# head(own)
# names(own)




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
  summarise(avg_10y = mean(domestic_10y), inflation) %>% 
  ggplot(aes(x = avg_10y, y = inflation)) + 
  geom_point()

#foreign changes
yield_factor %>% 
  mutate(week_year = yearweek(date)) %>% 
  group_by(week_year) %>% 
  summarise(avg_10y = mean(domestic_10y), avg_chg = mean(foreign_own)) %>% 
  ggplot(aes(x = avg_chg, y = avg_10y)) + 
  geom_point()


#function read excel 2016-2021
read_newsheet <- function(filename, sheet = sheet){
  
  own <- read.xlsx(xlsxFile = filename, fillMergedCells = TRUE, colNames = F, startRow = 2, sheet = sheet) %>% 
    as.data.frame()
  
  date_header <- read.xlsx(filename, fillMergedCells = TRUE, colNames = F, sheet = sheet, rows = c(1:2), detectDates = TRUE) %>% t() %>% as.data.frame() %>% 
    do(na.locf(.)) %>%  select(1) %>% t()
  
  own_firstcol <- own %>% select(1)
  
  merge <- rbind(date_header, own %>% select(-1)) %>% as_tibble() 
  
  #add dummy row at top
  dum <- rep(NA, ncol(own_firstcol))
  own_firstcol <- rbind(dum, own_firstcol)
  
  merge_clean <- cbind(own_firstcol, merge)
  
  names(merge_clean) <- merge_clean[1, ]
  
  colnames(merge_clean)[1] <- "INSTITUTION"
  
  merge_test <- merge_clean %>% 
    pivot_longer(names_to = "date", cols = c(-1)) %>% 
    rename(id = INSTITUTION) %>% 
    filter(!id %in% c("INSTITUTION", "BANK*", "Government Institution", "NON-BANK", NA))
  
  ownership <- merge_test %>%  
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
}

read_allmonths <- function(filename){

  own_jan <- read_newsheet(filename,sheet = 1)
  own_feb <- read_newsheet(filename,sheet = 2)
  own_mar <- read_newsheet(filename,sheet = 3)
  own_apr <- read_newsheet(filename,sheet = 4)
  own_may <- read_newsheet(filename,sheet = 5)
  own_jun <- read_newsheet(filename,sheet = 6)
  own_jul <- read_newsheet(filename,sheet = 7)
  own_aug <- read_newsheet(filename,sheet = 8)
  own_sep <- read_newsheet(filename,sheet = 9)
  own_oct <- read_newsheet(filename,sheet = 10)
  own_nov <- read_newsheet(filename,sheet = 11)
  own_dec <- read_newsheet(filename,sheet = 12)

return(ownership <-
         bind_rows(list(own_jan, own_feb, own_mar, own_apr, own_may, own_jun,                                                                                       own_jul, own_aug, own_sep,
                        own_oct, own_nov, own_dec))
       )
}

#special script for 2021 Jan-Aug
read_allmonths_2021 <- function(filename){
  
  own_jan <- read_newsheet(filename,sheet = 1)
  own_feb <- read_newsheet(filename,sheet = 2)
  own_mar <- read_newsheet(filename,sheet = 3)
  own_apr <- read_newsheet(filename,sheet = 4)
  own_may <- read_newsheet(filename,sheet = 5)
  own_jun <- read_newsheet(filename,sheet = 6)
  own_jul <- read_newsheet(filename,sheet = 7)
  own_aug <- read_newsheet(filename,sheet = 8)

  
  return(ownership <-
           bind_rows(list(own_jan, own_feb, own_mar, own_apr, own_may, own_jun, own_jul, own_aug))
  )
}
ownership_2016 <- read_allmonths("data/ownership_2016.xlsx")
ownership_2017 <- read_allmonths("data/ownership_2017.xlsx")
ownership_2018 <- read_allmonths("data/ownership_2018.xlsx")
ownership_2019 <- read_allmonths("data/ownership_2019.xlsx")
ownership_2020 <- read_allmonths("data/ownership_2020.xlsx")
ownership_2021 <- read_allmonths_2021("data/ownership_2021.xlsx")

ownership_bonds <- rbind(ownership_2015, ownership_2016, ownership_2017,
                         ownership_2018, ownership_2019, ownership_2020, ownership_2021) 

saveRDS(ownership_bonds, "ownership_bonds.rds")

