#script for create UST, Yield, cds, foreign
fac1 <- readxl::read_xlsx("data/forecast_new.xlsx", col_names = T, sheet = 2) %>%
  clean_names() %>% as_tibble()

#read inflation
cpi <- read_csv("data/cpi_worldbank.csv", skip = 4) %>% 
  filter(`Country Name` == "Indonesia") %>% as_tibble()

cpi_t <- t(cpi[-1:-4]) 
colnames(cpi_t) <- cpi[ ,1]

cpi_t <- cbind(year = rownames(cpi_t), cpi_t)
rownames(cpi_t) <- NULL

cpi_t <- cpi_t %>% as_tibble() %>% 
  mutate(year = as.numeric(year), inflation = as.numeric(Indonesia)) %>% 
  select(-2) %>% drop_na()

#read ownership
own <- readxl::read_xlsx("data/kepemilikan asing.xlsx", col_names = T) %>% drop_na() %>%  janitor::clean_names() %>%  as_tibble()

own_total <- read_xlsx("data/TOTAL KEPEMILIKAN.xlsx", col_names = F) %>%
  rename(date = 1, ownership = 2) %>% 
  as_tibble()

ownership <- left_join(x = own, y = own_total, by = "date") %>% 
  filter(date >= date("2015-01-02"))

#combine all factors
yield_join <- left_join(fac1, ownership, by = "date") %>% 
  fill(c(ownership, foreign_own), .direction = "down") %>% 
  mutate(foreign_pct = foreign_own/ownership, year = year(date))

yield_factor <- left_join(yield_join, cpi_t, by = "year") %>% select(-year) %>% 
  na_replace(inflation, replacement = 1.4)

#impute inflation for 2021
na_replace(yield_factor$inflation, replacement = 1.4)

yield_factor <- yield_factor %>% 
  mutate(for_chg = foreign_pct - lag(foreign_pct, default = first(foreign_pct)), 
         up_down = case_when(for_chg < 0 ~ "down",
                             for_chg > 0 ~ "up", TRUE ~ "no change")) %>% 
  rename(domestic_10y = domestik_10y)

#add policy rate
pol_rate <- read_xlsx("data/policy_rate.xlsx", skip = 4, col_names = T) %>% 
  mutate(date = as.Date(Tanggal), rate = as.double(parse_number(`BI-7Day-RR`))) %>%
  select(date, rate)

yield_factors <- left_join(yield_factor, pol_rate, by = "date") %>% 
  fill(rate, .direction = "down")

unique(ownership_bonds$group)

cek <- ownership_bonds %>% 
  filter()
  summarise(date, id, )

yield_factors <- left_join(yield_factor)



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
    mutate(id = trimws(gsub(".*/","",id)), date = as.Date(gsub("[/]", "", date), format = "%m%d%Y")) %>% 
    mutate(group = factor(case_when(id %in% c("bank indonesia", "bank indonesia ") ~ "government",
                                    id == "banks" ~ "bank",
                                    id == "incl. foreign government(s) & central bank(s) *" ~ "non-bank (part of foreign holders)",
                                    id %in% c("mutual funds","mutual fund", "pension fund","insurance", "insurance company", "foreign holders", "pension funds", "securities company", "individual", "others")~"non-bank", TRUE ~ "government"))
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
                          str_extract(id, "net, excluding") == "net, excluding" ~ "bank indonesia",
                          id == "-gov't securities used in monetary operation with banks" ~ "-gov't securities used in monetary operation with banks",
                          TRUE ~ id))%>% 
    mutate(date = ymd(date), 
           group = factor(case_when(
             id == "bank indonesia" ~ "government",
             stringr::str_extract(id, "gross") == "gross" ~ "government (part of BI)",
             id %in% c("conventional bank", "islamic bank") ~ "bank",
             stringr::str_extract(id, "foreign government") == "foreign government" ~ "non-bank (part of foreign holders)",
             id %in% c("mutual funds","mutual fund", "pension fund","insurance", 
                       "insurance company", "foreign holders", "pension funds", 
                       "securities company", "individual", "others", "insurance and pension fund") ~ "non-bank", 
             TRUE ~ "government (part of BI)"))
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

ownership_bonds %>% 
  filter(date == "2021-08-02", group %in% c("government (part of BI)", "government"))


##add auction date
auction_2021 <- date(c('2021/01/05', '2021/01/19', '2021/02/02', '2021/02/16', '2021/03/02', '2021/03/16', 
                   '2021/03/30', '2021/04/13', '2021/04/27', '2021/05/25',
                   '2021/06/08', '2021/06/22', '2021/07/06', '2021/07/21', 
                   '2021/08/03', '2021/08/18', '2021/08/31', '2021/09/14', '2021/09/28',
                   '2021/10/12', '2021/10/26', '2021/11/09', '2021/11/23', '2021/12/07')) %>% as_tibble()

auction_2020 <- date(c('2020/01/07', '2020/01/21', '2020/02/04', '2020/02/18', '2020/03/03', '2020/03/17',
                       '2020/03/31', '2020/04/14', '2020/04/28', '2020/05/12', '2020/06/02', '2020/06/16', '2020/06/30',
                       '2020/07/14', '2020/07/28', '2020/08/11', '2020/08/25', '2020/09/08', '2020/09/22', '2020/10/06',
                       '2020/10/20', '2020/11/03', '2020/11/17', '2020/12/01')) %>% as_tibble()



auction_2019 <- date(c('2019/01/03', '2019/01/15', '2019/01/29', '2019/02/12', '2019/02/26',
                       '2019/03/12', '2019/03/26', '2019/04/09', '2019/04/23', '2019/05/07', 
                       '2019/05/21', '2019/06/18', '2019/07/02', '2019/07/16', '2019/07/30', 
                       '2019/08/13', '2019/08/27', '2019/09/10', '2019/09/24', '2019/10/08', '2019/10/22',
                       '2019/11/05', '2019/11/19', '2019/12/03')) %>% as_tibble()

auction_2018 <- date(c('2018/01/03', '2018/01/16', '2018/01/30', '2018/02/13', '2018/02/27', '2018/03/13',
                       '2018/03/27', '2018/04/10', '2018/04/24', '2018/05/08', '2018/05/22', '2018/06/05',
                       '2018/07/03', '2018/07/17', '2018/07/31', '2018/08/14', '2018/08/28', '2018/09/12', '2018/09/25',
                       '2018/10/09', '2018/10/23', '2018/11/06', '2018/11/21', '2018/12/04', '2018/12/18')) %>% as_tibble()

auction_2017 <- date(c('2017/01/03', '2017/01/17', '2017/01/31', '2017/02/14', '2017/02/28', '2017/03/14', '2017/03/27', 
                       '2017/04/11', '2017/04/25', '2017/05/09', '2017/05/23', '2017/06/06', '2017/06/20', 
                       '2017/07/11', '2017/07/25', '2017/08/08', '2017/08/22', '2017/09/05', '2017/09/19',
                       '2017/10/03', '2017/10/17', '2017/10/31', '2017/11/14', '2017/12/05')) %>% as_tibble()

auction_2016 <- date(c('2016/01/05', '2016/01/19', '2016/02/02', '2016/02/16', '2016/03/01', '2016/03/15', '2016/03/29',
                       '2016/04/12', '2016/04/26', '2016/05/10', '2016/05/24', '2016/06/07', '2016/06/21', '2016/07/19', '2016/08/02', '2016/08/16', '2016/08/30',
                       '2016/09/13', '2016/09/27', '2016/10/11', '2016/10/25', '2016/11/08')) %>% as_tibble()

auction_date <- rbind(auction_2016, auction_2017, auction_2018, auction_2019, auction_2020, auction_2021)


# read vix
vix <- read_excel("data/Data VIX_req Pega.xlsx") %>% 
  mutate(date = date(Tanggal)) %>% select(-Tanggal) %>% clean_names()

yield_factors <- left_join(yield_factors, vix, by = "date")

debt_gdp <- read_excel("data/data_macroindicator.xlsx", col_names = F) %>% clean_names()

colnames(debt_gdp) <- c("year", "month", "debt_gdp")

debt_gdp <- debt_gdp %>% mutate(yearmon = paste(year, month, sep = " "))


yield_factors <- yield_factors %>% 
  mutate(yearmon = as.character(yearmonth(date)))

yield_factors <- left_join(yield_factors, debt_gdp, by = "yearmon") %>% 
  select(-VIX, -`Chg 1D`,-`debt_gdp.x`, -vix.x, -yearmon)

yield_factors <- yield_factors %>% 
  select(-year.x, -month.x, -debt_gdp.y, -year.y, -month.y, -for_chg, -up_down)

yield_factors <- yield_factors %>% select(-debt_gdp.x, -debt_gdp.y, -x) 

yield_factors <- yield_factors %>% 
  rename(vix = vix.y)

yield_factors <- yield_factors %>% 
  rename(vix_chg = chg_1d)

yield_factors <- yield_factors %>%
  select(-foreign_pct)

