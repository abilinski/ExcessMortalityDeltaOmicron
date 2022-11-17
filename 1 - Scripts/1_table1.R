#**************************************** Table 1 *****************************************#
#                                                                                          #
#                                                                                          #
# This code generates Table 1.                                                             #
#******************************************************************************************#

#### SOURCE GLOBAL OPTIONS ####
# source global options
source("global_options.R")

#### STATE PREPARATION ####

# import mortality data from US states
us = read.csv(here("0 - Data", "deaths_state2.csv")) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y"),
         year = year(date),
         week = epiweek(date), 
         year_week = ifelse(week<10, paste(year, week, sep = "-0"), paste(year, week, sep = "-")),
         state = ifelse(state=="NYC", "NY", state)) %>% 
  arrange(state, year, week) %>% 
  group_by(state, year_week) %>% 
  summarize(deaths = sum(new_death))

# import state vaccination data
data(state_census)
state_pops = read.csv(here("0 - Data", "Census_pop_2021.csv")) %>%
  left_join(state_census, c("Geographic.Area.Name" = "NAME")) %>%
  filter(!is.na(REGION))

# import vaccination data
vax = read.csv(here("0 - Data", "COVID-19_Vaccination_Trends_in_the_United_States_National_and_Jurisdictional.csv")) %>% 
  mutate(date = as.Date(Date, format = "%m/%d/%y"),
         day = day(date),
         fully_vax = Series_Complete_Pop_Pct) %>%
  filter(date_type=="Admin") %>%
  filter(!Location%in%c("AS", "PW", "PR", "MP", "GU", "FM", "MH", "US", "VI")) %>%
  filter(date == "2022-01-01") %>% group_by(date) %>%
  mutate(rank_vacc = rank(-1*fully_vax),
         top_10 = rank_vacc <= 10, bottom_10 = rank_vacc >= 42,
         abbv = Location, 
         State = sapply(abbv, function(a) ifelse(a=="DC", "District of Columbia", state.name[which(state.abb==a)]))) %>% 
  left_join(state_pops, c("abbv" = "ABBR")) %>%
  mutate(pop = X2021)

# pull the most- and least-vaccinated states
vax_by_states = vax %>% 
  filter(top_10 | bottom_10) %>%
  mutate(Set = ifelse(top_10, "Top 10", "Bottom 10")) %>%
  ungroup() %>%
  dplyr::select(State, fully_vax, rank_vacc, Set) %>%
  group_by(Set) %>% 
  rename(Rate = "fully_vax", Rank = "rank_vacc") %>%
  arrange(Rank)
df = apply(vax_by_states,2,as.character)
write.csv(df, file = here("3 - Supplement", "Vax_by_states.csv"))

# merge mortality and vaccination data
states = us %>% left_join(vax, c("state" = "abbv"))

## pull & summarize state subgroups
# top 10
s1 = states %>% filter(top_10) %>% group_by(year_week) %>% 
  summarize(weekly_count = sum(deaths), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(country = "10 most vaccinated")

# top 9 -- consistent
s1.1 = states %>% filter(top_10) %>% filter(state!="NY") %>%
  group_by(year_week) %>% 
  summarize(weekly_count = sum(deaths), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(country = "9 most vaccinated")

# bottom 10
s2 = states %>% filter(bottom_10) %>% group_by(year_week) %>% 
  summarize(weekly_count = sum(deaths), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(country = "10 least vaccinated")

# bottom 8 -- consistent
s2.1 = states %>% filter(bottom_10) %>% filter(!state%in%c("ND", "IN")) %>%
  group_by(year_week) %>% 
  summarize(weekly_count = sum(deaths), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(country = "8 least vaccinated")

# all US states for comparison
s0 = states %>%
  group_by(year_week) %>% 
  summarize(weekly_count = sum(deaths, na.rm = T), 
            vax = weighted.mean(fully_vax, w = pop, na.rm = T),
            population = sum(pop, na.rm = T)) %>% 
  mutate(country = "USA2") #%>% mutate(population = 331002647, pop = 331002647)


# bind together
s = bind_rows(s0, s1, s2, s1.1, s2.1)

#### COUNTRY PREPARATION ####

# import mortality data
e0 = read.csv(here("0 - Data", "WHO-COVID-19-global-data.csv"))

# import country codes
data(codelist)
codelist = codelist %>% dplyr::select(iso2c, iso3c)

# List of OECD countries, obtained from OECD website
oecd = unlist(read.csv(here("0 - Data", "oecd_countries.csv")) %>% dplyr::select(Country.formatted))

# GDP per capita, from World Bank
gdp = read.csv(here("0 - Data", "API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4578209.csv")) %>% mutate(gdp_2021 = X2021) %>% dplyr::select(Country.Code, gdp_2021)

# population from OECD website
pop = read.csv(here("0 - Data", "HISTPOP_06102022221213356.csv")) %>% 
  filter(Sex == "Total" & Time == 2021 & Age=="Total") %>%
  mutate(popKEEP = Value) %>% dplyr::select(LOCATION, popKEEP)

#### 2. SELECT COUNTRIES IN ECDC DATA ####

# OECD
e1a = e0 %>% filter(Country%in%oecd) %>% left_join(codelist, c("Country_code" = "iso2c")) %>% mutate(country_code = iso3c)
#CHECK: length(oecd)==length(unique(e1a$Country))
#oecd[!oecd%in%e1a$Country]

# Match to GDP
e1b = e1a %>% left_join(gdp, c("country_code" = "Country.Code")) %>% left_join(pop, c("country_code" = "LOCATION")) %>%
  mutate(population = popKEEP)
#CHECK: sum(is.na(e1b$gdp_2021))
#CHECK: sum(is.na(e1b$popKEEP))
#CHECK: table(e1b$gdp_2021)


# FILTER ON GDP AND POPULATION
e1c = e1b %>% filter(population>5e6) %>% filter(gdp_2021 > 30000)

# assemble data
e1 = e1c %>% mutate(date = as.Date(Date_reported, "%Y-%m-%d"), year = year(date), week = epiweek(date),
                    week = ifelse(week<10, paste("0", week, sep = ""), week),
                    year_week = paste(year, week, sep = "-")) %>%
  group_by(year_week, year, week, country_code, Country, population) %>% summarize(weekly_count = sum(New_deaths)) %>%
  mutate(country = Country) %>%
  ungroup()

# pull vaccination data
v = read.csv(here("0 - Data", "owid-covid-data.csv")) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  mutate(date_diff = abs(date-as.Date("2022-01-01")),
         vax = people_fully_vaccinated_per_hundred) %>%
  group_by(iso_code) %>% filter(date_diff==min(date_diff)) %>%
  filter(iso_code%in%e1$country_code) %>% dplyr::select(iso_code, vax)

# join vaccination data to country data
vax2 = v %>% left_join(e1 %>% dplyr::select(country_code, Country) %>% unique(), c("iso_code" = "country_code")) 
mean(e1$country_code%in%v$iso_code)
table(v$iso_code)

#### CREATE FUNCTIONS FOR RATES AND DEATH ESTIMATES

# bind states and ECDC
e2 = bind_rows(e1 %>% left_join(vax2, c("Country")), s)

# extract US population
pop = e2$population[e2$country=="USA2"][1]

# estimate values
est_date = function(e2, weeks = c("2020-09", "2020-10"), US_pop = pop, n = 1){
  
  e3 = e2 %>% group_by(country) %>% filter(year_week %in% weeks) %>%
    # calculate death rate
    summarize(pop = population[1], vax = round(vax[1]),
           count = sum(weekly_count, na.rm = T)/n,
           rate = count/pop*100000) %>% 
    # calculate death difference
    mutate(total =  (rate[country=="USA2"]-rate)/1e5*US_pop,
           US_max = count[country=="USA2"])
  return(e3)
}

#### RUN ESTIMATES FOR TIME POINTS OF INTEREST ####

# select dates
set2 = paste("2021", 26:51, sep = "-") 
set3 = c(paste("2021", 52:53, sep = "-"), paste("2022", 1:9, sep = "-0"), paste("2022", 10:12, sep = "-"))
set = list(set2, set3, c(set2, set3))
labs = c("2. Delta", "3. Omicron", "4. Post-vaccination total")

run_table = function(e2, by_month = F){
# run dates
est_date_out = data.frame()
  for(i in 1:length(set)){
    if(!by_month){
      divide_by = 1
      round = 1
    }else {
      divide_by = length(set[[i]])
      round = 1}
    temp = est_date(e2, weeks = set[[i]], n = divide_by) %>% mutate(lab = labs[i])
    est_date_out = est_date_out %>% bind_rows(temp)
  }
  
  # reshape data frame
  tbl1 = est_date_out %>%
    dplyr::select(-count, -pop) %>% gather(var, value, rate, total) %>%
    mutate(lab = paste(var, lab),
           # format values
           value = ifelse(var=="rate", round(value,round), 
                          paste(comma(value,1), " (", round(value/US_max*100), ")", sep = ""))) %>% 
    dplyr::select(-var, -US_max) %>% spread(lab, value) %>% arrange(as.numeric(`rate 4. Post-vaccination total`)) %>%
    mutate(country = ifelse(country=="USA2", "United States", country))
  
  return(list(tbl1, est_date_out))
}

# run primary table
tbl1 = run_table(e2 %>% filter(country!="United States of America") %>% filter(!grepl("9|8", country)))
#View(tbl1[[1]])
write.csv(tbl1[[1]], file = here("2 - Output", "tbl1_out.csv"))

# run table separated by month
tbl_s1 = run_table(e2 %>% filter(!grepl("9|8", country)), by_month = T) 
write.csv(tbl_s1[[1]], file = here("3 - Supplement", "tbls1_out.csv"))

# run table with additional comparators
tbl_s3 = run_table(e2)
write.csv(tbl_s3[[1]], file = here("3 - Supplement", "tbls3_out.csv"))
#View(tbl_s3[[1]])

# run table with sensitivity analysis on time
set2 = paste("2021", 31:51, sep = "-")
set3 = c(paste("2022", 2:9, sep = "-0"), paste("2022", 10, sep = "-"))
set = list(set2, set3, c(set2, set3))

tbl_s7 = run_table(e2 %>% filter(!grepl("9|8", country)), by_month = F) 
#View(tbl_s7[[1]])
write.csv(tbl_s7[[1]], file = here("3 - Supplement", "tbls7_out.csv"))


#### 9. RUN REGRESSIONS ####
est_date_out = tbl1[[2]]

# COMPARISONS TO US
regs = est_date_out %>% 
  # reorder factor levels so US is first in factor
  mutate(location_name = factor(country, levels = rev(sort(unique(country)))))

# Delta
summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs %>% filter(lab=="2. Delta" & !grepl("vaccinated", location_name)), family = poisson()))
summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs %>% filter(lab=="2. Delta" & grepl("vaccinated", location_name)), family = poisson()))

# Omicron
summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs %>% filter(lab=="3. Omicron" & !grepl("vaccinated", location_name)), family = poisson()))
summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs %>% filter(lab=="3. Omicron" & grepl("vaccinated", location_name)), family = poisson()))

# COMPARISONS TO BOTTOM 10
levs = rev(levels(regs$location_name))
regs2 = regs %>% ungroup() %>% filter(!country%in%"USA2") %>%
  mutate(location_name = factor(location_name, levels = levs))

summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs2 %>% filter(lab=="2. Delta"), family = poisson()))

summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs2 %>% filter(lab=="3. Omicron"), family = poisson()))

# COMPARISONS TO TOP 10
levs2 = rev(levels(regs$location_name))[c(2,1,3:21)]
regs3 = regs %>% ungroup() %>% filter(!country%in%"USA2") %>%
  mutate(location_name = factor(location_name, levels = levs2))

summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs3 %>% filter(lab=="2. Delta"), family = poisson()))

summary(glm(formula = count~location_name, offset = log(pop), 
            data = regs3 %>% filter(lab=="3. Omicron"), family = poisson()))
