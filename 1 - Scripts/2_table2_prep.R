#**************************************** Table 2 *****************************************#
#                                                                                          #
#                                                                                          #
# This code compiles datasets to be used in Table 2.                                       #
#******************************************************************************************#

#### SOURCE FILES ####
source("global_options.R")
source(here("1 - Scripts", "1_table1.R"))

####**************************** INPUT DATA ****************************#### 

# read in the OECD mortality data
c = read.csv(here("0 - Data", "oecd3.csv")) %>%
  filter(COUNTRY %in% e1$country_code) %>%
  filter(VARIABLE == "ALLCAUNB") %>%
  filter(GENDER=="TOTAL" & AGE=="TOTAL") %>%
  filter(!COUNTRY%in%c("JPN", "KOR"))

# format population data
p1 = read.csv(here("0 - Data", "HISTPOP_06102022221213356.csv")) %>%
  filter(AGE=="TOTAL" & Sex == "Total") %>% dplyr::select(Country, TIME, Value) %>%
  rename(population = "Value")
p2 = read.csv(here("0 - Data", "projections.csv")) %>%
  filter(AGE=="TOTAL" & Sex == "Total") %>% dplyr::select(Country, TIME, Value) %>%
  rename(population = "Value") %>% filter(TIME > 2021)
p = bind_rows(p1, p2) %>% group_by(Country) %>%
  mutate(population = ifelse(TIME==2022, population[TIME==2021], population))

# join to mortality data
c = c %>% left_join(p, c("Country" = "Country", "YEAR" = "TIME"))
table(is.na(c$population))

####********************** STATE DATA **********************####

####* Mortality data
states = read.csv(here("0 - Data", "state_excess_mort2.csv")) %>% 
  #filter(Type=="Predicted (weighted)") %>%
  filter(Type=="Unweighted") %>%
  group_by(Week.Ending.Date, Age.Group) %>%
  mutate(
    Number.of.Deaths = ifelse(is.na(Number.of.Deaths), 0, Number.of.Deaths),
    chk = length(Number.of.Deaths[Jurisdiction=="New York City"]),
    Number.of.Deaths = ifelse(Jurisdiction=="New York" & chk > 0, Number.of.Deaths + Number.of.Deaths[Jurisdiction=="New York City"],
                              Number.of.Deaths)) %>%
  filter(!Jurisdiction%in%c("United States", "New York City", "Puerto Rico")) %>%
  group_by(Jurisdiction, Week.Ending.Date, Year, Week) %>% summarize(deaths = sum(Number.of.Deaths)) %>%
  rename(country = Jurisdiction, YEAR = Year, WEEK = Week)

# pull state population data
p1 = read.csv(here("0 - Data", "pop_est_2010_2020.csv"))
p2 = read.csv(here("0 - Data", "Census_pop_2021.csv"))
p = p1 %>% left_join(p2, c("Area" = "Geographic.Area.Name")) %>% gather(var, value, -Area, -Estimates.Base.Population..April.1..2020) %>%
  mutate(var = as.numeric(sub("X", "", var)))


####* Vaccination data
states_out = states %>% left_join(vax %>% ungroup() %>%
                                    dplyr::select(State, fully_vax, top_10, bottom_10), c("country" = "State")) %>%
  left_join(p %>% dplyr::select(Area, var, value), c("country"="Area", "YEAR" = "var")) %>%
  mutate(pop = value)

# top 10
s1 = states_out %>% filter(top_10) %>% group_by(YEAR, WEEK) %>% 
  summarize(Value = sum(deaths, na.rm = T), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(COUNTRY = "10 most vaccinated", County = COUNTRY)

s1.1 = states_out %>% filter(top_10) %>% filter(country!="New York") %>%
  group_by(YEAR, WEEK) %>% 
  summarize(Value = sum(deaths, na.rm = T), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(COUNTRY = "9 most vaccinated", County = COUNTRY)

# bottom 10
s2 = states_out %>% filter(bottom_10) %>% group_by(YEAR, WEEK) %>% 
  summarize(Value = sum(deaths, na.rm = T), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(COUNTRY = "10 least vaccinated", County = COUNTRY)

s2.1 = states_out %>% filter(bottom_10) %>% filter(!country%in%c("North Dakota", "Indiana")) %>%
  group_by(YEAR, WEEK) %>% 
  summarize(Value = sum(deaths, na.rm = T), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(COUNTRY = "8 least vaccinated", County = COUNTRY)

# all
s5 = states_out %>% group_by(YEAR, WEEK) %>% 
  summarize(Value = sum(deaths, na.rm = T), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop), n = n()) %>% 
  mutate(COUNTRY = "USA2", Country = "USA2")

s6 = states_out %>% group_by(YEAR, WEEK, country) %>% 
  summarize(Value = sum(deaths, na.rm = T), 
            vax = weighted.mean(fully_vax, w = pop),
            population = sum(pop)) %>% 
  mutate(COUNTRY = country, Country = country)

g = s6 %>% ungroup() %>% dplyr::select(country, vax) %>% unique()
write.csv(g, file = "states_by_vax_status.csv")

states_out2 = bind_rows(s1, s2, s1.1, s2.1, s5) %>% mutate(Country = COUNTRY)

####**************************** PROCESS DATA SET ****************************####

# filter through December 2022
c2 = c %>% bind_rows(states_out2) %>% group_by(COUNTRY) %>% mutate(has_data = sum(YEAR==2022)) %>% 
  filter(has_data >= 10) %>% filter(YEAR < 2022 | WEEK <= 12) %>%
  mutate(deaths = Value, Value = deaths/population*100000,
         # set week 53 as equal to 52 for comparison
         WEEK_OLD = WEEK,
         WEEK = ifelse(WEEK==53, 52, WEEK)) %>%
  filter(COUNTRY!="IRL") # Ireland is missing data

table(c2$COUNTRY, c2$YEAR)

