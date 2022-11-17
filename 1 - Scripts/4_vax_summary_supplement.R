#************************************** Supplement ****************************************#
#                                                                                          #
#                                                                                          #
# This code compiles datasets to be used in Table 2.                                       #
#******************************************************************************************#

#### SOURCE FILES ####
source("global_options.R")

# calculate top and bottom vaxxed states by month
vax2 = read.csv(here("0 - Data", "COVID-19_Vaccination_Trends_in_the_United_States_National_and_Jurisdictional.csv")) %>% 
  mutate(date = as.Date(Date, format = "%m/%d/%y"),
         day = day(date),
         fully_vax = Series_Complete_Pop_Pct) %>%
  filter(date_type=="Admin") %>%
  filter(!Location%in%c("AS", "PW", "PR", "MP", "GU", "FM", "MH", "US", "VI")) %>%
  filter(day == 1) %>% group_by(date) %>%
  mutate(rank_vacc = rank(-1*fully_vax),
         top_10 = rank_vacc <= 10, bottom_10 = rank_vacc >= 42) %>%
  dplyr::select(date, Location, top_10, bottom_10, rank_vacc, fully_vax) %>%
  group_by(Location) %>%
  filter(date>"2021-06-15") %>%
  mutate(calc = sum(top_10 | bottom_10),
         top_10_ct = sum(top_10)>1,
         top_10_inc = top_10[date=="2022-05-01"] | bottom_10[date=="2022-05-01"]) %>%
  filter(calc>0) %>% filter(date <= "2022-05-01") #filter(top_10 | bottom_10) 

# plot 1 (top 10 and bottom by month)
ggplot(vax2, aes(x = date, 
                 y = reorder(Location, rank_vacc), fill = top_10 | bottom_10)) + 
  geom_tile() + 
  geom_text(aes(label = rank_vacc)) + 
  facet_wrap(.~top_10_ct, scales = "free")

# plot 2 (vax rates over time)
ggplot(vax2, aes(x = date, y = fully_vax, group = paste(Location), col = top_10_inc)) + geom_line(alpha = .5) +
  theme_minimal() + ylim(0, 100) + labs(x = "", y = "2-dose vaccination rate") + 
  scale_color_manual(name = "", values = c("grey", "darkblue"), guide = "none") +
  scale_shape_discrete(name = "", guide = "none") + 
  geom_point(aes(pch = top_10 | bottom_10), alpha = .5)
ggsave(here("3 - Supplement", "vaccination_rate.png"), width = 4, height = 3)

# included states
vax2 %>% group_by(Location) %>% filter(top_10_inc) %>% dplyr::select(Location) %>% unique()
