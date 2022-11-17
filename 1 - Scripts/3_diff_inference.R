#*********************************** Additional analysis **********************************#
#                                                                                          #
#                                                                                          #
# This code estimates inference on differences between countries.                          #
#******************************************************************************************#

#### SOURCE FILES ####
source("global_options.R")
source(here("1 - Scripts", "2_table2_prep.R"))

####**************************** ESTIMATE EXCESS vs. COVID-19 DEATHS ****************************####

# set up code to run model
c3 = c2 %>% mutate(YEAR_WEEK = paste(YEAR, ifelse(WEEK<10, paste("0", WEEK, sep = ""), WEEK), sep = "-"),
                   YEAR_WEEK_OLD = paste(YEAR, ifelse(WEEK_OLD<10, paste("0", WEEK_OLD, sep = ""), WEEK_OLD), sep = "-"),
         trt_week = ifelse(YEAR>=2020, YEAR_WEEK, 0),
         country_week = paste(Country, WEEK, sep = "_"),
         country_trt_week = ifelse(YEAR>=2020, paste(Country, trt_week, sep = "."), 0),
         week_char = paste("W:", WEEK)) %>%
  filter(!Country%in%c("United States", "9 most vaccinated", "8 least vaccinated")) %>%
  left_join(e2 %>% group_by(country) %>%
              mutate(country = ifelse(country=="The United Kingdom", "United Kingdom", country),
                     weekly_count = ifelse(year_week=="2021-52", 
                     weekly_count[year_week=="2021-52"] + weekly_count[year_week=="2021-53"],
                     weekly_count)),
                     #country = ifelse(country=="United States of America", "USA2", country)), 
            c("YEAR_WEEK" = "year_week", "Country" = "country")) %>%
  mutate(weekly_count = ifelse(YEAR < 2020, 0, weekly_count),
         population.y = ifelse(is.na(population.y), 100, population.y),
         weekly_per_cap = weekly_count/population.x*100000, 
         diff = Value - weekly_per_cap)

# run model to estimate main effects
lm3 = lmer(diff~country_week + (0+YEAR|Country), 
           data = c3 %>% filter(YEAR <= 2019))
lm4 = lmer(diff~country_trt_week + country_week + (0+YEAR|Country), 
             data = c3)

save(lm4, file = here("2 - Output", "lm_output_diff.RData"))
load(here("2 - Output", "lm_output_diff.RData"))

# run predictions for Period 2
year_week1 = paste("2021", 26:51, sep = "-")

# run predictions for Period 3
year_week2 = c(paste("2021", 52:53, sep = "-"), paste("2022",  c(paste("0", 1:9, sep = ""), 10:12), sep = "-"))

# setup for inference
params0 = data.frame(vars = rownames(params), params)

pull1 = paste(paste("", year_week1, sep = ""), collapse = "|", sep = "")
vcov_use = vcov(lm4)

####************************* INFERENCE************************#### 

# run mortality regressions
deaths_reg = function(comp1 = "USA2", 
                      dates = year_week1, vcov_keep = vcov_use){
  
  # pull country 1
  vars1 = paste("country_trt_week", comp1, ".", dates, sep = "")

  # set up indicator vector
  ind = matrix(0, nrow(params0), 1)
  ind[params0$vars%in%vars1] = 1

  # run calculations
  est = (t(ind)%*%params0$Estimate)
  var = t(ind)%*%vcov_keep%*%ind
  test_stat = est[1]/sqrt(var[1])

  # form linear hypothesis
  p.val = 2*(1-pnorm(abs(test_stat)))

  # return p-value
  return(data.frame(comp1, p = round(p.val, 5), est = est) %>%
           mutate(chk = ifelse(p < .0001, "**", "*"),
                  chk = ifelse(p > .005, "", chk)))
}

# p-values over different comparisons
comps = unique(c3$Country)
p.vals = data.frame()
for(i in comps){
  p.vals1 = deaths_reg(comp1 = i, year_week1) %>% mutate(per = "Period 2")
  p.vals2 = deaths_reg(comp1 = i, year_week2) %>% mutate(per = "Period 3")
  p.vals = rbindlist(list(p.vals, p.vals1, p.vals2))
}
View(p.vals) # use top 10 comparisons for paper

