#**************************************** Table 2 *****************************************#
#                                                                                          #
#                                                                                          #
# This code compiles datasets to be used in Table 2.                                       #
#******************************************************************************************#

#### SOURCE FILES ####
source("global_options.R")
source(here("1 - Scripts", "2_table2_prep.R"))

# set up code to run model
c3 = c2 %>% mutate(YEAR_WEEK = paste(YEAR, ifelse(WEEK<10, paste("0", WEEK, sep = ""), WEEK), sep = "-"),
                   trt_week = ifelse(YEAR>=2020, YEAR_WEEK, 0),
                   country_week = paste(Country, WEEK, sep = "_"),
                   country_trt_week = ifelse(YEAR>=2020, paste(Country, trt_week, sep = "."), 0),
                   week_char = paste("W:", WEEK)) %>%
  filter(!Country%in%c("United States", "9 most vaccinated", "8 least vaccinated")) %>%
  group_by(YEAR_WEEK) %>% mutate(w = population/sum(population))

####************************** TRAINING *******************************####
# test models
test_function = function(c2, weeks = c(1:53), test_year = 2019,
                          test_weeks = c(1:53), k = 1){
  
  # filter data
  c3.train = c2 %>% filter(YEAR < test_year & WEEK %in% weeks)
  c3.test = c2 %>% filter(YEAR == test_year & WEEK %in% test_weeks)
  
  # run alternative specifications
  lm1 = lm(Value~country_week, 
           data = c3.train %>% filter(YEAR == test_year-k))
  lm2 = lm(Value~Country*factor(WEEK) + Country*YEAR, 
           data = c3.train %>% filter(YEAR <= test_year-k))
  lm3 = lmer(Value~Country*factor(WEEK) +
               (0+YEAR|Country), REML = TRUE, 
             data = c3.train %>% filter(YEAR <= test_year-k))
   
  # run predictions
  c3.test$preds1 = predict(lm1, c3.test) 
  c3.test$preds2 = predict(lm2, c3.test)
  c3.test$preds3 = predict(lm3, c3.test) 
  
  # evaluate predictions
  out = c3.test %>%
    gather(var, value, preds1, preds2, preds3) %>%
    group_by(Country, var, YEAR) %>%
    summarize(mse = (sum((value-Value)^2)), abs.err = abs(sum(value-Value)), 
              err = sum(value - Value), n = n())
  
  return(out)
}

# specify weeks
weeks_all = c(1:52)
weeks1 = 26:51
weeks2 = c(1:12, 52)

#### 1 year ahead
# global models
out2 = bind_rows(test_function(c3, weeks = weeks1, test_weeks = weeks1, test_year = 2019) %>% mutate(id = "Per2", type = "week")) %>%
  bind_rows(test_function(c3, weeks = weeks1, test_weeks = weeks1, test_year = 2018) %>% mutate(id = "Per2", type = "week")) %>%
  bind_rows(test_function(c3, weeks = weeks2, test_weeks = weeks2, test_year = 2019) %>% mutate(id = "Per3", type = "week")) %>%
  bind_rows(test_function(c3, weeks = weeks2, test_weeks = weeks2, test_year = 2018) %>% mutate(id = "Per3", type = "week")) %>%
  bind_rows(test_function(c3, weeks = weeks_all, test_weeks = weeks1, test_year = 2019) %>% mutate(id = "Per2", type = "weeks_all")) %>%
  bind_rows(test_function(c3, weeks = weeks_all, test_weeks = weeks1, test_year = 2018) %>% mutate(id = "Per2", type = "weeks_all")) %>%
  bind_rows(test_function(c3, weeks = weeks_all, test_weeks = weeks2, test_year = 2019) %>% mutate(id = "Per3", type = "weeks_all")) %>%
  bind_rows(test_function(c3, weeks = weeks_all, test_weeks = weeks2, test_year = 2018) %>% mutate(id = "Per3", type = "weeks_all"))

# check counts
out2 = out2 %>% group_by(var, type, YEAR, id) %>% mutate(chk = n())
table(out2$chk) # predictions for each country
table(out2$n)

# process data
out3 = out2 %>% mutate(est = paste(var, type)) %>%
  group_by(Country, est, id, YEAR) %>% summarize(mse_tot = sum(mse), err = abs(sum(err))) %>%
  group_by(Country, id, YEAR) %>% mutate(rank_mse = rank(mse_tot), rank_err = rank(err)) %>%
  mutate(id = ifelse(id=="Per2", "Delta weeks (26-51)", "Omicron weeks (52-53, 1-12)"))
  
temp = out3 %>% group_by(id, est, YEAR) %>% summarize(rmse = sqrt(mean(mse_tot)),
                                               rmse_c = mean(sqrt(mse_tot)),
                                               rmse_cp = sqrt(mean(err^2)), mean_rank_mse = mean(rank_mse),
                                               mean_rank_err = mean(rank_err)) %>%
  gather(var, value, rmse, rmse_cp, mean_rank_mse)

# plot data
ggplot(temp, aes(x = reorder(est, value), y = value, fill = factor(paste(YEAR, id)))) + 
         geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(.~var, scales = "free", ncol = 3) + 
  geom_text(aes(label = round(value,1)), size = 3,
            position=position_stack(vjust=0.5)) + 
  theme_bw() +
  scale_fill_brewer("Test Year & Weeks", palette = "Set2") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "", y = "")


#### 2 years ahead
out = bind_rows(test_function(c3, weeks = weeks1, test_weeks = weeks1, test_year = 2019, k = 2) %>% mutate(id = "Per2", type = "week")) %>%
  bind_rows(test_function(c3, weeks = weeks2, test_weeks = weeks2, test_year = 2019, k = 2) %>% mutate(id = "Per3", type = "week")) %>%
  bind_rows(test_function(c3, weeks = weeks_all, test_weeks = weeks1, test_year = 2019, k = 2) %>% mutate(id = "Per2", type = "weeks_all")) %>%
  bind_rows(test_function(c3, weeks = weeks_all, test_weeks = weeks2, test_year = 2019, k = 2) %>% mutate(id = "Per3", type = "weeks_all"))

# make plots
out_2 = out %>% mutate(est = paste(var, type)) %>%
  group_by(Country, est, id, YEAR) %>% summarize(mse_tot = sum(mse), err = abs(sum(err))) %>%
  group_by(Country, id, YEAR) %>% mutate(rank_mse = rank(mse_tot), rank_err = rank(err),
                                         id = ifelse(id=="Per2", "Delta weeks (26-51)", "Omicron weeks (52-53, 1-12)")) %>%
  group_by(id, est, YEAR) %>% summarize(rmse = sqrt(mean(mse_tot)),
                                        rmse_c = mean(sqrt(mse_tot)),
                                        rmse_cp = sqrt(mean(err^2)), mean_rank_mse = mean(rank_mse),
                                        mean_rank_err = mean(rank_err)) %>%
  gather(var, value, rmse, rmse_cp, mean_rank_mse) 

ggplot(out_2, aes(x = reorder(est, value), y = value, fill = factor(id))) + geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(.~var, scales = "free") + 
  geom_text(aes(label = round(value,1)), size = 3,
            position=position_stack(vjust=0.5)) + 
  theme_bw() +
  scale_fill_brewer("Test Weeks", palette = "Set2") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "", y = "")

####**************************** ESTIMATE EXCESS DEATHS ****************************####

# run model to estimate main effects
lm3 = lmer(Value~country_week + (0+YEAR|Country), 
           data = c3 %>% filter(YEAR <= 2019))

summary(lm3)

# run model for inference
tic()
lm3.5 = lmer(Value~country_trt_week + country_week + (0+YEAR|Country), 
             data = c3)
toc()
save(lm3.5, file = here("2 - Output", "lm_output.RData"))
load(here("2 - Output", "lm_output.RData"))

# estimate table 2 with a given functional form
year_week1 = c(paste("2020", 12:53, sep = "-"), paste("2021",  paste("0", 1:9, sep = ""), sep = "-"), paste("2021", 10:25, sep = "-"))
year_week2 = c(paste("2021", 26:53, sep = "-"), paste("2022",  c(paste("0", 1:9, sep = ""), 10:12), sep = "-"))

est_preds = function(lm3 = lm3){
  # run predictions for Period 2
  c4 = c3 %>% filter(YEAR_WEEK %in% year_week1) %>% ungroup() 
  c4$temp = predict(lm3, c4)
  c4 = c4 %>% mutate(pred = Value - temp,
                     per = "Period 2")
  
  # run predictions for Period 3
  c5 = c3 %>% filter(YEAR_WEEK %in% year_week2) 
  c5$temp = predict(lm3, c5)
  c5 = c5 %>% mutate(pred = Value - temp, 
                     per = "Period 3")
  
  # combine matrices         
  c_out = bind_rows(c4, c5) %>% dplyr::select(Country, per, pred) %>%
    group_by(Country, per) %>% summarize(pred = sum(pred))
  
  # create a table
  tbl2 = c_out %>% spread(per, pred) %>% 
    mutate(`PeriodTot` = `Period 2` + `Period 3`) %>%
    gather(per, pred, `Period 2`, `Period 3`, PeriodTot) %>%
    group_by(per) %>%
    filter(Country!="United States") %>%
    mutate(value2 = pred[Country=="USA2"] - pred,
           v2 = paste(comma(round(value2*pop/100000)), " (", round(value2/pred[Country=="USA2"]*100), ")",  sep = ""),
           v1 = round(pred, 1)) %>%
    ungroup() %>%
    dplyr::select(-value2, -pred) %>%
    gather(var_new, value_new, v1, v2) %>%
    mutate(var_keep = paste(var_new, per)) %>% dplyr::select(-per, -var_new) %>%
    spread(var_keep, value_new) %>% arrange(as.numeric(`v1 PeriodTot`)) %>%
    # join to vaccination data
    left_join(vax2, c("Country" = "Country")) %>%
    left_join(states_out2 %>% ungroup() %>%  dplyr::select(COUNTRY, vax) %>% mutate(vax = round(vax)) %>%
                unique(),
              c("Country" = "COUNTRY")) %>%
    mutate(vax = ifelse(is.na(vax.x), round(vax.y), round(vax.x)),
           Country = ifelse(Country=="USA2", "United States", Country),
           model = "choice")
}

# table 2
tbl2 = est_preds(lm3) %>% mutate(model = "RF")
tbl2_out = tbl2[,c(1,11,2:7)]
View(tbl2_out)
write.csv(tbl2_out, file = here("2 - Output", "tbl2_out_full_pandemic.csv"))

# table 2 - FE
lm3_FE = lm(Value~Country*WEEK + YEAR*Country, 
            data = c3 %>% filter(YEAR <= 2019))
tbl2_FE = est_preds(lm3_FE) %>% mutate(model = "FE")

# table 2 - 2019
lm3_2019 = lm(Value~country_week, 
              data = c3 %>% filter(YEAR %in% c(2019)))
tbl2_2019 = est_preds(lm3_2019) %>% mutate(model = "2019_only")

# table 2 - RF weighted by pop
lm3_w = lm(Value~country_week, 
                  data = c3 %>% filter(YEAR <= 2019))
tbl2_w = est_preds(lm3_w) %>% mutate(model = "RF_weighted_by_pop")

# compare estimates as sensitivity analysis
ests_combined = bind_rows(tbl2, tbl2_FE, tbl2_2019, tbl2_w) %>%
  mutate(model = factor(model, levels = c("RF", "FE", "2019_only", "RF_weighted_by_pop"))) %>%
  gather(var, value, `v1 Period 2`, `v1 Period 3`) %>%
  group_by(var, model) %>% mutate(rank = rank(as.numeric(value)), n = n(),
                                  var = sub("v1 |v2 ", "", var),
                                  var = ifelse(var=="Period 2", "Delta", "Omicron"))

# plots of sensitivity analysis
# ranks aren't v sensitive, but absolute values are somewhat

# delta
ggplot(ests_combined %>% filter(var=="Delta"), aes(x = as.numeric(rank), y = model, fill = as.numeric(value))) + 
  geom_tile() + coord_flip() + theme_bw() +
  geom_text(aes(label = Country), col = "white") + 
  theme_bw() + 
  scale_fill_viridis("Excess mortality per 100K") + labs(y = "", x = "Rank", title = "Delta")

# omicron
ggplot(ests_combined %>% filter(var=="Omicron"), aes(x = as.numeric(rank), y = model, fill = as.numeric(value))) + 
  geom_tile() + coord_flip() + theme_bw() +
  geom_text(aes(label = Country), col = "white") + 
  theme_bw() + 
  scale_fill_viridis("Excess mortality per 100K") + labs(y = "", x = "Rank", title = "Omicron")

# now pull parameters to check that they match and obtain CIs
params = summary(lm3.5)$coeff
params0 = data.frame(vars = rownames(params), params)

pull1 = paste(paste("", year_week1, sep = ""), collapse = "|", sep = "")
params1 = params0 %>% filter(grepl(pull1, vars)) %>% 
  mutate(vars = sub("country_trt_week", "", vars)) %>%
  separate(vars, into = c("Country", "Time"), sep = "\\.") %>%
  group_by(Country) %>% summarize(sum(Estimate))

View(params1)

####************************* INFERENCE************************#### 
vcov_use = vcov(lm3.5)
#vcov_alt = vcovCR(lm3.5, type = "CR1")

# run mortality regressions
deaths_reg = function(comp1 = "USA2", comp2 = "Austria",
                      dates = year_week1, vcov_keep = vcov_use){
  
  # pull country 1
  vars1 = paste("country_trt_week", comp1, ".", dates, sep = "")
  vars2 = paste("country_trt_week", comp2, ".", dates, sep = "")
  
  # set up indicator vector
  ind = matrix(0, nrow(params0), 1)
  ind[params0$vars%in%vars1] = 1
  ind[params0$vars%in%vars2] = -1
  
  # run calculations
  est = (t(ind)%*%params0$Estimate)
  var = t(ind)%*%vcov_keep%*%ind
  test_stat = est[1]/sqrt(var[1])
  
  # form linear hypothesis
  p.val = 2*(1-pnorm(abs(test_stat)))
  
  # return p-value
  return(data.frame(comp1, comp2, p = p.val, est = est) %>%
           mutate(chk = ifelse(p < .0001, "**", "*"),
                  chk = ifelse(p > .005, "", chk)))
}

# run test
#linearHypothesis(lm3.5, "country_trt_weekUSA2.2021-18 + country_trt_weekUSA2.2021-19 - country_trt_weekUnited States.2021-18 - country_trt_weekUnited States.2021-19=0")
#deaths_reg("USA2", "United States", dates = c("2021-18", "2021-19"))

# p-values over different comparisons
comps = unique(c2$Country)
p.vals = data.frame()
for(i in comps[!comps%in%c("USA2", "United States", "10 most vaccinated", "10 least vaccinated", 
                           "9 most vaccinated", "8 least vaccinated")]){
  p.vals1 = deaths_reg(comp1 = "USA2", comp2 = i, year_week1) %>% mutate(per = "Period 2")
  p.vals2 = deaths_reg(comp1 = "USA2", comp2 = i, year_week2) %>% mutate(per = "Period 3")
  p.vals = rbindlist(list(p.vals, p.vals1, p.vals2))
}
View(p.vals)

p.vals = data.frame()
for(i in comps[!comps%in%c("USA2", "United States", "10 most vaccinated", "9 most vaccinated")]){
  p.vals1 = deaths_reg(comp1 = "10 most vaccinated", comp2 = i, year_week1) %>% mutate(per = "Period 2")
  p.vals2 = deaths_reg(comp1 = "10 most vaccinated", comp2 = i, year_week2) %>% mutate(per = "Period 3")
  p.vals = rbindlist(list(p.vals, p.vals1, p.vals2))
}
View(p.vals)

p.vals = data.frame()
for(i in comps[!comps%in%c("USA2", "United States", "10 least vaccinated", "8 least vaccinated")]){
  p.vals1 = deaths_reg(comp1 = "10 least vaccinated", comp2 = i, year_week1) %>% mutate(per = "Period 2")
  p.vals2 = deaths_reg(comp1 = "10 least vaccinated", comp2 = i, year_week2) %>% mutate(per = "Period 3")
  p.vals = rbindlist(list(p.vals, p.vals1, p.vals2))
}
View(p.vals)
