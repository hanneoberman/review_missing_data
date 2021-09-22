# final versions dataviz chapter

# MIMIC data
admin_demo <- ricu::mimic_demo$admissions %>% as_tibble()
patients_demo <- ricu::mimic_demo$patients %>% as_tibble() 
admin_demo <- admin_demo[,-1] %>% full_join(patients_demo[-1])
chart_demo <- ricu::mimic_demo$chartevents %>% as_tibble() %>% 
  select(subject_id, charttime, itemid, value) %>% 
  left_join(admin_demo[, c("subject_id", "admittime")]) %>% 
  mutate(time_passed = round(as.numeric((
    charttime - admittime
  ) / 60), 0),
  .keep = "unused")
lab_demo <- ricu::mimic_demo$labevents %>% as_tibble() %>% 
  select(subject_id, charttime, itemid, value) %>% 
  left_join(admin_demo[, c("subject_id", "admittime")]) %>% 
  mutate(time_passed = round(as.numeric((
    charttime - admittime
  ) / 60), 0),
  .keep = "unused")
in_demo <- ricu::mimic_demo$inputevents_cv %>% as_tibble() %>%
  select(subject_id, charttime, itemid, amount) 
in_demo2 <- ricu::mimic_demo$inputevents_mv %>% as_tibble() %>% 
  select(subject_id, charttime = starttime, itemid, amount) 
in_demo <- in_demo %>% 
  full_join(in_demo2) %>% 
  left_join(admin_demo[, c("subject_id", "admittime")]) %>% 
  mutate(time_passed = round(as.numeric((
    charttime - admittime
  ) / 60), 0),
  .keep = "unused")
out_demo <- ricu::mimic_demo$outputevents %>% as_tibble() %>%
  select(subject_id, charttime, itemid, value) %>% 
  left_join(admin_demo[, c("subject_id", "admittime")]) %>% 
  mutate(time_passed = round(as.numeric((
    charttime - admittime
  ) / 60), 0),
  .keep = "unused")
micro_demo <-
  ricu::mimic_demo$microbiologyevents %>% as_tibble() %>%
  select(subject_id, charttime, itemid = org_itemid, interpretation) %>% 
  left_join(admin_demo[, c("subject_id", "admittime")]) %>% 
  mutate(time_passed = round(as.numeric((
    charttime - admittime
  ) / 60), 0),
  .keep = "unused")

# monotone vs non-monotone
patterns <- rbind(
  tibble(pat = rep("Monotone", 5), rownr = 5:1, X = rep(1, 5), Y = c(1,1,1,0, 0), Z = c(1, rep(0,4))) %>% pivot_longer(cols = c(X, Y, Z)),
  tibble(pat = rep("Non-monotone", 5), rownr = 5:1, X = c(0, rep(1,4)), Y = c(1,1,1,0, 0), Z = c(1, rep(0,3), 1)) %>% pivot_longer(cols = c(X, Y, Z))) %>%
  mutate(across(everything(), as.factor))
ggplot(patterns, aes(x = name, y = rownr, fill = value, type = pat)) +
  geom_tile(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~pat) +
  theme_void()

# single imputation methods
# imp_dat <- lab_demo %>% pivot_wider(
#   id_cols = c(subject_id), 
#   names_from = itemid,
#   values_from = value,
#   values_fn = first
# ) %>% 
#   full_join(admin_demo, .)
# (oxy_miss <- sum(is.na(imp_dat$`50816`)))/nrow(imp_dat)
# imp_dat %>% 
#   rbind(NA, .) %>% 
#   mutate(oxy = as.numeric(`50816`)) %>% 
#   ggplot(aes(x = oxy, fill = !is.na(oxy))) +
#   geom_histogram(bins = 10) +
#   scale_fill_brewer(palette = "Set1") +
#   theme_classic()

imp_dat <- chart_demo %>% pivot_wider(
  id_cols = c(subject_id), 
  names_from = itemid,
  values_from = value,
  values_fn = first
) %>% 
  full_join(admin_demo, .) %>% 
  mutate(abp = as.numeric(`51`),
         admittime = as.Date.POSIXct(admittime) %>% as.numeric(),
         dob = as.Date.POSIXct(dob) %>% as.numeric(),
         age = (admittime - dob)/(365)) %>% 
  filter(age <=120)

# missing data
imp_dat %>% 
  ggplot(aes(x = age, y = abp, color = is.na(abp))) +
  geom_point(shape = 1) +
  geom_point(aes(x = age, y = -Inf), shape = 3, data = imp_dat %>% filter(is.na(abp))) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_classic() +
  labs(x = "Age (years)", y = "Systolic blood pressure (mmHg)")

# mean imp
imp_dat %>% 
  ggplot(aes(x = age, y = abp, color = is.na(abp))) +
  geom_point(shape = 1) +
  geom_point(aes(x = age, y = mean(imp_dat$abp, na.rm = TRUE)), shape = 1, data = imp_dat %>% filter(is.na(abp))) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_classic() +
  labs(x = "Age (years)", y = "Systolic blood pressure (mmHg)")

# regression imputation
coeff <- lm(abp~age, imp_dat)$coefficients
imp_dat %>% 
  ggplot(aes(x = age, y = abp, color = is.na(abp))) +
  geom_point(shape = 1) +
  geom_point(aes(x = age, y = coeff[1] + age*coeff[2]), shape = 1, data = imp_dat %>% filter(is.na(abp))) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_classic() +
  labs(x = "Age (years)", y = "Systolic blood pressure (mmHg)")

# rf imputation
