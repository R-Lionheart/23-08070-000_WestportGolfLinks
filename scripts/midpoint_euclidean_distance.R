

profile.pattern <- "prof_16|prof_17"
source("scripts/src/assign_profile_parks.R")


## Euclidean distances: in feet
euclidean <- complete.profile %>%
  group_by(profile, year) %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2)) %>%
  select(profile, Park, year, BasePoint_X, BasePoint_Y, x_midpoint,  y_midpoint) %>%
  unique() %>%
  arrange(profile, year) %>%
  group_by(profile) %>%
  mutate(feet_to_BP = (sqrt(((BasePoint_X - x_midpoint)^2) +
                                  ((BasePoint_Y -  y_midpoint)^2))) *  3.28084) %>% ## Convert m to feet
  drop_na() %>%
  mutate(net_profile_slope = ifelse(feet_to_BP[which.min(year)] < feet_to_BP[which.max(year)],
                                "Accretion", "Erosion")) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47")))


### Extract equation parameters
equation.details <- euclidean %>%
  select(profile, Park, year, feet_to_BP) %>%
  unique() %>%
  drop_na() %>%
  group_by(profile) %>%
  mutate(dummy_year = row_number()) %>%
  do(model = lm(feet_to_BP ~ dummy_year, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2],
         rsq = summary(model)$r.squared,
         se = summary(model)$sigma,
         pvalue = glance(model)$p.value) %>%
  mutate(profile_direction = ifelse(slope > 0, "Accretion", "Erosion")) %>%
  select(profile, slope, rsq, se, pvalue) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47")))

## All data combined to one df
toplot <- euclidean %>%
  left_join(equation.details, by = "profile") %>%
  mutate(shoreline_profile = ifelse(pvalue < 0.05 & slope > 0, "Significant Accretion",
                          ifelse(pvalue > 0.05 & slope > 0, "Non Significant Accretion", 
                                 ifelse(pvalue < 0.05 & slope < 0, "Significant Erosion", 
                                        ifelse(pvalue > 0.05 & slope < 0, "Non Significant Erosion", "Other"))))) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47")))


## Visualize euclidean distance from average Euclidean distance of each year
midpoint.euc.dist.plot <- ggplot(toplot %>% drop_na(), 
                                 aes(year, feet_to_BP, 
                                     fill = shoreline_profile, group = shoreline_profile)) +
  scale_fill_manual(values=c("grey55", "grey54", "#04A1FF", "tomato2", "grey")) +
  facet_wrap(~Park) +
  geom_col(position = position_dodge(width = 1)) +
  geom_line(aes(group = shoreline_profile), position = position_dodge(width = 1),
            linewidth = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="black") +
  xlab("Year") +
  ylab("Distance in feet from BasePoint") + 
  theme(axis.text.y = element_blank()) +
  guides(fill=guide_legend(title="")) +
  ggtitle("Net Accretion or Erosion per Profile")
midpoint.euc.dist.plot


## Pearson's correlation 
pearson.correlation <- toplot %>%
  group_by(profile) %>%
  mutate(dummy_year = row_number()) %>%
  group_by(profile) %>%
  summarize(pearson_correlation=cor(dummy_year, feet_to_BP)) %>%
  mutate(relationship = ifelse(pearson_correlation > 0 & pearson_correlation < 1, 
                               "Accretion", "Erosion")) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47")))

## Table of results
results.table <- toplot %>%
  select(profile, Park, shoreline_profile) %>%
  unique() %>%
  left_join(equation.details, by = "profile") %>%
  left_join(pearson.correlation, by = "profile") %>%
  select(-relationship)
results.table <- lapply(results.table, function(x) if(is.numeric(x)) round(x, 2) else x) %>%
  as.data.frame()
  


## Download equations for WCEHA comparison
write.csv(results.table, "data_secondary/profiles_with_equations.csv", row.names = FALSE)

## Download csv for clustering
cluster <- euclidean %>%
  select(profile:year, feet_to_BP) %>%
  unique()
write.csv(cluster, "data_secondary/profiles_with_midpoint_distance.csv", 
          row.names = FALSE)
