##############################################################################
# impact_analysis_exploratory.r
#
# Red Cross Fire Alarm Phase 2
# J. Zlotoff
#
# This script runs exploratory data analysis on the impact data set.
#
# Inputs:
#     impact_data.rdata
#         Census-level data of fires and home visits by year
#         Created by impact_data_prep.r script
#
# Outputs:
#     impact_data_analysis.rdata
#         Census-level data of fires and home visits by year, with additional
#         variables for analysis
#
#     visit_impact_by_year_pop_rural.png
#         Visualization of fire rate chnages by year, population and percent rural
##############################################################################

# load packages
library(tidyverse)

ftab1 <- function(x) {data.frame(prop.table(table(x)))}
ftab2 <- function(x, y) {prop.table(table(x,y),2)}

# load data
load('impact_data.rdata')
names(impact_data)

# determine useful bins for pct_rural, pct_65+, population
rural_hist <- impact_data %>%
    ggplot(aes(x=pct_RURAL_POP_CEN_2010)) +
    geom_histogram(bins=10) +
    ggtitle("Histogram of Pct Rural Population")
print(rural_hist)

impact_data <- impact_data %>%
    mutate(rural_bin = ifelse(pct_RURAL_POP_CEN_2010<=10, "10 or less", "Missing"),
          rural_bin = ifelse(pct_RURAL_POP_CEN_2010>10 & pct_RURAL_POP_CEN_2010<=50, "11-50", rural_bin),
          rural_bin = ifelse(pct_RURAL_POP_CEN_2010>50, "51 or more", rural_bin)
           )

table(impact_data$rural_bin)
ftab1(impact_data$rural_bin)


senior_hist <- impact_data %>%
    ggplot(aes(x=pct_Pop_65plus_CEN_2010)) +
    geom_histogram(binwidth=10) +
    ggtitle("Histogram of Pct Age 65+ Population")
print(senior_hist)

impact_data <- impact_data %>%
    mutate(
        senior_bin = ifelse(pct_Pop_65plus_CEN_2010<=10, "10 or less", "Missing"),
          senior_bin = ifelse(pct_Pop_65plus_CEN_2010>10 & pct_Pop_65plus_CEN_2010<=20, "11-20", senior_bin),
         senior_bin = ifelse(pct_Pop_65plus_CEN_2010>20, "21 or more", senior_bin)
           )

table(impact_data$senior_bin)
ftab1(impact_data$senior_bin)



pop_hist <- impact_data %>%
    ggplot(aes(x=Tot_Population_CEN_2010)) +
    geom_histogram(binwidth=100) +
    ggtitle("Histogram of Total Population")
print(pop_hist)

impact_data <- impact_data %>%
    mutate(pop_bin = ifelse(Tot_Population_CEN_2010<=2500, "2500 or less", "Missing"),
          pop_bin = ifelse(Tot_Population_CEN_2010>2500 & Tot_Population_CEN_2010<=5000, "2500-5000", pop_bin),
          pop_bin = ifelse(Tot_Population_CEN_2010>5000 & Tot_Population_CEN_2010<=7500, "5000-7500", pop_bin),
          pop_bin = ifelse(Tot_Population_CEN_2010>7500, "7500 or more", pop_bin)
           )

table(impact_data$pop_bin)
ftab1(impact_data$pop_bin)

save(impact_data, file = "impact_data_analysis.rdata")

# calculate fire incidence rate by year, then before/after interventions
impact_data <- impact_data %>%
    mutate(visited14 = ifelse(visits_2014>0,'Y','N'),
           visited15 = ifelse(visits_2015>0,'Y','N'),
           visited16 = ifelse(visits_2016>0,'Y','N'))
ftab2(impact_data$visits_2014, impact_data$visited14)

analysis14 <- impact_data %>%
    filter(!is.na(pop_bin),
           !is.na(rural_bin)) %>%
    group_by(rural_bin, pop_bin, visited14) %>%
    mutate(pre_fires = sum(fires_2013) * 1000 / sum(Tot_Population_CEN_2010),
           post_fires = sum(fires_2014) * 1000 / sum(Tot_Population_CEN_2010),
           net_fires = post_fires - pre_fires,
           total_visits = sum(visits_2014) * 1000 / sum(Tot_Population_CEN_2010),
           pop_sum = sum(Tot_Population_CEN_2010)) %>%
    select(pop_bin, rural_bin, visited14, pre_fires, post_fires, net_fires, total_visits, pop_sum) %>%
    distinct() %>%
    mutate(year = 2014) %>%
    rename(visited = visited14) %>%
    arrange(pop_bin, rural_bin, visited)

chart14 <- analysis14 %>%
    ungroup() %>%
    mutate(rural_bin = paste(rural_bin, "% rural")) %>%
    ggplot(aes(x=pop_bin, y=net_fires, color=visited)) +
    geom_point() +
    facet_wrap(~rural_bin) +
    labs(title = "Net Change in Fires, 2013-2014",
         x="Tract Population", y="Net Change in Fires Per 1000 People", color="Visited by ARC") +
    theme(axis.text.x = element_text(angle = 45))
print(chart14)

analysis15 <- impact_data %>%
    filter(!is.na(pop_bin),
           !is.na(rural_bin)) %>%
    group_by(rural_bin, pop_bin, visited15) %>%
    mutate(pre_fires = sum(fires_2014) * 1000 / sum(Tot_Population_CEN_2010),
           post_fires = sum(fires_2015) * 1000 / sum(Tot_Population_CEN_2010),
           net_fires = post_fires - pre_fires,
           total_visits = sum(visits_2015) * 1000 / sum(Tot_Population_CEN_2010),
           pop_sum = sum(Tot_Population_CEN_2010)) %>%
    select(pop_bin, rural_bin, visited15, pre_fires, post_fires, net_fires, total_visits, pop_sum) %>%
    distinct() %>%
    mutate(year = 2015) %>%
    rename(visited = visited15) %>%
    arrange(pop_bin, rural_bin, visited)

analysis16 <- impact_data %>%
    filter(!is.na(pop_bin),
           !is.na(rural_bin)) %>%
    group_by(rural_bin, pop_bin, visited16) %>%
    mutate(pre_fires = sum(fires_2015) * 1000 / sum(Tot_Population_CEN_2010),
           post_fires = sum(fires_2016) * 1000 / sum(Tot_Population_CEN_2010),
           net_fires = post_fires - pre_fires,
           total_visits = sum(visits_2016) * 1000 / sum(Tot_Population_CEN_2010),
           pop_sum = sum(Tot_Population_CEN_2010)) %>%
    select(pop_bin, rural_bin, visited16, pre_fires, post_fires, net_fires, total_visits, pop_sum) %>%
    distinct() %>%
    mutate(year = 2016) %>%
    rename(visited = visited16) %>%
    arrange(pop_bin, rural_bin, visited)

analysis <- analysis14 %>%
    bind_rows(analysis15) %>%
    bind_rows(analysis16) %>%
    ungroup()

rm(analysis14, analysis15, analysis16)

eda1 <- analysis %>%
    mutate(rural_bin = paste(rural_bin, "% rural")) %>%
    ggplot(aes(x=pop_bin, y=net_fires, color=visited)) +
    geom_point() +
    facet_wrap(~year + rural_bin) +
    labs(title = "Net Change in Fires",
         x="Tract Population", y="Net Change in Fires Per 1000 People", color="Visited by ARC") +
    theme(axis.text.x = element_text(angle = 45))
ggsave(filename = "visit_impact_by_year_pop_rural.png", width = 8)
print(eda1)

save(impact_data, analysis, eda1, file = "impact_data_analysis.rdata")
