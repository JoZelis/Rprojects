# install from github with library(devtools) and than command devtools::install_github("joachim-gassen/tidycovid19")
library(tidycovid19) #covid data importer van joachim-gassen.githubio
library(plyr)
library(ggrepel) # for repelling overlapping stuff in plots
library(gghighlight)
library(tidyverse) # contains ggplot2, dplyr, readr among others
library(lubridate) # for data management
library(zoo) # for rolling average
library(rvest) # for web scraping
library(gsubfn) #for modifying string data

merged_dta <- download_merged_data(cached = TRUE)
merged_dta %>% filter(country == "Netherlands") -> nl # data with filter Netherlands

# text under figure
lab_notes <- paste0(
  "Data as provided by Johns Hopkins University Center for Systems Science ", 
  "and Engineering (JHU CSSE)\nand obtained on ", Sys.Date(),
  " through tidycovid19 library from https://joachim-gassen.github.io.The sample\n", 
  "is limited to countries with at least seven days of positive event days data.\n" 
)

#View(merged_dta)
#sum(is.na(marged_dta)) gives the total missing values
# further visualization of missing data at the end of the script
#
# make a tibble database from the original one as_tibble(merged_dta) to work easier
#summary(merged_dta) summary of the data

# add numbers with web scraping
url <- "https://www.rivm.nl/coronavirus-covid-19/actueel" #define RIVM site
RIVM <- read_html(url)

conf <- html_nodes(RIVM, #get confirmed data
                   '#top > article > div > div:nth-child(2) > div > div > div > div.par.content-block-wrapper.bg-brand-lightest > div > table > tbody > tr:nth-child(1) > td:nth-child(2) > span > span') %>%
  html_text() %>% #read the HTML text of confirmed
  str_replace_all("[[:punct:]]", "") %>% #remove the punctuations from the number
  as.numeric()#convert the string to number

death <- html_nodes(RIVM, #get death data
                    '#top > article > div > div:nth-child(2) > div > div > div > div.par.content-block-wrapper.bg-brand-lightest > div > table > tbody > tr:nth-child(3) > td:nth-child(2) > span > span') %>%
  html_text() %>% #read the HTML text of confirmed
  str_replace_all("[[:punct:]]", "") %>% #remove the punctuations from the number
  as.numeric()#convert the string to number

dateRIVM <- html_nodes(RIVM,
                       "#top > article > div > div:nth-child(2) > div > div > div > div.content-dates > span") %>% 
  html_text() %>%
  strapplyc("[0-9-]{10,}", simplify=TRUE) %>% #extract 10 characters (in this case date) with "-" in the string
  strptime("%d-%m-%Y") #convert to date format

#add new row to the Netherlands for previous day
if (isTRUE(dateRIVM == Sys.Date()-1)){
  paste("Date for", as.Date(Sys.Date()-1), "is the same as RIVM update!")
} else if (isTRUE(any(nl==as.character(Sys.Date()-1)))){
  paste("Data for", as.Date(Sys.Date()-1), "is already in the dataframe!")
} else {
  new_row_LagDay <-
    data.frame(
      country = "Netherlands",
      iso3c = "NLD",
      date = as.Date(Sys.Date())-1, #updated for 21-04-2020
      confirmed = 34134,
      deaths = 3916,
      recovered = NA,
      soc_dist = NA,
      mov_rest = NA,
      pub_health = NA,
      gov_soc_econ = NA,
      lockdown = NA,
      apple_mtr_driving = NA,
      apple_mtr_walking = NA,
      apple_mtr_transit = NA,
      gcmr_retail_recreation = NA,
      gcmr_grocery_pharmacy = NA,
      gcmr_parks = NA,
      gcmr_transit_stations = NA,
      gcmr_workplaces = NA,
      gcmr_residential = NA,
      gtrends_score = NA,
      gtrends_country_score = NA,
      region = "Europe & Central Asia",
      income = "High income",
      population = 17231017,
      land_area_skm = 33690,
      pop_density = 511.4579,
      pop_largest_city = 1131690,
      life_expectancy = 81.76098,
      gdp_capita = 	55022.92,
      timestamp = format(Sys.Date(), tz="") #remove timezone
    )
  # combine row with whole dataframe
  merged_dta <-
    rbind(merged_dta, new_row_LagDay)
}

# add data RIVM to dataframe
if (isTRUE(any(nl==as.character(dateRIVM)))){
  paste("Data for", as.Date(dateRIVM), "is already in the dataframe!")
} else {
  new_row_Web <-
    data.frame(
      country = "Netherlands",
      iso3c = "NLD",
      date = as.Date(dateRIVM),
      confirmed = conf,
      deaths = death,
      recovered = NA,
      soc_dist = NA,
      mov_rest = NA,
      pub_health = NA,
      gov_soc_econ = NA,
      lockdown = NA,
      apple_mtr_driving = NA,
      apple_mtr_walking = NA,
      apple_mtr_transit = NA,
      gcmr_retail_recreation = NA,
      gcmr_grocery_pharmacy = NA,
      gcmr_parks = NA,
      gcmr_transit_stations = NA,
      gcmr_workplaces = NA,
      gcmr_residential = NA,
      gtrends_score = NA,
      gtrends_country_score = NA,
      region = "Europe & Central Asia",
      income = "High income",
      population = 17231017,
      land_area_skm = 33690,
      pop_density = 511.4579,
      pop_largest_city = 1131690,
      life_expectancy = 81.76098,
      gdp_capita = 	55022.92,
      timestamp = format(Sys.Date(), tz="") #remove timezone
    )
  merged_dta <-
    rbind(merged_dta, new_row_Web)# combine row with whole dataframe
}

# daily average and rolling average calculations
merged_dta = merged_dta %>% arrange(country, date) #arrange by country and date
merged_dta <- ddply(merged_dta, .(country), mutate, dailyDeaths = deaths - lag(deaths)) # country here is used to group the data
merged_dta <- ddply(merged_dta, .(country), mutate, dailyConfirmed = confirmed - lag(confirmed))
merged_dta <- ddply(merged_dta, .(country), mutate, avDailyDeaths05 = rollmean(dailyDeaths, k = 5, fill = NA))
merged_dta <- ddply(merged_dta, .(country), mutate, avDailyConfirmed05 = rollmean(dailyConfirmed, k = 5, fill = NA))
merged_dta <- ddply(merged_dta, .(country), mutate, avDailyDeaths07 = rollmean(dailyDeaths, k = 7, fill = NA))
merged_dta <- ddply(merged_dta, .(country), mutate, avDailyConfirmed07 = rollmean(dailyConfirmed, k = 7, fill = NA))
# growth rate calculations
merged_dta <- ddply(merged_dta, .(country), mutate, Rate_percentC = dailyConfirmed/confirmed * 100)# growth rate in percent for confirmed cases
merged_dta <- ddply(merged_dta, .(country), mutate, Rate_percentD = dailyDeaths/deaths * 100)# growth rate in percent for deaths
# deaths and confirmed per country population
merged_dta <- ddply(merged_dta, .(country), mutate, deaths_1e5pop = 1e5*deaths/population)
merged_dta <- ddply(merged_dta, .(country), mutate, confirmed_1e5pop = 1e5*confirmed/population)

# filter countries by more than 10 deaths
death_dta = merged_dta
death_dta %>%
  group_by(country) %>%
  filter(deaths >= 10) %>%
  summarise(edate_deaths = min(date)) -> edates_deaths

# filter countries by more than 7 days of data
death_dta %>%
  left_join(edates_deaths, by = "country") %>%
  mutate(
    edate_deaths = as.numeric(date - edate_deaths)
  ) %>%
  filter(edate_deaths >= 0) %>%
  group_by(country) %>%
  filter (n() >= 7) %>%
  ungroup() -> death_dta

#filter countries by more than 100 confirmed
confirmed_dta = merged_dta
confirmed_dta %>% 
  group_by(country) %>%
  filter(confirmed >= 100) %>%
  summarise(edate_confirmed = min(date)) -> edates_confirmed

# filter countries by more than 7 days of data
confirmed_dta %>% 
  left_join(edates_confirmed, by = "country") %>%
  mutate(
    edate_confirmed = as.numeric(date - edate_confirmed)
  ) %>%
  filter(edate_confirmed >= 0) %>%
  group_by(country) %>%
  filter (n() >= 7) %>% 
  ungroup() -> confirmed_dta

# plot for rolling average over 7 days edate_deaths and death_dta is used for date so after 10th death, full data x = date
death_dta %>% filter(country == "Netherlands") -> cf #filter Netherlands data
#to plot with legend go from wide to long
long_cf <- cf %>% gather(State, Total, avDailyDeaths07:avDailyConfirmed07)
ggplot(long_cf, aes(x = edate_deaths, y = Total, group = State, colour = State)) + 
  geom_line() +
  scale_colour_discrete(name = "Cases",
                        labels=c("Confirmed","Deaths")
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),) +
  labs(caption = lab_notes, 
       x = "Number of days since 10th death", 
       y = "Change in cases",
       title = "Total change in deaths and confirmed cases for the Netherlands (7-day moving average)"
  )

# make it interactive with library(plotly) ggplotly(the_ggplot)


# Growth rate or %daily change of deaths per selected county

death_dta %>% filter(country == "Netherlands" | 
                       country== "US" | 
                       country == "Italy") -> grd # "growth rate death" copy of data for filter countries

ggplot(grd %>%  filter (edate_deaths <= 70), # edate deaths is how many days you want to display from 10th death
       aes(x = edate_deaths, color = country, y = Rate_percentD)) +
  geom_line() + theme_minimal() + 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),) + 
  gghighlight(TRUE, label_params = list(segment.color = NA, nudge_x = 1)) + # label_params is part of ggrepel to make sure names not overlap
  labs(caption = lab_notes,
       x = "Number of days since 10th death",
       y = "%change in death",
       title = "%Daily change in deaths per country\n"
  ) + geom_smooth(method = "lm")

# animate the above plot without trend lines
# library(gganimate)
# p <- ggplot(grd %>%  filter (edate_deaths <= 70), # edate deaths is how many days you want to display
#             aes(x = edate_deaths, color = country, y = Rate_percentD)) +
#   geom_line() + theme_minimal() +
#   theme(
#     plot.title.position = "plot",
#     plot.caption.position =  "plot",
#     plot.caption = element_text(hjust = 0),
#     axis.title.x = element_text(hjust = 1),
#     axis.title.y = element_text(hjust = 1),) +
#   gghighlight(TRUE, label_params = list(segment.color = NA, nudge_x = 1)) + # label_params is part of ggrepel to make sure names not overlap
#   labs(caption = lab_notes,
#        x = "Number of days since 10th death",
#        y = "%change in death",
#        title = "%Daily change in deaths per country\n"
#   )
# p + geom_point() + transition_reveal(edate_deaths) # animats the plot and adds a point
# anim_save("gif.gif") #saves it as a gif

# Growth rate or %daily change of confirmed cases per selected county
confirmed_dta %>% filter(country == "Netherlands" | 
                           country== "US" | 
                           country == "Italy"
) -> grc # copy of data for filter countries

ggplot(grc %>%  filter (edate_confirmed <= 70),
       aes(x = edate_confirmed, color = country, y = Rate_percentC)) +
  geom_line() + theme_minimal() + 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),) + 
  gghighlight(TRUE, label_params = list(segment.color = NA, nudge_x = 1)) +
  labs(caption = lab_notes,
       x = "Number of days since 100th confirmed case",
       y = "%change in confirmed cases",
       title = "%Daily change in confirmed cases per country\n"
  )+ geom_smooth(method = "lm")

# plots for deaths per country
# define event time after 10th death
# Also a require each country to have at least 7 days post event day 0

death_dta %>% filter(
  country == "Netherlands" |
    country == "China" |
    country == "Germany" |
    country == "Italy" |
    country == "Spain" |
    country == "Belgium" |
    country == "Korea, South" |
    country == "Japan" | 
    country == "US"
) -> df

# text directly under figure left
lab_x_axis_deaths <- sprintf(paste(
  "Days since 10th death\n"))

# set log scale and highlights
gg_my_blob <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes,
       x = lab_x_axis_deaths,
       y = "Confirmed deaths (logarithmic scale)"),
  gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)

# plot for total deaths
ggplot(df %>% filter (edate_deaths <= 70), 
       aes(x = edate_deaths, color = country, y = deaths)) +
  geom_line() + labs(title = "Total Deaths per Country\n"
  ) + gg_my_blob

# plot for deaths relative to population
ggplot(df %>% filter (edate_deaths <= 70), 
       aes(x = edate_deaths, color = country, y = deaths_1e5pop)) +
  geom_line() +
  gg_my_blob +
  labs(
    y = "Confirmed deaths per 100,000 inhabitants (logarithmic scale)",
    title = "Deaths relative to population\n"
  )

# code for CONFIRMED
confirmed_dta %>% filter(
  country == "Netherlands" |
    country == "China" |
    country == "Germany" |
    country == "Italy" |
    country == "Spain" |
    country == "Belgium" |
    country == "Korea, South" |
    country == "Japan" | 
    country == "US"
) -> dfc

lab_x_axis_confirmed <- sprintf(paste(
  "Days since confirmed cases matched or exceeded 100\n"
))

# set log scale and highlights
gg_my_blob <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes,
       x = lab_x_axis_confirmed,
       y = "Confirmed cases (logarithmic scale)"),
  gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)

# plot for confirmed cases per county
ggplot(dfc %>% filter (edate_confirmed <= 70), 
       aes(x = edate_confirmed, color = country, y = confirmed)) +
  geom_line() +
  labs(
    title = "Total Confirmed Cases by Country\n"
  ) +
  gg_my_blob

# plot for confirmed relative to population
ggplot(dfc %>% filter (edate_confirmed <= 70), 
       aes(x = edate_confirmed, color = country, y = confirmed_1e5pop)) +
  geom_line() +
  gg_my_blob +
  labs(y = "Confirmed cases per 100,000 inhabitants (logarithmic scale)",
       title = "Confirmed cases relative to population\n"
  ) 

# example from Joachim Gassen
#
#
# merged_dta %>%
#  group_by(country) %>%
#  mutate(
#    reported_deaths = max(deaths),
#    soc_dist_measures = max(soc_dist)
#  ) %>%
#  select(country, iso3c, reported_deaths, soc_dist_measures) %>%
#  distinct() %>%
#  ungroup() %>%
#  arrange(-reported_deaths) %>%
#  head(20) -> df
#
# View(merged_dta)
#
# ggplot(df, aes(x = reported_deaths, y = soc_dist_measures)) +
#  geom_point() +
#  geom_label_repel(aes(label = iso3c)) +
#  theme_minimal() +
#  scale_x_continuous(trans='log10', labels = scales::comma) +
#  labs(x = "Reported deaths (logarithmic scale)",
#       y = "Number of governmental social distancing measures",
#       annotation = "Data from JHU CSSE and ACAPS.")
#
# fast plot with: plot_covid19_spread(merged_dta, highlight = c("ITA", "ESP", "FRA", "DEU", "USA", "NLD", "BEL", "KOR", "CHN", "JPN"), intervention = "lockdown") + geom_line(size=1.5)
# SHINY APPLICATION: shiny_covid19_spread()

# Code generated by shiny_covid19_spread() of the {tidycovid19} package
# See: https://github.com/joachim-gassen/tidycovid19
# Run in R/Rstudio. See https://www.r-project.org and https://www.rstudio.com
# Uncomment the following to install the {tidycovid19} package

# remotes::install_github("joachim-gassen/tidycovid19)

# #missing values plots
#
# missing.values <- merged_dta %>%
#   +     gather(key = "key", value = "val") %>%
#   +     mutate(is.missing = is.na(val)) %>%
#   +     group_by(key, is.missing) %>%
#   +     summarise(num.missing = n()) %>%
#   +     filter(is.missing==T) %>%
#   +     select(-is.missing) %>%
#   +     arrange(desc(num.missing))
# 
# #plots for percentage of missing values
# missing.values <- merged_dta %>%
#   gather(key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   group_by(key) %>%
#   mutate(total = n()) %>%
#   group_by(key, total, isna) %>%
#   summarise(num.isna = n()) %>%
#   mutate(pct = num.isna / total * 100)
# levels <- (missing.values  %>% filter(isna == T) %>%     
#              arrange(desc(pct)))$key
# percentage.plot <- missing.values %>%
#   ggplot() +
#   geom_bar(aes(x = reorder(key, desc(pct)), 
#                y = pct, fill=isna), 
#            stat = 'identity', alpha=0.8) +
#   scale_x_discrete(limits = levels) +
#   scale_fill_manual(name = "", 
#                     values = c('steelblue', 'tomato3'), 
#                     labels = c("Present", "Missing")) +
#   coord_flip() +
#   labs(title = "Percentage of missing values", 
#        x = 'Variable', y = "% of missing values")
# percentage.plot
# 
# #plot the missing values per row
# row.plot <- df %>%
#   mutate(id = row_number()) %>%
#   gather(-id, key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   ggplot(aes(key, id, fill = isna)) +
#   geom_raster(alpha=0.8) +
#   scale_fill_manual(name = "",
#                     values = c('steelblue', 'tomato3'),
#                     labels = c("Present", "Missing")) +
#   scale_x_discrete(limits = levels) +
#   labs(x = "Variable",
#        y = "Row Number", title = "Missing values in rows") +
#   coord_flip()
# row.plot
# 
# #combine the two plots
# library(gridExtra)
# grid.arrange(percentage.plot, row.plot, ncol = 2)


