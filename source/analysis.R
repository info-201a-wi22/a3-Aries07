# Load Data #
incarceration <- read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
                        header = TRUE, stringsAsFactors = FALSE)
library("tidyverse")
library("leaflet")
# Filter Data #
inc_08_18 <- incarceration %>% 
  filter( year > 2007 & year < 2019)
inc_08_18$year <- as.character(inc_08_18$year)


# Summary Information#
# 1. Number of observation and feature#
num_obser <- nrow(inc_08_18)
num_feature <- ncol(inc_08_18)

# 2. Average value of total jail population across all counties in latest year #
ave_total_jail_latest <- inc_08_18 %>% 
  filter( year == max(year)) %>% 
  summarise(ave_total_jail_latest = mean(total_jail_pop, na.rm = TRUE)) %>% 
  pull(ave_total_jail_latest) %>% 
  round(0)

# 3. When the female jail population has highest value 
# through 2008 to 2018 #
highest_female_year <- inc_08_18 %>% 
  filter(female_jail_pop == max(female_jail_pop,
         na.rm = TRUE)) %>% 
  pull(year)

# 4. Which county has highest average male jail population through 2008 to 2018 #
highest_male_county <- inc_08_18 %>% 
  group_by(county_name) %>% 
  summarize(ave_male_jail_pop = mean(male_jail_pop)) %>% 
  filter(ave_male_jail_pop == max(ave_male_jail_pop, 
         na.rm = TRUE)) %>% 
  pull(county_name)

# 5. What is the highest total jail population change from 2008 to 2018 #
total_jail_diff_08to18 <- inc_08_18 %>%
  group_by(county_name) %>% 
  summarize(diff_08to18 = max(total_jail_pop) - min(total_jail_pop)) %>% 
  filter(diff_08to18 == max(diff_08to18, na.rm = TRUE)) %>% 
  pull(diff_08to18)

# Chart1 data frame #
long_dt <- inc_08_18 %>% 
  select(year, state, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>% 
  gather(key = race, value = pop, - year, -state) %>% 
  filter( pop != "NA") 

# Chart2 data frame #
total_pop_jail_pop <- inc_08_18 %>%
  select(year, state, total_pop, total_jail_pop) %>% 
  filter(total_pop != "NA" & total_jail_pop != "NA")

# Chart3 data frame #
top_10_state <- inc_08_18 %>% 
  group_by(state) %>% 
  summarize(all_jail_adm = sum(total_jail_adm, na.rm = TRUE)) %>% 
  arrange(-all_jail_adm ) %>% 
  top_n(10) %>% 
  pull(state)

county_lat_lng <- read.csv(file = "C:/Users/Y/Desktop/code/a3-Aries07/source/uscounties.csv",
                           header = TRUE, stringsAsFactors = FALSE)

wa_county <- county_lat_lng %>% 
  select(county_fips, lat, lng)
total_jail_adm <- inc_08_18 %>% 
  filter(total_jail_adm != "NA") %>% 
  select(state, county_name, fips, total_jail_adm)
total_jail_adm <- left_join(total_jail_adm, wa_county, 
                            by = c("fips" = "county_fips")) %>% 
  filter(state == top_10_state)


