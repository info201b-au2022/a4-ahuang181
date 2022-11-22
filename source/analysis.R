library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# Load Dataset
get_data <- function(num_records=-1) {
  fname <- "~/Documents/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}

incarceration_df <- get_data()

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# Total jail population over a span of 10 years in the newest data included (2008-2018)
total_jail_pop_1970 <- incarceration_df %>%
  filter(year == "1970") %>%
  select(year, total_jail_pop) %>%
  summarize(total_num_jail = round(sum(total_jail_pop, na.rm = TRUE))) %>%
  pull(total_num_jail)

total_jail_pop_2008 <- incarceration_df %>%
  filter(year == "2008") %>%
  select(year, total_jail_pop) %>%
  summarize(total_num_jail = round(sum(total_jail_pop, na.rm = TRUE))) %>%
  pull(total_num_jail)

total_jail_pop_2018 <- incarceration_df %>%
  filter(year == "2018") %>%
  select(year, total_jail_pop) %>%
  summarize(total_num_jail = round(sum(total_jail_pop, na.rm = TRUE))) %>%
  pull(total_num_jail)

change_1970_2008 <- total_jail_pop_2008 - total_jail_pop_1970

change_2008_2018 <- total_jail_pop_2018 - total_jail_pop_2008

# Race with highest population in jail 1970-2018

# Race in 1970
highest_race_jail_pop <- incarceration_df %>%
  filter(year == 1970)

# Race in 2018
  
  






## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  data <- incarceration_df %>%
      select(year, total_jail_pop)
  return(data)   
}

# get_year_jail_pop()

# test_data <- incarceration_df %>% select(year, total_jail_pop)

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  labels <- labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Increase of Jail Population in U.S. (1970-2018)",
   # subtitle = "Data from test dataset (not real).",
    caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018).
              This chart shows that there has been a steep increase in the
              overall number of people in jail in the U.S. over time, especially
              from the years of 1980 to 2005.",
   alt = "Increase of Jail Population in U.S. (1970-2018)"
  )
  
  chart <- ggplot(data) +
    geom_col(
      mapping = aes(x = year, y = total_jail_pop),
    #  color = "blue",              # color of line 
    #  fill = "red",                # color of bars
    #  linetype = "dotted",         # solid | dashed | dotted
    # alpha = 0.5                   # Transparency 
    ) +
    labels +
    scale_y_continuous(labels = scales::comma)
  return(chart)
}

figure_1 <- plot_jail_pop_for_us()
figure_1

# SUMMARY
# Question: How have the overall number of jail populations across the country
# of the United States changed over time?
# The bar chart depicting the U.S. jail population over the years is significant
# as it points to a trend in which there has been great increases in jailings
# across the country. The data is important as people can visually see that since
# 1970, the number of jailings have nearly quadrupled, which is quite alarming,
# and could point to other factors that are causing this. Additionally, based on
# the graph, the steepest increases occurred between the years 1980 and 2005.


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
  data <- incarceration_df %>%
    select(year, state, total_jail_pop) %>%
    filter(state == states)
  return(data)
}

# data <- get_jail_pop_by_states("CA")

plot_jail_pop_by_states <- function(states) {
  data <- get_jail_pop_by_states(states)
  labels <- labs(
    x = "Year",
    y = "Jail Population",
    color = "State",
    title = "Jail Population Over Time By State (1970-2018)",
    caption = "Figure 2. Jail Population Over Time By State (1970-2018).
              This chart shows different states having varying jail
              populations over time, with the states of CA and NY
              having the most jailings in the majority of the years.",
    alt = "Jail Population Over Time By State (1970-2018)"
  )
  
  chart <- ggplot(data) +
    geom_line(
      mapping = aes(x = year, y = total_jail_pop, color = state)
    ) +
    labels
  return(chart)
}

figure_2 <- plot_jail_pop_by_states(c("CA", "WA", "AL", "NY", "NV", "FL"))
figure_2

# SUMMARY
# Question: How does the jail population in different states vary and
# change over time?
# The line graph displays the number of people in jails by state over the time
# period between 1970 and 2018. From the chart, it can be clearly seen that
# the states of California and New York have the highest jail populations
# for a good amount of the years that are included in the data, which could
# be interesting, as it potentially points to the systems that are put into place
# in those specific states and locations that could be further looked into.
# In addition, each state seems to have spikes and jumps in the number of jailings
# every couple years. The states of AL, CA, FL, NV, NY, and WA were chosen
# as many of these states are spread out in different sections of the United States.
# Additionally, there are also a couple states chosen that are next to each other
# geographically on the US map, for those that want to see how states that
# are so close in vicinity could also have very different jail populations.


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# Compare inequalities over time of the different race populations that are in
# jail in comparison to the total total_jail_pop.

# Example of stacked bar chart: https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html

# Comparing Races Dataset
year <- length(incarceration_df$year)
print(year)
# total <- length(incarceration_df$total_jail_pop)
# print(total)
aapi <- length(incarceration_df$aapi_jail_pop)
print(aapi) # = 153811
black <- length(incarceration_df$black_jail_pop)
print(black) # = 153811
latinx <- length(incarceration_df$latinx_jail_pop)
print(latinx) # = 153811
native <- length(incarceration_df$native_jail_pop)
print(native) # = 153811
white <- length(incarceration_df$white_jail_pop)
print(white) # = 153811
other <- length(incarceration_df$other_race_jail_pop)
print(other) # = 153811
# all have 153811 values so the data will match in the dataframe

# Creating the data frame for race jail data
year <- c(rep(incarceration_df$year, 6))
race <- c(rep("AAPI", 153811), rep("Black", 153811), rep("Latinx", 153811),
          rep("Native American", 153811), rep("White", 153811), rep("Other", 153811))
value <- c(incarceration_df$aapi_jail_pop, incarceration_df$black_jail_pop,
           incarceration_df$latinx_jail_pop, incarceration_df$native_jail_pop,
           incarceration_df$white_jail_pop, incarceration_df$other_race_jail_pop)
race_data <- data.frame(year, race, value)

# Creating a stacked bar chart with no percentages
get_race_jail_pop <- function() {
  data <- race_data
  return(data)   
}

# get_race_jail_pop()

plot_race_jail_pop <- function()  {
  data <- get_race_jail_pop()
  labels <- labs(
    x = "Year",
    y = "Jail Population",
    title = "Jail Population in the U.S. Over Time by Race (1970-2018)",
    caption = "Figure 3. Jail Population in the U.S. Over Time by Race (1970-2018).
              This chart shows that as the overall jail population in the U.S.
              increased over time, the people of the same race still made up
              the most portions of jailings, which are the Black people and
              White people.",
    alt = "Jail Population in the U.S. Over Time by Race (1970-2018)",
    fill = "Race",
  )
  
  chart <- ggplot(race_data, aes(fill = race, y = value, x = year)) + 
    geom_bar(position="stack", stat="identity") +
    labels +
    scale_y_continuous(labels = scales::comma)
  return(chart)
}

figure_3 <- plot_race_jail_pop()
figure_3

# SUMMARY
# Question: How does the jail population of people belonging to different races
# compare, and what are the trends of those racial populations in jail over time?
# The graph is significant because it shows groups of people of different
# racial backgrounds and identities that are disproportionately affected by
# the jail system, regardless of what the reason
# may be. It also shows how those trends continue over time, from the 1900s to
# the present day, therefore signifying that jailings according to race
# may still be a great issue for politicians and researchers alike to look into.
# It is interesting to see that over the years, regardless of what the total
# jail population is, Black people and White people, always seem to have
# the most people in jail. It is also notable that from the years of 1970
# all the way up to nearly 1985, there is no data on the
# bar chart, as data could not have been compared since the data for some of the
# races were not available. This is also significant as the data being unavailable could possibly
# point to the idea that data in regards to race had not been collected
# at the time, which also points to how times may have potentially
# changed and people started emphasizing the importance in the
# disparities between race, which in this case, is the jailing system in the U.S. over time.


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 

# Map

# Some directions from Programming Skills for Data Science textbook
# pages 248-251

# Load shapefile of U.S. states from ggplot
state_shape <- map_data("state")

# Create blank map of U.S. states
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white",
    size = .1
  ) +
  coord_map()

# Load data
jail_pop <- incarceration_df %>%
  filter(year == 2018) %>%
  select(state, total_jail_pop) %>%
  rename("Code" = state)

# Change state Code names from jail_pop to state full names
states_fullname <- read.csv("~/Documents/info201/assignments/a4-ahuang181/source/state_names_and_codes.csv")
states_fullname <- states_fullname %>%
  select(Code, State)

jail_pop <- jail_pop %>%
  left_join(states_fullname) %>%
  rename("states" = State) %>%
  mutate(state = tolower(states))

# Join data with U.S. shapefile
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(jail_pop, by = "state")

# Data function
get_2018_jail_pop <- function() {
  data <- state_shape
  return(data)
}

# get_2018_jail_pop()

# Drawing and plotting the map
plot_2018_jail_pop <- function() { 
  data <- get_2018_jail_pop()
  ggplot(data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "white",
    size = .1
  ) +
  coord_map() +
  theme( # Clean up the data by cleaning up the theme
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    ) +
    labs(
      title = "Jail Population Across the U.S. in 2018",
      caption = "Figure 4. Jail Population Across the U.S. in 2018.
              This chart shows that there are varying jail populations
              jail populations across the U.S.",
      alt = "Jail Population Across the U.S. in 2018"
    ) +
    scale_fill_continuous(name="Jail Population",
                          low = "#132B43", high = "Red",
                          limits = c(0, 4200),
                          breaks=c(0, 1000, 2000, 3000, 4000),
                          na.value = "grey50")
}

# found changing scale fill color at: https://remiller1450.github.io/s230s19/Intro_maps.html

# tested different scales to see what colors, ranges, to stop at for the fill
# so that all available data values could be included in the fill color on the map

figure_4 <- plot_2018_jail_pop()
figure_4

# SUMMARY
# Question: How do jail populations across the U.S. in different states vary in
# the year of 2018?
# The graph is significant because it shows how different states across the U.S.
# have different amounts of jail populations. It can be seen that many of the
# states in the U.S. had similar numbers in jail populations due to the color
# fill of navy that they consist of. However, areas that are more of a darker
# red and magenta color had greater jail populations, such as the states of
# New Mexico, Colorado, and Idaho. Furthermore, California has a brighter red,
# and is also the color highest on the scale for this map, signifying that
# they had the most jail population in the year of 2018. In addition, the year
# of 2018 was chosen as it is the most recent to the present day in regards to
# this dataset. With such existing variances, it would be interesting for
# people to dive into the reasonings of why such inequalities occur, and if
# they point to differing policies in the different states.

