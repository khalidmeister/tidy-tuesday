# install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-07-21')

# Grouped Radar Chart
# Data For Radar Chart
animal_outcomes <- tuesdata$animal_outcomes
data_for_radar <- animal_outcomes %>%
  select(-Total) %>%
  filter(animal_type == "Cats") %>%
  pivot_longer(4:11, names_to = "state", values_to = "total_per_state") %>%
  group_by(state, animal_type, outcome) %>%
  summarize(total_per_state = sum(total_per_state, na.rm=T)) %>%
  spread(outcome, total_per_state)

data_for_radar <- as.data.frame(data_for_radar)
rownames(data_for_radar) <- data_for_radar$state
data_for_radar <- data_for_radar[, -c(1, 2)]
data_for_radar

# Normalize the data (Has 0-1 range)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data_for_radar['Currently In Care'] <- normalize(data_for_radar['Currently In Care'])
data_for_radar['Euthanized'] <- normalize(data_for_radar['Euthanized'])
data_for_radar['In Stock'] <- normalize(data_for_radar['In Stock'])
data_for_radar['Other'] <- normalize(data_for_radar['Other'])
data_for_radar['Reclaimed'] <- normalize(data_for_radar['Reclaimed'])
data_for_radar['Rehomed'] <- normalize(data_for_radar['Rehomed'])
data_for_radar['Transferred'] <- normalize(data_for_radar['Transferred'])
data_for_radar

data_for_radar <- rbind(rep(max(data_for_radar), 7), rep(0, 7), data_for_radar)
data_for_radar

# Define fill colors
colors_fill <- c(scales::alpha("red", 0.1),
                 scales::alpha("orange", 0.1),
                 scales::alpha("yellow", 0.1),
                 scales::alpha("lightgreen", 0.1),
                 scales::alpha("blue", 0.1),
                 scales::alpha("purple", 0.1),
                 scales::alpha("gray", 0.1),
                 scales::alpha("cyan", 0.1))

# Define line colors
colors_line <-  c(scales::alpha("darkred", 0.9),
                  scales::alpha("darkorange", 0.9),
                  scales::alpha("gold", 0.9),
                  scales::alpha("darkgreen", 0.9),
                  scales::alpha("darkblue", 0.9),
                  scales::alpha("purple", 0.9),
                  scales::alpha("darkgray", 0.9),
                  scales::alpha("cyan", 0.9))

# install.packages("fmsb")
library(fmsb)
radarchart(data_for_radar, 
           seg = 7,  # Number of axis segments
           title = "Cat Output Status from RSPCA at Australia",
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 2)

# Add a legend
legend(x=1.15, 
       y=1.35, 
       legend = rownames(data_for_radar[-c(1,2),]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 1.5)

# Complaints Dataset
library(tidyverse)
library(lubridate)

#Preprocessing
complaints <- tuesdata$animal_complaints
colnames(complaints) <- c("animal_type", "complaint_type", "date_received", "suburb", "electoral_division")
complaints <- complaints %>%
  separate(date_received, c("month", "year"), " ")

months <- data.frame(month=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                     month_number=1:12)
complaints <- complaints %>%
  left_join(months) %>%
  select(-month)
tail(complaints)
complaints$date <- ymd(paste(complaints$year, complaints$month_number, 1, sep="-"))
complaints <- complaints %>%
  select(-year, -month_number)
complaints

complaints_tidy <- complaints %>%
  group_by(date, animal_type) %>%
  count(complaint_type)

# Line Chart (Dog-related complaints only)
complaints_tidy %>%
  filter(animal_type == "dog") %>%
  ggplot(aes(x=date, y=n, color=complaint_type)) +
    geom_line(size=1) +
    facet_wrap(~complaint_type, dir = "h") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "transparent", colour = NA)
    ) +
    labs(
      x = "Year",
      y = "# of complaints",
      color = "Complaint Type"
    ) +
    ggtitle("Dog-Related Complaints Report to RSPCA", subtitle = "From 2014 until 2020")

# Line Chart (Cat & Dog)
complaints %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-12-31")) %>%
  filter(animal_type %in% c("dog", "cat")) %>%
  group_by(date, animal_type) %>%
  count(complaint_type) %>%
  ggplot(aes(x=date, y=n, color=complaint_type)) +
  geom_line(size=1.2) +
  facet_wrap(~animal_type, dir = "h") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "transparent", colour = NA)
  ) +
  labs(
    x = "Year",
    y = "# of complaints",
    color = "Complaint Type"
  ) +
  ggtitle("Complaint Type Report for Each Animal to RSPCA", subtitle = "From 2014 until 2020")

         