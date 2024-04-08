install.packages("tidyverse")
install.packages("maps")
install.packages("plotly")
install.packages("dplyr")


library(tidyverse)
library(maps)
library(plotly)
library (dplyr)


unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

data_join <- full_join(unicef_metadata, unicef_indicator_2, by = c("country" = "country"))

data_join <- unicef_metadata %>% 
  full_join(unicef_indicator_2, by = c("country" = "country", "year" = "time_period"))

map_world <- map_data("world") 

map_data_join <- full_join(unicef_indicator_2, map_world, by = c("country" = "region"))

ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low ="orange" , high = "red" ) +
  labs (fill = "%") +
  labs(title = "Average % of Careseeking for Febrile Children 
  (under age 5) across Observed Countries: (2004-2022)") 




#Time series 3

library(ggplot2)
library(plotly)

# Sample data (replace with your actual dataset)
set.seed(123)
years <- 2006:2021
males <- c(0.646, 0.728, 0.6145, 0.6145, 0.5351, 0.5527, 0.5735, 0.6172, 0.5593, 0.6308, 0.6041, 0.6304, 0.6341, 0.6528, 0.6556, 0.7940)
females <- c(0.6185, 0.6448, 0.5520, 0.5981, 0.5228, 0.5444, 0.5624, 0.6007, 0.5708, 0.6265, 0.5997, 0.6291, 0.6216, 0.6640, 0.6480, 0.802)

data <- data.frame(year = years, males = males, females = females)

# Reshape data to long format
data_long <- tidyr::pivot_longer(data, cols = c(males, females), names_to = "sex", values_to = "care_seek_percentage")

# Plotly plot
p <- ggplot(data_long, aes(x = year, y = care_seek_percentage * 100, color = sex, group = sex)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Care-seeking Percentage (%)", color = "Sex", title = "Gender Disparities in Care-seeking Percentages by Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to plotly
ggplotly(p)

# Scatter plot

vis_3 <- data_join %>%
  group_by(country) %>%
  summarise(
    Avg_Life_Expectancy = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE),
    Obs_value_avg = mean(obs_value, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Avg_Life_Expectancy, y = Obs_value_avg, color = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.6, color = "black") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, NA)) +  
  labs(
    y = "Average Life Expectancy (years)",
    x = "Average Observed Value",
    title = "Average Life Expectancy by Country"
  )

ggplotly(vis_3)



#Bar Chart

library(ggplot2)
library(plotly)

# Input your own percentages for the five countries
countries <- c("Sri Lanka", "Tuvalu", "Nauru", "Albania", "Yemen")
male_percentage <- c(0.8955, 0.8765, 0.697, 0.824, 0.618)
female_percentage <- c(0.376, 0.7085, 0.595, 0.3680, 0.2940)

# Create a data frame
data <- data.frame(
  country = rep(countries, each = 2),  # Repeat each country twice
  sex = rep(c("Male", "Female"), times = length(countries)),  # Specify sex labels for each country
  percentage = c(male_percentage, female_percentage)
)

# Convert to plotly
p <- ggplot(data, aes(x = country, y = percentage, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Care-Seeking Percentage", fill = "Gender", title = "Gender Disparities in Careseeking for Febrile Children 2006-2022") +
  theme_minimal()

ggplotly(p)








vis_3 <- data_join %>%
  group_by(country) %>%
  summarise(
    Avg_Life_Expectancy = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE),
    Obs_value_avg = mean(obs_value, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Avg_Life_Expectancy, y = Obs_value_avg, color = country)) +
  geom_point(color = c("orange")) +  # Set colors for points
  geom_smooth(method = "lm", se = FALSE, size = 0.6, color = "black") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, NA)) +  
  labs(
    y = "Average Life Expectancy (years)",
    x = "Average Observed Value",
    title = "Average Life Expectancy by Country"
  )

ggplotly(vis_3)


