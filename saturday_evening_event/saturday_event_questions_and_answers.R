#
# OCRUG Hackathon 2021-04
# Saturday Evening Data Challenge Event
# 2021-04-10
# 
# Answer Key
#


library(tidyverse)

dat <- read_csv("penguins.csv")

# Q1:
# Which island(s) have at least 2 penguin species?
dat %>%
  group_by(island) %>%
  summarize(n_species = n_distinct(species), .groups = "drop") %>%
  filter(n_species >= 2) %>%
  pull(island)


# Q2:
# Which species has the shortest mean bill length and what is its bill length value? 
# Remove any missing values from the calculation.
dat %>%
  group_by(species) %>%
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE), .groups = "drop") %>%
  filter(mean_bill_length == min(mean_bill_length)) %>%
  select(species, mean_bill_length)


# Q3:
# Create a scatter plot of bill length (x-axis) and bill depth (y-axis) colored by species.
ggplot(dat, aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point()


# Q4: 
# Create a scatter plot of body mass vs. flipper length for Gentoo penguins and add a linear trend line. 
# Put the value of the trend line slope in the title of the plot.
dat_gentoo <- dat %>%
  filter(species == "Gentoo")

trend_lm <- lm(body_mass_g ~ flipper_length_mm, data = dat_gentoo)
trend_slope <- coef(trend_lm)["flipper_length_mm"]

ggplot(dat_gentoo, aes(flipper_length_mm, body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = paste0("Gentoo Body Mass vs. Flipper Length, Trend Slope = ", trend_slope)
  )
  
# "body mass vs. flipper length" means body mass goes on the y-axis
# but most teams reversed the axes.  Here's the solution with flipped x and y-axes
dat_gentoo <- dat %>%
  filter(species == "Gentoo")

trend_lm <- lm(flipper_length_mm ~ body_mass_g, data = dat_gentoo)
trend_slope <- coef(trend_lm)["body_mass_g"]

ggplot(dat_gentoo, aes(body_mass_g, flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = paste0("Gentoo Flipper Length vs. Body Mass, Trend Slope = ", trend_slope)
  )
