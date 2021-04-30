library(scales)
library(dplyr)
library(ggplot2)

# Reset Vars
rm(list = ls())

# Prepare Data
pts = read.csv("C:\\Users\\isaac\\Documents\\[College]\\STAT201\\data\\stat201_final_merged.csv")

# Research Question: Is there a significant difference in the average rates of 
# depression and anxiety between White people and BIPOC?

# Clean data
pts_num <- pts %>%
  # Filter our NA values
  na.omit()

# Shorten labels for each race
pts_num <- pts_num %>%
  mutate(race = ifelse(race == "Black or African American", "Black",
                 ifelse(race == "Prefer not to say", "Unknown",
                  ifelse(race == "Native Hawaiian/ Other Pacific Islander", "P.I.",
                   ifelse(race == "American Indian or Alaska native", "Indegenous",
                    ifelse(race == "Asian", "Asian", "White"))))))

#Add BIPOC column
pts_num <- pts_num %>%
  mutate(white_or_bipoc = ifelse(race == "White", "White", "BIPOC"))

# Add columns for if white/bipoc and anxiety/depression present
pts_avg <- pts_num %>%
  mutate(white_and = ifelse(race == "White" & dx_and == "Anxiety present", 1, 0)) %>%
  mutate(white_dep = ifelse(race == "White" & dx_dep == "Depression present", 1, 0)) %>%
  mutate(bipoc_and = ifelse(race != "White" & dx_and == "Anxiety present", 1, 0)) %>%
  mutate(bipoc_dep = ifelse(race != "White" & dx_dep == "Depression present", 1, 0)) %>%
  select(white_and, white_dep, bipoc_and, bipoc_dep)

# T-Test
# Disprove null with small P. H1 = significant diff.
t.test(pts_avg$white_and, pts_avg$bipoc_and, paired = TRUE)

# Disprove null with small P. H1 = significant diff.
t.test(pts_avg$white_dep, pts_avg$bipoc_dep, paired = TRUE)


# Graphs - White vs BIPOC
ggplot(pts_num, aes(x = white_or_bipoc, fill = dx_and)) +
  geom_bar(position = "fill") + 
  ggtitle("Anxiety by race") +
  labs(x = "BIPOC or not")

ggplot(pts_num, aes(x = white_or_bipoc, fill = dx_dep)) +
  geom_bar(position = "fill") + 
  ggtitle("Depression by race") +
  labs(x = "BIPOC or not")

# My reason for using BIPOC and not breaking it down by race was so that I
# Could use a t.test instead of ANOVA for my analysis, as all the others
# Are already using ANOVA.

# Breakdown by race, in case you are curious
ggplot(pts_num, aes(x = race, fill = dx_and)) +
  geom_bar(position = "fill") + 
  ggtitle("Anxiety by race") +
  labs(x = "Race")

ggplot(pts_num, aes(x = race, fill = dx_dep)) +
  geom_bar(position = "fill") + 
  ggtitle("Depression by race") +
  labs(x = "Race")


