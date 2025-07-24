# Tutorial 1-2

# Set Up =======================================================================


# Install Packages
install.packages(c(
  "ggpubr",
  "ggplot2",
  "viridis",
  "ggvis",
  "data.table",
  "stringr",
  "forcats",
  "dplyr",
  "tidylog",
  "janitor"
))

# Or just install min packages for tutorial, then load these lines with library
install.packages(c("dplyr", "tidylog", "janitor", "ggplot2"))


### Load Packages --------------------------------------------------------------

# Visualization
library(ggpubr)
library(ggplot2)
library(viridis)
library(ggvis)

# Data manipulation
library(data.table)
library(stringr)
library(forcats)
library(dplyr)
library(tidylog)
library(janitor)


### Directories and variables --------------------------------------------------

# Update with your directory names
work_dir <- c("C:/Users/mjohnso5/Downloads")

in_dir <- "C:/Users/mjohnso5/Downloads" # Where input data is saved
plot_dir <- "C:/Users/mjohnso5/Downloads"
out_dir <- "C:/Users/mjohnso5/Downloads" # file.path works similar to paste, but the separator is set to a /


setwd(work_dir) 
getwd()

# Read in data ----------------------------------------------------------------

data <- read.csv(file = file.path(in_dir, "GBD_enteric_subset.csv"), header=T)

data <- read.csv(file = "C:/Users/mjohnso5/Documents/teaching/data/GBD_enteric_subset.csv")

data <- clean_names(data) # Function from Janitor package

# Inspect and subset -----------------------------------------------------------

View(data)
head(data, 10)

str(data)

class(data)
class(data$measure)
class(in_dir)

data$age <- as.factor(data$age)

# Subset data (base R)

# Subset rows and columns
# Always rows x columns [], [row index/indices, column index], leave blank for all entries
data[1, 3]

# Use : to get sequence of numbers
data[1:5, 1:3]

# Subset rows
data[1:5,] # Leave row or column index blank to keep as is

# Subset columns
data$sex # or

data[, "sex"]
# Subset multiple columns
data[4, c("cause", "year")]

test_vector <- c("cause", "year", "sex")

# Save subset
test_subset <- data[1:4, ]

# dplyr version 

# Subset columns
select(data, sex, year)
data %>% select(sex, year)

# Subset rows
data %>% slice(2:6) # same as slice(data, 1:4) # just a style preference
slice(data, 1:4)

# Part 2 =======================================================================

# Filter data -----------------------------------------------------------------

# dplyr
data_2 <- filter(data, location == "Southeast Asia")

data %>% filter(location == "Asia")
# Filter using %in%, useful for filtering based on external chr vectors
data3 <- filter(data, location %in% c("Southeast Asia", "South Asia"))

# Filter for age just children, limit year past 1995 ...

# Filter duplicate data
data <- distinct(data)

# Rename variables
# measure, metric, cause, all the same as filtered from bigger data-set
data <- dplyr::rename(data, enteric_deaths = val)

write.csv(data,
          file=file.path(out_dir, "GBD_enteric_subset2.csv"),
          row.names = F)

?write.csv

# Table summaries -------------------------------------------------------------

head(data)

# Simple table
table(data$location)
  # Tidy factors
  data <- droplevels(data)

# Cross tabulation
table(data$location, data$age)

# Summary stats (numeric variables)
summary(data$enteric_deaths)
mean(data$enteric_deaths)
IQR(data$enteric_deaths)

# Go back to prez :)

# Data format ------------------------------------------------------------------

head(data)
# Long format, every observation is a single row

install.packages("tidyr")
library(tidyr) # good for reshaping data

# Convert from long to wide format: (probably wouldn't use this much)
# Each unique 'sex' becomes a new column, values are from 'enteric_deaths'
wide_data <- data %>% pivot_wider(names_from = sex,
                                  values_from = enteric_deaths)

long_data <- wide_data %>%
  pivot_longer(cols = c(Male, Female, Both),
               names_to = "sex",
               values_to = "enteric_deaths")


# Filter then inspect/summarize with dplyr
data %>%
  filter(location != "South Asia" & sex == "Both") %>%
  summarise(mean_enteric_deaths = mean(enteric_deaths, na.rm = TRUE))

# With base R (row condition by column condition)
# Subset appropriate conditions, then summarize mean()
data[data$location == "South Asia" & data$sex == "Both", c("enteric_deaths")]

# Mutate


# Tidy variables ---------------------------------------------------------------

# Refer to columns as variables, rows are observations (long format)
str(data)
# dplyr mutate (write over or make new variables)
data2 <- data %>% mutate(range = upper-lower)

?mutate

# Convert variables to appropriate data type (base)
data$location <- as.factor(data$location) # note save back to column (not data frame)
# Convert using mutate
data <- data %>% mutate(sex = as.factor(sex))

# Convert multiple factors at once (function iterates over all variables)
data <- data %>%
  mutate_if(is.character, as.factor)
# Other methods; lapply with if else, within read.csv(stringsAsFactors = TRUE)

# Rename variables
# measure, metric, cause, all the same as filtered from bigger data-set
data <- rename(data, enteric_deaths = val)

# Remove redundant columns
test <- select(data2, !c("cause", "metric", "measure"))

# Reorder
data <- select(data, enteric_deaths, upper, lower, everything())


# Check for normality ---------------------------------------------------------

# Create a histogram with a normal distribution curve of variable of interest
# Decide what metric you might look at later e.g 
# Comparing deaths across time (within a country and age group)
sub <- data %>% filter(sex == "Both" & location == "South Asia" & age == "10-14 years")

ggplot(sub, aes(x = enteric_deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = .005, fill = "lightblue", color = "black") + 
  geom_density(alpha = .2, fill = "darkgreen") + # Overlay a density plot
  labs(title ="Histogram of Enteric Deaths with Normal Curve")

# Binwidth is the x-axis units (ticks), will differ by data
# e.g if my range of deaths was 1-100 i might choose a bin of 10 

# QQ Plot
# Deviations from diagonal line may indicate non-parametric data
ggplot(sub, aes(sample = enteric_deaths)) + 
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot for Enteric Deaths")

shapiro.test(data$enteric_deaths) # Note sensitive to sample size


# Plotting ---------------------------------------------------------------------

# Hypothesis
# Were there differences in enteric deaths between regions in 2019
  # Did these deaths differ by age and sex?

# Tips:
# Think of the sort of plot you want, draw it out
# refresh on data frame structure
# Multiple time points, genders, countries
# Narrow down what we are interested in, as no filtering or faceting will just add up totals 

# No filtering
ggplot(data, aes(x = age, y = enteric_deaths, fill = age)) +
  geom_boxplot() +
  labs(title = "age", y = "Percentage Enteric Deaths")

# Filter to just look at 2019, and facet by sex
data %>% filter(year == 2019) %>%
ggplot(aes(x = location, y = enteric_deaths, fill = location)) +
  geom_boxplot() +
  labs(title = "Enteric Deaths by Location and Sex", y = "Percentage Enteric Deaths") + facet_wrap(~sex) + theme() + theme_bw()


# Deaths over time in south asia
data %>% filter(sex == "Both" & location == "South Asia") %>%
ggplot(aes(x = year, y = enteric_deaths, group = 1)) +
  geom_line(color = "lightblue") +
  geom_point(color = "darkgreen") +
  labs(title = "Trend of Enteric Deaths Over Time", x = "Year", y = "Enteric Deaths (%)") + facet_wrap(~age) + theme_bw()

# reorder age group factor
data$age <- fct_relevel(data$age, "<5 years", "5-9 years", "10-14 years")



# Part 2 
# Statistical tests on ggplot and plot modifications
# Add statistical tests








