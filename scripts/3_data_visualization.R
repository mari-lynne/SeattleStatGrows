# Tutorial 3 Data Visualization

# Packages =====================================================================


# Install any packages not installed yet
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
  "janitor",
  "patchwork"
))


library(ggpubr)
library(ggplot2)
library(viridis)
library(ggvis)
library(stringr)
library(forcats)
library(janitor)
library(dplyr)
library(tidylog)
library(patchwork)


# Set Up  ======================================================================

# Save file paths as variables to be used later
# Update with your directory names
# Can also just type in file path directly

work_dir <- c("C:/Users/mjohnso5/Documents/teaching")

in_dir <- "C:/Users/mjohnso5/Documents/teaching/data" # Where input data is saved
plot_dir <- "C:/Users/mjohnso5/Documents/teaching/plots"
out_dir <- "C:/Users/mjohnso5/Documents/teaching/results" 

setwd(work_dir) 

# I.e this is the same as
data <- read.csv(file = file.path(in_dir, "GBD_enteric_subset.csv"), header=T) 
# This:
data <- read.csv(file = "C:/Users/mjohnso5/Documents/teaching/data/GBD_enteric_subset.csv")


# Check data ===================================================================

# Always inspect data and factors first
str(data)
head(data)
table(data$measure) ; table(data$cause) ; table(data$metric)

# Remove extra variables (data set was already subsetted from larger table in the GBD database)
data <- select(data, -measure, -cause)

# Rename variables
data <- rename(data, enteric_deaths_pct = val)
# Note: looking at percentage of enteric deaths; (enteric deaths/all deaths in that population+time point) *100

# Update from decimal into percentage
data$enteric_deaths_pct <- data$enteric_deaths_pct * 100

# Update our categorical variables from chr type to factor
data <- data %>%
  mutate_if(is.character, as.factor) # saves running data$sex <- as.factor(data$sex) x 3

# Qs/Hypotheses ================================================================

# 1) Compare  deaths from enteric infections across countries and age groups
# 2) Investigate differences between sex 
# 3) Compare enteric deaths over time and age groups


# 1) Enteric deaths across geographical regions --------------------------------

# Just one year, and both sexes (otherwise would be duplicating data)
# Always filter first, then make plot :)


data %>% filter(year == 2019 & sex == "Both") %>%
  ggplot(aes(x = location, y = enteric_deaths_pct, fill = location)) + # axis variables and colors (aesthetics)
  geom_boxplot() + # Type of plot (geometry)
  labs(title = "Enteric Deaths across Regions in 2019 (all age groups)", y = "Percentage Enteric Deaths") + # Customize labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") # Adjust theme

# Extra color pallets 
library(viridis)

data %>% filter(year == 2019 & sex == "Both") %>%
  ggplot(aes(x = location, y = enteric_deaths_pct, fill = location)) + # axis variables and colors (aesthetics)
  geom_boxplot() + # Type of plot (geometry)
  labs(title = "Enteric Deaths across Regions in 2019 (all age groups)", y = "Enteric Deaths (%)") + # Customize labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + # Adjust plot details with theme
  scale_fill_viridis(discrete = TRUE, option = "plasma") # scale_fill_viridis(discrete = TRUE, option = "plasma")

?scale_fill_viridis

library(RColorBrewer)
data %>% filter(year == 2019 & sex == "Both") %>%
  ggplot(aes(x = location, y = enteric_deaths_pct, fill = location)) + # axis variables and colors (aesthetics)
  geom_boxplot() + # Type of plot (geometry)
  labs(title = "Enteric Deaths in 2019 (all age groups)", y = "Enteric Deaths (%)") + # Customize labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + # Adjust theme
  scale_fill_brewer(type = "div", palette = "Spectral")

?scale_fill_brewer

# Reorder factor levels
library(forcats)

levels(data$location)

data$location <- fct_relevel(data$location, c("Western Sub-Saharan Africa", "Central Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Eastern Sub-Saharan Africa", "Central Asia", "South Asia", "Southeast Asia", "East Asia"))


# 2) Sex Differences -----------------------------------------------------------

# Make a box plot comparing the differences in enteric deaths between sex groups within just one region

# First filter data
# Then think of x/y variables, type of plot, color schemes etc.

your_plot <- data %>% filter(year == 2019 & location == "Central Sub-Saharan Africa") %>% 
  ggplot(aes(x = sex, y = enteric_deaths_pct, fill = sex)) + 
  geom_boxplot() + 
  labs(title = "Enteric Deaths in 2019 in Central Sub-Saharan Africa (all age groups)", y = "Enteric Deaths (%)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")  + stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")

  
your_plot

# Save plot to a new object "your_plot <- "
    
### Add statistical comparisons ------------------------------------
library(ggpubr)

my_comparisons <- list(c("Female", "Male"), c("Female", "Both")) 
# Specify variables to test, add more comparisons with, c("Both", "Female") inside the list

your_plot + stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")
# probably because we are comparing across only 5 data points within each group

?stat_compare_means

# ggboxplot
library(ggpubr)

data %>%
  filter(year == 2019 & sex == "Both") %>%
  ggviolin(
    x = "location",
    y = "enteric_deaths_pct",
    fill = "location",        # Color by location
    palette = "Spectral",     # Use ColorBrewer palette
  ) +
  labs(
    title = "Enteric Deaths in 2019 (all age groups)",
    y = "Enteric Deaths (%)",
    x = "Location"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )

ggsave(filename = file.path(plot_dir, "test_plot.png"))

ggsave(filename = "C:/Users/mjohnso5/Documents/teaching/plots/test_plot.png")

# Tutorial 4 Start =============================================================

# Aims:

# Explore trends in enteric infection data over time
# Plot correlation plots
# Explore 

# 3) Enteric Deaths Across Age groups ------------------------------------------

# As there were no sex differences, let's just look at them together

data <- filter(data, sex == "Both")

# Deaths over time in south asia
SA_data <- filter(data, location == "South Asia")

SA_data %>%
  ggplot(aes(x = year, y = enteric_deaths_pct)) +
  geom_line(color = "lightblue") +
  geom_point(color = "darkgreen") +
  labs(title = "Trend in Enteric Deaths Over Time (South Asia)", x = "Year", y = "Enteric Deaths (%)") + facet_grid(~age) + theme_bw()

data$age <- fct_relevel(data$age, "<5 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years")

# Is this true across other regions?
data %>% filter(sex == "Both") %>%
  ggplot(aes(x = year, y = enteric_deaths_pct, color = location)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend in Enteric Deaths Over Time", x = "Year", y = "Enteric Deaths (%)") + facet_grid(~age) + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")


# This data was just looking at a percentage of deaths - what about raw numbers?

# Total Deaths comparison ------------------------------------------------------

death_nums <- read.csv(file = "C:/Users/mjohnso5/Documents/teaching/data/GBD_enteric_subset_pt2.csv", stringsAsFactors = TRUE)

death_nums <- rename(death_nums, enteric_deaths_num = val)

library(dplyr)
library(tidylog)

# Joining data (if no sample/subject ID make a key)

death_nums$key <- death_nums$key <- paste(
  death_nums$location,
  death_nums$sex,
  death_nums$age,
  death_nums$year,
  sep = "_"
) # Paste columns

data$key <- data$key <- paste(
  data$location,
  data$sex,
  data$age,
  data$year,
  sep = "_"
) # Paste columns

# Join data
all_data <- left_join(data, death_nums, by = "key")

# Clean columns
clean_cols <- function(df) {
  # Tidy duplicate columns
  colnames(df) <- str_remove(colnames(df), "\\.x")
  # Remove duplicate column
  df <- df[ ,str_detect(colnames(df), "\\.y")==FALSE]
  # Remove columns with all NA values
  df <- df[, colSums(is.na(df)) != nrow(df)]
  return(df)
}

all_data <- clean_cols(all_data) # Note should have renamed upper and lower before, but we're not looking at them for now so ignore
all_data <- droplevels(all_data)
table(all_data$sex)

# Replot Enteric Deaths over time but with death number on y

# Just look at 5-9 age group (filter)

# Change log scale


# Compare two plots together 
library(patchwork) # https://patchwork.data-imaginist.com/
# Save plots as new objects then replot 
(a / b) + plot_layout(guides = "collect")


# Correlation plots -------------------------------------------------------------

all_data %>% filter(age == "5-9 years" & year == "2019") %>% ggplot(aes(x = enteric_deaths_num, y = enteric_deaths_pct)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_log10() +
  stat_cor(method = "pearson") +
  geom_smooth()

# each point is a country value in that year (plot more for illustrative purposes)

# Check for normality ---------------------------------------------------------

# Create a histogram with a normal distribution curve of variable of interest
# Decide what metric you might look at later e.g 
# Comparing deaths across time (within a country and age group)
sub <- data %>% filter(age == "5-9 years" & year == "2019")

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

# Answers ---------------------------------------------------------

your_plot <- data %>% filter(year == 2019 & location == "Central Sub-Saharan Africa" & sex != "Both") %>% 
  ggplot(aes(x = sex, y = enteric_deaths_pct, fill = sex)) + 
  geom_boxplot() + 
  labs(title = "Enteric Deaths in 2019 in Central Sub-Saharan Africa (all age groups)", y = "Enteric Deaths (%)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + 
  scale_fill_brewer(type = "div", palette = "Spectral")



all_data %>% ggplot(aes(x = year, y = enteric_deaths_num, colour = location)) +
  geom_line() +
  geom_point() +
  facet_grid(~age) +
  theme_bw()

a <- all_data %>% filter(age == "5-9 years") %>%
  ggplot(aes(x = year, y = enteric_deaths_pct, colour = location)) +
  geom_line() +
  geom_point() +
  theme_bw()

all_data %>% filter(age == "5-9 years") %>%
  ggplot(aes(x = year, y = enteric_deaths_num, colour = location)) +
  geom_line() +
  geom_point() +
  theme_bw()


# add log scale/limits
b <- all_data %>% filter(age == "5-9 years") %>%
  ggplot(aes(x = year, y = enteric_deaths_num, colour = location)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  theme_bw()


# The change in total deaths for 5-9 year olds from enteric infections is increasing, but not as dramatically as the percentage, therefore other causes of mortality in that group my have been going down, while enteric infections remained underlooked... AIDs/Malaria?

