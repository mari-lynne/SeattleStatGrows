# Tutorial 4 Start =============================================================

# Aims:

# Explore trends in enteric infection data over time
# Visualize correlations between variables
# Use loops to automate our code



# Set up -----------------------------------------------------------------------


# Load Packages
library(ggpubr)
library(ggplot2)
library(viridis)
library(ggvis)
library(stringr)
library(forcats)
library(janitor)
library(dplyr)
library(tidylog)
library(patchwork)  # Use install.packages("package_name") if not installed yet


# Set Directories (file paths)
work_dir <- c("C:/Users/mjohnso5/Documents/teaching")

in_dir <- "C:/Users/mjohnso5/Documents/teaching/data" # Where input data is saved
plot_dir <- "C:/Users/mjohnso5/Documents/teaching/plots"
out_dir <- "C:/Users/mjohnso5/Documents/teaching/results" 

setwd(work_dir) 


# Load data
data <- read.csv(file = file.path(in_dir, "GBD_enteric_subset.csv"), header=T, stringsAsFactors = TRUE)
# Note strings as factors, sets up chr columns as factors (i.e your categorical data)


# Tidy Data --------------------------------------------------------------------
data <- select(data, -measure, -cause)

# Rename variables
data <- rename(data, enteric_deaths_pct = val)

# Update from decimal into percentage
data$enteric_deaths_pct <- data$enteric_deaths_pct * 100

# Not separating by sex
data <- filter(data, sex == "Both")

levels(data$age)
data$age <- fct_relevel(data$age, "<5 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years")

data$location <- fct_relevel(data$location, c("Western Sub-Saharan Africa", "Central Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Eastern Sub-Saharan Africa", "Central Asia", "South Asia", "Southeast Asia", "East Asia"))

# Enteric Deaths Across Age groups--------------------------------------------

  ggplot(data,
         aes(x = year, y = enteric_deaths_pct, color = location)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend in Enteric Deaths Over Time", x = "Year", y = "Enteric Deaths (%)") +
  facet_grid(~age) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")


# This data was just looking at a percentage of deaths - what about raw numbers?

# Tutorial 4 Start) Total Deaths comparison ------------------------------------


death_nums <- read.csv(file = "C:/Users/mjohnso5/Documents/teaching/data/GBD_enteric_subset_pt2.csv", stringsAsFactors = TRUE)

death_nums <- rename(death_nums, enteric_deaths_num = val)
death_nums <- filter(death_nums, sex == "Both")

# remove extra columns
data <- select(data, -sex)
death_nums <- select(death_nums, -sex)


# Joining data (if no sample/subject ID - make a key to join data by, or can also join by multiple variables)
death_nums$key <- death_nums$key <- paste(
  death_nums$location,
  death_nums$age,
  death_nums$year,
  sep = "_"
) # Paste columns

data$key <- data$key <- paste(
  data$location,
  data$age,
  data$year,
  sep = "_"
) # Paste columns

# Join data
all_data <- left_join(data, death_nums, by = "key")
# Note repeated columns because same column names

# delete upper/lower CI cols (we wont use these for the tutorial's sake)
death_nums <- dplyr::select(death_nums, -c("upper", "lower", "metric"))
data <- dplyr::select(data, -c("upper", "lower", "metric"))


# OR join by multiple grouping variables
all_data <- left_join(data, death_nums, by = c("location", "age", "year"))
all_data <- select(all_data, -cause)

write.csv(all_data, file = "C:/Users/mjohnso5/Documents/teaching/data/enteric_death_data.csv", row.names = F)

# Clean columns (just keep first instance of x column)
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


# Exercises -------------------------------------------------------------------

# 1) Re-plot Enteric Deaths over time (as before, split by age, color by location), but with death number on the y axis

ggplot(all_data,
       aes(x = year, y = enteric_deaths_num, color = location)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend in Enteric Deaths Over Time", x = "Year", y = "Enteric Deaths (%)") +
  facet_grid(~age) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom") + scale_y_log10()


# 2) Just look at the 5-9 age group within South Asia + Western Sub-Saharan Africa

# Tip: remember our filter operators == != & | %in%
# %in% c("value_1", "value_2")


a <- all_data %>% filter(age == "5-9 years" & location %in% c("Western Sub-Saharan Africa", "South Asia")) %>%
  ggplot(aes(x = year, y = enteric_deaths_pct, color = location)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend in Enteric Deaths Over Time", x = "Year", y = "Enteric Deaths %") +
  facet_grid(~age) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom") + scale_y_log10()


b <- all_data %>% filter(age == "5-9 years" & location %in% c("Western Sub-Saharan Africa", "South Asia")) %>%
  ggplot(aes(x = year, y = enteric_deaths_num, color = location)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend in Enteric Deaths Over Time", x = "Year", y = "Enteric Deaths Num") +
  facet_grid(~age) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom") + scale_y_log10()


a

b


# 3) Look again at the total percentage deaths but also filtered as above 5-9 


# Save as new plots

# Compare two plots together 
library(patchwork) # https://patchwork.data-imaginist.com/
# Save plots as new objects then replot 
(a / b) + plot_layout(guides = "collect")




# Answers Pt 1------------------------------------------------------------------



# No peeking until you've tried! :)





# 1)

ggplot(all_data, aes(x = year, y = enteric_deaths_num, colour = location)) +
  geom_line() +
  geom_point() +
  facet_grid(~age) +
  theme_bw()


# 2)

a <- all_data %>% filter(age == "5-9 years" & location %in% c("South Asia", "Western Sub-Saharan Africa")) %>%
  ggplot(aes(x = year, y = enteric_deaths_num, colour = location)) +
  geom_line() +
  geom_point() +
  theme_bw() + scale_y_log10()

a

# 3) Original 
b <- all_data %>% filter(age == "5-9 years" & location %in% c("South Asia", "Western Sub-Saharan Africa")) %>%
  ggplot(aes(x = year, y = enteric_deaths_pct, colour = location)) +
  geom_line() +
  geom_point() +
  theme_bw()

(a / b) + plot_layout(guides = "collect")

# South Asia, overall infection numbers are decreasing
# western subsarharn Africa however we do see an increase
# In the case where percentage deaths is increasing, that could be due to other killers being controlled better- i.e introduction of malaria nets, AIDs..., is one enteric infection driving this trend?S

# Wheras in Sub-Saharan Africa, we see both a percentage and total increase
# What factors could be exasperating deaths

# As you can see we have many questions and variables we would like to explore
# This would require a lot of plot code... or not!



# For Loops ------------------------------------------------------------------


# Basic Structure
for(variable in vector) {
  Perform function on variable
}

# Examples
chr_vec <- c("a", "b", "c", "geese")

for (i in chr_vec) {
  print(i)
}

# So iteration 1 (i = 1) prints a
#    iteration 2 (i = 2) prints b

for (i in (1:10)) {
  print(i)
}

# Basic example structured:
# Step 1: Define vector to loop over

num_vec <- (1:10)

# 2) Write loop

for (i in num_vec) {
  print(i *10)
}


# Q) Write a loop squaring the values from your data frame column - enteric_deaths_pct
# all_data$enteric_deaths_pct ^

test_vec <- head(all_data$enteric_deaths_num)

for (i in test_vec) {
  print(i ^ 2)
}



### Save new values --------------------------------------------------------


# R2 Example - save results into vector ####

# Calculate the R^2 values from a vector containing Pearson R coeficients
# Save the output of the loop in a new vector

# Step 1:
# Create vector to loop over - (Dummy R values)
# Create empty vector to save output of loop to 

R_values <- c(0.8, 0.2, 0.53, 0.64, 0.5) # Input


output_R2 <- numeric(length(R_values)) # character() etc.

output_R2

# Step 2: Write loop
for (i in R_values) {
  output_R2 <- (i ^2)
}

# Check Results
output_R2


# Problem: the loop works but the output is only saving the last calculation
# We need to index our input and output vectors so we can keep track of what row the loop is at-- Then the results can be saved to the corresponding place in the output vector

# Solution: create a index to subset our input and ouptput objects respectively
# Use seq_along function to create an index

seq_along(R_values)
seq_along(num_vec)
seq_along(chr_vec)

# Subsetting reminder

chr_vec[1] # 4th element of vector

test_list <- list(data, all_data, b)
test_list[[1]] # 3rd element of list


# REDO R2 example:

# Step 2: Write loop
for (i in seq_along(R_values)) { #for every element in vector R_values, perform function
  output_R2[i] <- R_values[i] ^ 2 # Subset our input and output vectors by each loop iteration [i] index
}

output_R2 

# The first iteration of the loop (first row) would be 0.8^2, this is also saved in the first row of our output vector #The next iteration would be 0.2^2 and so on...


### Prep data for plot loop  ---------------------------------------------------

other_causes <- read.csv(file = file.path(in_dir, "other_causes.csv"), stringsAsFactors = T) # Or just use the full file path/file_name.csv
str(other_causes)
# breakdown of other diseases

# Plot loops -------------------------------------------------------------------

# Basic plot loop structure:

for(every variable i in dataframe) { # Loop through variables in DF to plot
  plt <- plot variable i against y # Plot parameters and save plot output as object
  print(plt) # Print plot output to screen
}



# Translate to code :)

# Step 1: Define vector of column names to iterate the for loop over ##
# We can get a list of variable names to loop over from colnames

#Select  data from dataframe
cols <- other_causes %>% select(contains("num")) # use helper functions

# Use names or colnames
col_names <- colnames(cols)
col_names

# Step 2:

# Think of x and y variables/type of plot
# x = enteric deaths, y = other columns in vector
# Then save the static variable as a vector
x_var <- other_causes$enteric_deaths_num

# 2b) 
# I like to write out basic plot code first (no loop) to prep my code
ggplot(other_causes, aes(x=x_var, y=malaria_number, color = location)) +
  geom_point() # or facet_wrap


# Step 3: 

# Put code together in loop 

# Needs the for statement (), and the function code {}

for (i in col_names){
  plot <- ggplot(other_causes, aes(x=.data[["enteric_deaths_num"]], y=.data[[i]], color = location)) + geom_point()# use each value of i as the y axis
  print(plot) # print output after each iteration
  Sys.sleep(2)
}


# Tidy plot and add regression + correlation

west <- filter(other_causes, location == "Western Sub-Saharan Africa")

for (i in col_names){
  plot <- ggplot(west, aes(x=.data[["enteric_deaths_num"]], y=.data[[i]], color = year)) + geom_point() +
  stat_cor(method = "pearson") +theme_bw()   + 
    geom_smooth(method=lm, alpha=0.25, color="black", fill="darkgreen")
  print(plot) 
  Sys.sleep(2)
}


# Other Useful for loops ####

# Loop over files ####

# Get vector containing names of files to loop over
data_files <- list.files("C:/Users/mjohnso5/Documents/teaching/data", pattern = "*.csv")

data_files

# Read csv files
for(i in seq_along(data_files)) {                              
  assign(paste0("data", i), #pastes 'data' + row index i as file name to <- assign
         read.csv2(paste0(data_files[i])))# pastes each filename to the read.csv function,                                         subsetted by index
}


setwd(in_dir)

# IF ELSE LOOPS ----------------------------------------------------------------

# Copy data to keep it safe
df <- other_causes

# Create an empty vector to store results
malaria_burden <- character(nrow(df))

# Loop over each row
for (i in seq_along(df)) {  # same as seq_along
  if (df$malaria_number[i] > 10000) {
    malaria_burden[i] <- "High"
  } else {
    malaria_burden[i] <- "Low"
  }
}

head(malaria_burden)

# Add to data frame
df$malaria_burden <- malaria_burden

# Check result
head(df[, c("malaria_number", "malaria_burden")])



# Note: for this sort of problem I actually like to use the ifelse function with mutate
# But now you understand what the ifelse function is doing to your data - it is iterating through every row, like in a loop, checking IF a statement is true, and then performing a function (in this case mutate), based on the condition

df <- other_causes %>%
  mutate(
    malaria_burden = ifelse(
      malaria_number > 10000,  # condition
      "High",                  # value if TRUE
      "Low"                    # value if FALSE
    )
  )


ifelse(data$dbgapconsent == )



# Extra notes for mari  --------------------------------------------------------

# How I made the input data

subset <- all_data %>% filter((age == "5-9 years" & location %in% c("South Asia", "Western Sub-Saharan Africa"))) %>% select(-upper, -lower) # ignoring CIs for now

# Load latest dataset 

other_causes <- read.csv(file = file.path(in_dir, "other_death_causes.csv")) 

str(other_causes)
# Split metric column into percentages and number columns (wide format - good for correlations)

other_causes <- other_causes %>%
  select(-upper, -lower) %>%   # Drop what you don’t need
  pivot_wider(
    names_from  = c(cause, metric),  # Use cause & metric together
    values_from = val,
    names_sep   = "_"                 # Makes e.g. malaria_Number
  )

other_causes <- inner_join(subset, other_causes, by = c("year", "location"))
other_causes <- clean_cols(other_causes)
other_causes <- select(other_causes, -key, -metric, -cause, -measure, -sex)
other_causes <- distinct(other_causes)
other_causes <- clean_names(other_causes)

write.csv(other_causes, file = file.path(in_dir, "other_causes.csv"), row.names = F)
