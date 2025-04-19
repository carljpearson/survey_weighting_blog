#install (if needed) and load packages

install.packages("tidyverse") # For plots and data cleaning
library(tidyverse)
install.packages("DescTools") # For unweighted binomial CIs
library(DescTools)
install.packages(c("survey","srvyr")) # For survey weighting and analysis
library("survey")
library("srvyr")
filter <- dplyr::filter
install.packages("sjPlot")
library(sjPlot)
install.packages("here") # For reading in data
library(here)
source("https://raw.githubusercontent.com/carljpearson/cjp_r_helpers/main/cjp_r_helpers.r") # For my plot theme

mycols <- c(
  "#4d6e50",
  "#7fa281",
  "#55aaff", 
  "#6b7698",
  "#8e5b29",
  "#3c6c83"
)

# Load data

df <- read_csv(here("df.csv"))

# look at simulated sample data

head(df %>% slice_sample(n=100))

df %>%
  group_by(Order_Freq, wave) %>%
  count()

# Get/create data.frame for population

pop_freq <- data.frame(
  Order_Freq = c("High Order","Low Order","High Order","Low Order"),
  wave = c("Q1","Q1","Q2","Q2"),
  Freq = c(100000,400000,120000,450000)
) %>% # Specify data
  mutate(stratum=paste(Order_Freq,wave)) %>%
  select(stratum,Freq)

pop_freq

# Get/create data.frame for population

pop_freq <- data.frame(
  Order_Freq = c("High Order","Low Order","High Order","Low Order"),
  wave = c("Q1","Q1","Q2","Q2"),
  Freq = c(100000,400000,120000,450000)
) %>% # Specify data
  mutate(stratum=paste(Order_Freq,wave)) %>%
  select(stratum,Freq)


#### Analyze data unweighted ####

# Analyze the data
analysis_unweighted <- 
  df %>% # Start with the dataframe 'df'
  group_by(wave) %>% # Group the data by the 'wave' variable
  mutate(total=n()) %>% # Create a new column 'total' which is the count of observations in each 'wave' group
  group_by(wave,total) %>% # Group the data by both 'wave' and 'total'
  summarize(n=sum(SEQ.t2b)) %>% # Count the number of responses for binomial CI calculation
  rowwise() %>% # Operate row-by-row, necessary for BinomCI
  mutate(
    proportion = BinomCI(n,total,method = "agresti-coull")[1], # Calculate the proportion and its confidence interval using the Agresti-Coull method, and extract the proportion
    ci_lower = BinomCI(n,total,method = "agresti-coull")[2], # Extract the lower confidence interval
    ci_upper = BinomCI(n,total,method = "agresti-coull")[3] # Extract the upper confidence interval
  )

analysis_unweighted

# Plot the data
analysis_unweighted %>% # Start with the summarized data
  ggplot(aes(y=proportion,x=wave,fill = wave)) + # Create a ggplot object, mapping 'proportion' to y, 'wave' to x, and 'wave' to fill color
  geom_bar(stat="identity") + # Create a bar plot, using 'identity' to directly map the 'proportion' values to the bar heights
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper)) + # Add error bars representing the confidence intervals
  theme_cjp() + # Add my custom theme for prettiness
  scale_y_continuous(labels = scales::percent) + # Format the y-axis labels as percentages
  labs(y="SEQ Top 2 Box",x='Wave') + # Add labels to the y and x axes
  scale_fill_manual(values=mycols) + # Apply custom colors from the 'mycols' vector to the fill of the bars
  coord_cartesian(ylim=c(0,1)) + # Set the y-axis limits from 0 to 1 for our percent data
  theme(
    legend.position="none" # Remove the legend
  ) 

#### Analyze data weighted ####

# Calculate post-stratification weights

survey_design <-
  df %>% # Specify data
  mutate(stratum=paste(Order_Freq,wave)) %>% # Combine different categorical strata into one variable (! important !)
    as_survey_design(ids = 1) %>% # Convert our DF into a "survey design" object
    postStratify( 
      strata = ~stratum,  # Specify the stratification variable(s)
      population = pop_freq # Provide the population frequencies
    )

# Now, use the survey design in your analysis
analysis_weighted_poststrat_1step <- 
  survey_design %>% # Start with survey design object
    group_by(wave) %>% # Our calcuation will be between waves
    summarize(
      survey_mean(SEQ.t2b, prooprtion=T,prop_method = "logit",vartype='ci') # Use the mean function is the survey package, specify the data type
    )  %>%
  rename(proportion=coef,ci_lower=`_low`,ci_upper=`_upp`) %>%
    mutate(
      method="poststrat_1step" # Adding method column for later plotting
    )


#bring together and plot


analysis_weighted_poststrat_1step %>%
  # Combine rows from the first data frame with rows from a modified second data frame.
  bind_rows(
    # Start processing the 'analysis_unweighted' data frame.
    analysis_unweighted %>%
      # Select only the specified columns: 'wave', 'proportion', 'ci_lower', 'ci_upper'.
      select(wave, proportion, ci_lower, ci_upper) %>%
      # Add a new column named 'method' and assign the value "unweighted" to all rows.
      mutate(method = "unweighted")
  ) %>% # The combined data frame now flows into the ggplot function.
  # Initialize a ggplot object, mapping 'proportion' to y, 'wave' to x, and 'wave' to the fill color.
  ggplot(aes(y = proportion, x = wave, fill = wave)) +
  # Add a bar geom; 'stat="identity"' means bar heights are taken directly from the 'y' aesthetic ('proportion').
  geom_bar(stat = "identity") +
  # Add error bars, mapping 'ci_lower' to ymin and 'ci_upper' to ymax.
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  # Create facets (subplots) arranged horizontally based on the unique values in the 'method' column.
  facet_grid(. ~ method) +
  # Apply my custom theme named 'theme_cjp'
  theme_cjp() +
  # Format the y-axis labels as percentages.
  scale_y_continuous(labels = scales::percent) +
  # Set the axis labels.
  labs(y = "SEQ Top 2 Box", x = 'Wave') +
  # Manually specify the fill colors using the 'mycols' object
  scale_fill_manual(values = mycols) +
  # Set the y-axis limits from 0 to 1 without clipping data points outside this range.
  coord_cartesian(ylim = c(0, 1)) +
  # Start modifying specific theme elements.
  theme(
    # Remove the legend from the plot.
    legend.position = "none"
  )


#Formal model

install.packages("emmeans")
library(emmeans)

#create model for logistic regression
mod.glm <- svyglm(SEQ.t2b ~ wave,survey_design)
summary(mod.glm)
#post hoc test
emmeans(mod.glm, ~ wave) %>% 
  pairs()
