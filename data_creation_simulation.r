install.packages("tidyverse")
library(tidyverse)

set.seed(123)

n <- 4000

# Define the Likert scale categories
categories <- 1:5

# Define the probabilities for each category

probabilities_HiOrd_Q1 <- c(0.1, 0.2, 0.1, 0.2, 0.4)
probabilities_LoOrd_Q1 <- c(0.1, 0.2, 0.1, 0.2, 0.4) 
probabilities_HiOrd_Q2 <- c(0.1, 0.2, 0.1, 0.2, 0.4)
probabilities_LoOrd_Q2 <- c(0.2, 0.1, 0.2, 0.2, 0.3)

data.frame(
  Value = rep(1:5, 4),
  Probability = c(
    probabilities_HiOrd_Q1,
    probabilities_LoOrd_Q1,
    probabilities_HiOrd_Q2,
    probabilities_LoOrd_Q2
  ),
  Group = rep(c("HiOrd", "LoOrd", "HiOrd", "LoOrd"), each = 5),
  Q = rep(c("Q1", "Q2"), each = 10)
) %>%
  ggplot(aes(x = Value, y = Group, height = Probability, fill = Group)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.7) +
  facet_wrap(~Q, nrow = 1) +
  labs(
    title = "Combined Probability Distributions for Q1 and Q2",
    x = "Value",
    y = "Group",
    fill = "Group"
  ) +
  theme_ridges(grid = FALSE) +
  theme(legend.position = "right") +
  scale_fill_manual(values=mycols)


#Create data set based on given probabilities

#choose sample size and sub group sizes
total_sample <- 6000
group <- total_sample/4
group_half <- total_sample/8
group_sublarge <- (total_sample/8)*1.5 #must be the inverse odds of next line
group_subsmall <- (total_sample/8)*0.5

df <- bind_rows(

    data.frame(
      Order_Freq = c(rep("High Order",group_half ),
                       rep("Low Order",group_half)) ,
      SEQ = c(sample(categories, group_half, replace = TRUE, prob = probabilities_HiOrd_Q1 ),
              sample(categories, group_half, replace = TRUE, prob = probabilities_LoOrd_Q1)
      )
    ) %>%
    mutate(wave="Q1")
  ,
  data.frame(
    Order_Freq = c(rep("High Order",group_sublarge ),
                     rep("Low Order",group_subsmall)) ,
    SEQ = c(sample(categories, group_half, replace = TRUE, prob = probabilities_HiOrd_Q2 ),
            sample(categories, group_half, replace = TRUE, prob = probabilities_LoOrd_Q2)
    )
  ) %>%
    mutate(wave="Q2")
) %>%
  #get top 2box
  mutate(SEQ.t2b = ifelse(SEQ>=4,1,0))

df <- read_csv(here("df.csv"))
