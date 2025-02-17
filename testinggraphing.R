library(tidyverse)

#Titanic dataset from github
df <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/refs/heads/master/titanic.csv")

view(df)
# testing out various plotting !! TESTING!! 


# scatterplot

ggplot(data = df,
       aes(x = Age,
           y = Survived, 
           color = Sex)) +
  geom_point(size = 3,
             alpha = 0.5) +
  labs(title = 
         "Title",
       x = "?",
       y = "?") +
  theme_minimal()


#boxplot

df %>%
  ggplot(aes(x = Pclass,
             y = Survived,
             fill = Sex)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "?",
       x = "?",
       y = "?") +
  theme_minimal()


#bar plot

ggplot(df,
       aes(x = Pclass,
           y = Survived,
           fill = Sex)) +
  geom_bar(stat = "summary",
           fun = "mean",
           alpha = 0.5) +
  labs(title = "Survival Rate of Passengers on Titanic",
       x = "Each Class on The Titanic",
       y = "Survived") +
  theme_minimal()


# scatter plot 2

ggplot(data = df,
       aes(x = PassengerId,
           y = Survived, 
           color = Sex)) +
  geom_point(size = 3,
             alpha = 0.5) +
  labs(title = 
         "Title",
       x = "?",
       y = "?") +
  theme_minimal()


# bar plot 3

ggplot(df,
       aes(x = Pclass,
           y = Survived,
           fill = Sex)) +
  geom_bar(stat = "summary",
           fun = "mean",
           alpha = 0.5) +
  labs(title = "Survival Rate of Passengers on Titanic",
       x = "Each Class on The Titanic",
       y = "Survived") +
  theme_minimal()
