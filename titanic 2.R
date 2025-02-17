"knitr::opts_chunk$set(echo = TRUE)"


library(janitor)
library(tidyverse)
library(reshape2)
library(knitr)


#Titanic dataset from github
# https://github.com/datasciencedojo/datasets/blob/master/titanic.csv
df <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/refs/heads/master/titanic.csv")

df %>% head() %>% kable()

view(df)


#which gender had the highest deaths
df %>% group_by(Survived, Sex) %>%
  tally() %>%
  dcast(Sex~Survived) %>%
  adorn_percentages("col") %>%
  adorn_totals() %>%
  adorn_pct_formatting() %>% kable()


ggplot(data = df %>% mutate(Survived = ifelse(Survived == 1, "Survived", "Perished")) %>% 
         group_by(Survived, Sex) %>%
         tally(),
       mapping = aes(x = Sex, 
                     y = n, 
                     group = Survived)) +
  geom_col(aes(fill = Survived), 
           position = "dodge") +
  geom_text(aes(y = n/2 ,label = n),
            position = position_dodge(width=1),size = 3) +
  labs(x = "Passenger's Gender",
       y = "Number of Passengers",
       title = "Distribution of Passengers by Sex and Survival Status")





# which class had the highest deaths
df %>% group_by(Survived, Pclass) %>%
  tally() %>%
  dcast(Pclass~Survived) %>%
  adorn_percentages("col") %>%
  adorn_totals() %>%
  adorn_pct_formatting() %>% kable()


ggplot(data = df %>% mutate(Survived = ifelse(Survived == 1, "Survived", "Perished")) %>% 
         group_by(Survived, Pclass) %>%
           tally(),
       mapping = aes(x = Pclass, 
                     y = n, 
                     group = Survived)) +
  geom_col(aes(fill = Survived), 
           position = "dodge") +
  geom_text(aes(y = n/2 ,label = n),
            position = position_dodge(width=1),
            size = 3) +
  labs(x = "Passenger Class",
       y = "Number of Passengers",
       title = "Distribution of Passengers by Class and Survival Status")





# average fare for each class
df %>% group_by(Pclass) %>%
  summarise(Average = mean(Fare)) %>% kable()


ggplot(data = df %>%
         group_by(Pclass) %>%
         summarise(Average = mean(Fare)),
       mapping = aes(x = factor(Pclass), 
                     y = Average)) +
  geom_col(aes(fill = factor(Pclass)), 
           position = "dodge") +
  geom_text(aes(label = round(Average, 2), 
                y = Average / 2),
            size = 3) +
  labs(x = "Passenger Class",
       y = "Average Fare",
       title = "Average Fare Per Passenger Class") +
        scale_fill_discrete(labels = c("1" = "1st Class", "2" = "2nd Class", "3" = "3rd Class"))





# average age of survivors
df %>% group_by(Survived) %>%
  summarise(Average = mean(Age, na.rm=T)) %>% kable()


ggplot(data = df %>%
         group_by(Survived) %>%
         summarise(Average = mean(Age, na.rm = T)),
       mapping = aes(x = factor(Survived), 
                     y = Average)) +
  geom_col(aes(fill = factor (Survived)), 
           position = "dodge") +
  geom_text(aes(label = round(Average, 2), 
                y = Average / 2), 
            size = 3) +
  labs(x = "Survival Status",
       y = "Average Age",
       title = "Average Age by Survival Status") +
  scale_x_discrete(labels = c("0" = "Perished", "1" = "Survived")) +
  scale_fill_discrete(labels = c("0" = "Perished", "1" = "Survived"))





# average age for each class
df %>% group_by(Pclass) %>%
  summarise(Average = mean(Age, na.rm=T)) %>% kable()


ggplot(data = df %>%
         group_by(Pclass) %>%
         summarise(Average = mean(Age, na.rm = TRUE)),
       mapping = aes(x = factor(Pclass), 
                     y = Average)) +
  geom_col(aes(fill = factor(Pclass)), 
           position = "dodge") +
  geom_text(aes(label = round(Average, 2), 
                y = Average / 2), 
            size = 3) +
  labs(x = "Passenger Class",
       y = "Average Age",
       title = "Average Age by Passenger Class") +
  scale_fill_discrete(labels = c("1" = "1st Class", "2" = "2nd Class", "3" = "3rd Class"))




# distribution of passengers by age group and survival status
ggplot(data = df %>% mutate(Survived = ifelse(Survived == 1, "Survived", "Perished"),
                            Age_Group = cut(Age, breaks = seq(from = 0, to = 100, by = 5))) %>% 
         group_by(Survived, Age_Group) %>%
         tally() %>% na.omit(),
       mapping = aes(x = Age_Group, 
                     y = n, 
                     group = Survived)) +
  geom_col(aes(fill = Survived), 
           position = "dodge") +
  geom_text(aes(y = n/2 ,label = n),
            position = position_dodge(width=1),
            size = 4) +
  labs(x = "Age Group",
       y = "Number of Passengers",
       title = "Distribution of Passengers by Age Group and Survival Status")