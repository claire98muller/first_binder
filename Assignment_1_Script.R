library(tidyverse)

##Entering the data 
raw_data <- read.csv("F:/assignment_1_data (1)1.csv")

head(raw_data)

##renaming each condition

#Renaming Condition 1 to ExpectNum_TargetNum
names(raw_data)[names(raw_data)=="Condition1"] <- "ExpectNum_TargetNum"

#Renaming Condition 2 to ExpectNum_TargetLet
names(raw_data)[names(raw_data)=="Condition2"] <- "ExpectNum_TargetLet"

#Renaming Condition 3 to ExpectLet_TargetNum
names(raw_data)[names(raw_data)=="Condition3"] <- "ExpectLet_TargetNum"

#Renaming Condition 4 to ExpectLet_TargetLet
names(raw_data)[names(raw_data)=="Condition4"] <- "ExpectLet_TargetLet"

#viewing the newly named data 
head(raw_data)

## Creating a column for condition and RT

longer_data <- raw_data %>%
  pivot_longer(cols = c(ExpectNum_TargetNum, ExpectNum_TargetLet,
                        ExpectLet_TargetNum, ExpectLet_TargetLet), names_to = "Condition", values_to ="RT")

head(longer_data)

## Separating the condition column

tidied_data <- longer_data %>%
  separate(col = "Condition", into = c("Expectation","Target"), sep = "_")

head(tidied_data)

## changing the characteristic of each column

tidied_data_2 <- tidied_data %>%
  mutate(Expectation = factor(Expectation), Target = factor(Target))

head(tidied_data_2)

## average reaction time for each participant 

participant_mean_RT <- tidied_data_2 %>%
  group_by(ID) %>%
  summarise(mean_RT = mean(RT)) %>%
  as.data.frame()

#condition 1
tidied_data_2 %>%
  filter(Expectation == "ExpectNum", Target == "TargetNum") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT),
            median_RT = median(RT)) %>%
  as.data.frame()

#condition 2
tidied_data_2 %>%
  filter(Expectation == "ExpectNum", Target == "TargetLet") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT),
            median_RT = median(RT)) %>%
  as.data.frame()

#condition 3
tidied_data_2 %>%
  filter(Expectation == "ExpectLet", Target == "TargetNum") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT),
            median_RT = median(RT)) %>%
  as.data.frame()

#condition 4
tidied_data_2 %>%
  filter(Expectation == "ExpectLet", Target == "TargetLet") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT),
            median_RT = median(RT)) %>%
  as.data.frame()

## visualisation of distribution of reaction time 

participant_mean_RT %>%
  ggplot(aes(x = mean_RT, y =..density..)) +
  geom_histogram(binwidth = 5, fill = "grey") +
  geom_density() +
  labs(title = "Distribution of Average Reaction Time for Participants", 
       x = "Average Reaction Time", 
       y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 13))


##visualisation of reaction time for each condition 

tidied_data_2$ExpectationTarget <- interaction(tidied_data_2$Expectation, 
                                               tidied_data_2$Target)

#boxplot
ggplot(aes(y = RT, x = ExpectationTarget), data = tidied_data_2) +
  geom_boxplot() +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Average Reaction for Each Condition", 
       x = "Condition", 
       y = "Reaction Time")


#boxplot with points
ggplot(aes(y = RT, x = ExpectationTarget), data = tidied_data_2) +
  geom_boxplot() +
  geom_jitter(width = .2, alpha = .75, size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust = 0.5)) +
  theme(text = element_text(size = 13)) +
  labs(title = "Average Reaction for Each Condition", 
       x = "Condition", 
       y = "Reaction Time")

#violin
tidied_data_2 %>%
  ggplot(aes(y = RT, x = ExpectationTarget, colour = ExpectationTarget)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha =.5) +
  theme(text = element_text(size = 13)) +
  theme_minimal() +
  guides(colour = FALSE) +
  labs(title = "Average Reaction for Each Condition", 
       x = "Condition", 
       y = "Reaction Time")
