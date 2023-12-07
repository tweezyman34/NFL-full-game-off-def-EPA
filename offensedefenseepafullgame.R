#Install and Load Packages
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
library(tidyverse)
library(nflfastR)
library(ggimage)

#Get NFL Logos
nfllogos <- read.csv("/Users/dylanwilkerson/Documents/CSV:XLSX/NFL Logos.csv")

#Pull Data
data <- load_pbp(2023)

#Drop NA's and get Offensive EPA
offdata <- data %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(posteam) %>%
  summarise(total_epaoff = sum(epa))

#Drop NA's and get Defensive EPA
defdata <- data %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(defteam) %>%
  summarise(total_epadef = sum(epa))

#Rename Columns
colnames(offdata)[1] = "team"
colnames(defdata)[1] = "team"

#Merge Offensive and Defensive Data
mergedata <- left_join(offdata, defdata, by = "team")

#Merge Complete Data with Logos
finaldata <- left_join(mergedata, nfllogos, by = "team")

#Calculate Mean EPA for Graph Line
meanepa<-mean(finaldata$total_epaoff)

#Create Plot- Adjust geom_text Items Accordingly
plot <- ggplot(finaldata, aes(total_epaoff, total_epadef)) +
  geom_image(aes(image = logo), size = .1) +
  geom_hline(yintercept = meanepa) +
  geom_vline(xintercept = meanepa) +
  geom_abline(intercept = 0, slope = -1) +
  labs(title = "EPA Earned/Allowed",
       subtitle = "Created by Dylan Wilkerson/@wilkersonadylan",
       x = "Total EPA Gained on Offense",
       y = "Total EPA Allowed on Defense") +
  theme_light() +
  geom_text(aes(x=-100, y=0, label="Bad Offense, Bad Defense"),
            size = 6) +
  geom_text(aes(x=-100, y=-100, label="Bad Offense, Good Defense"),
            size = 6) +
  geom_text(aes(x= 40, y=-100, label="Good Offense, Good Defense"),
            size = 6) +
  geom_text(aes(x= 40, y=50, label="Good Offense, Bad Defense"),
            size = 6)

#Print Plot
plot
