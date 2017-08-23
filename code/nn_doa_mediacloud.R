# This file includes R code supporting our Media Cloud analysis of 
# the net neutrality Day of Action

# Setup 
library(readr)
library(stringr)
library(gtrendsR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(directlabels)
library(lubridate)
library(ggthemes)
library(scales)    
library(plotly)

setwd("/Users/jeffreyfossett/Files/research/net_neutrality_day_of_action/")

my_theme <- theme_fivethirtyeight() + 
  theme(legend.position = 'none', 
        legend.title = element_blank(), 
        legend.text = element_text(size = 15),
        title = element_text(size=25),
        axis.title = element_text(size=18), 
        axis.text = element_text(size=15,face="plain"))

# Read data exported from Media Cloud
# Source: https://docs.google.com/spreadsheets/d/1UKvsSji9UUorXGRdNb2uz6-T2BN9_yxMivFG3ygFkwQ/edit?usp=sharing
df <- read.csv("MediaCloud_NetNeutrality_Coverage.csv") # also available in GH

# Compute confidence intervals 
df %>% 
  mutate(mainstream_se = sqrt(Mainstream_share*(1-Mainstream_share)/Mainstream_Baseline), 
         techblog_se = sqrt(Tech_Blogs_Share*(1-Tech_Blogs_Share)/Tech_Blogs_Baseline), 
         mainstream_upper = Mainstream_share+1.96*mainstream_se,
         mainstream_lower = ifelse(Mainstream_share-1.96*mainstream_se < 0, 0, Mainstream_share-1.96*mainstream_se), 
         techblog_upper = Tech_Blogs_Share+1.96*techblog_se,
         techblog_lower = ifelse(Tech_Blogs_Share-1.96*techblog_se < 0, 0, Tech_Blogs_Share-1.96*techblog_se))->df

# Plot count of mainstream and tech blog sentences about net neutrality (not in post)
df %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, NN_Mainstream_Sentences, NN_Techblog_Sentences) %>% 
  gather(var, val, 2:3) %>% 
  ggplot(aes(x=Date, y = val, fill = var)) + 
  geom_bar(stat='identity') + 
  facet_grid(var~.) + 
  my_theme + 
  scale_colour_hue() + 
  ggtitle("Media Coverage of Net Neutrality") + 
  labs(subtitle='Count of sentences including "net neutrality" (daily; US Mainstream & Tech Media)') + 
  ylab("Count of Sentences") + 
  xlab("Date") 

# Plot total sentences in mainstream and tech blog collections
df %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, Mainstream_Baseline, Tech_Blogs_Baseline) %>% 
  gather(var, val, 2:3) %>% 
  ggplot(aes(x=Date, y = val, fill = var)) + 
  geom_bar(stat='identity') + 
  facet_grid(var~.) + 
  my_theme + 
  scale_colour_hue() + 
  ggtitle("Count of sentences in MediaCloud Database") + 
  labs(subtitle='Daily; US Mainstream & Tech Media') + 
  ylab("Count of Sentences") + 
  xlab("Date") 

# Plot share of sentences covering NN in mainstream media as bar plot
df %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, Mainstream_share, mainstream_lower, mainstream_upper) %>% 
  mutate(var = "all") %>% 
  ggplot(aes(x=Date, y = Mainstream_share, fill = var)) + 
  geom_bar(stat='identity') + 
  geom_errorbar(aes(ymin=mainstream_lower, ymax = mainstream_upper), color = 'black') + 
  my_theme + 
  scale_colour_hue() + 
  scale_y_continuous(labels = percent) + 
  ggtitle("Mainstream Media Coverage of Net Neutrality") + 
  labs(subtitle='Percent of sentences in collection including "net neutrality" (daily; US Mainstream Media)') + 
  ylab("Percent of sentences in collection") + 
  xlab("Date") 

# Plot share of sentences covering NN in mainstream media as line plot
df %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, Mainstream_share, mainstream_lower, mainstream_upper) %>% 
  mutate(var = "all") %>% 
  ggplot(aes(x=Date, y = Mainstream_share, colour = var)) + 
  geom_line(size = 1.4) + 
  geom_point(size=1.4) + 
  geom_errorbar(aes(ymin=mainstream_lower, ymax = mainstream_upper), color = 'black') + 
  my_theme + 
  scale_colour_hue() + 
  scale_y_continuous(labels = percent) + 
  ggtitle("Mainstream Media Coverage of Net Neutrality") + 
  labs(subtitle='Percent of sentences in collection including "net neutrality" (daily; US Mainstream Media)') + 
  ylab("Percent of sentences in collection") + 
  xlab("Date") 

# Plot share of sentences covering NN in mainstream AND tech media as bar plot
df %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, Mainstream_share, Tech_Blogs_Share, mainstream_lower, mainstream_upper, techblog_lower, techblog_upper) %>% 
  gather(collection, share, 2:3) %>% 
  mutate(upper = ifelse(collection == "Mainstream_share", mainstream_upper, techblog_upper), 
         lower = ifelse(collection == "Mainstream_share", mainstream_lower, techblog_lower)) %>% 
  select(Date, collection, share, upper,lower) %>%
  ggplot(aes(x=Date, y = share, fill = collection)) + 
  geom_bar(stat='identity', position = position_dodge()) + 
  geom_errorbar(aes(ymin=lower, ymax = upper), color = 'black', position = position_dodge()) + 
  my_theme + 
  scale_colour_hue() + 
  scale_y_continuous(labels = percent) + 
  ggtitle("Mainstream Media Coverage of Net Neutrality") + 
  labs(subtitle='Percent of sentences in collection including "net neutrality" (daily; US Mainstream Media)') + 
  ylab("Percent of sentences in collection") + 
  xlab("Date")

# Plot share of sentences covering NN in mainstream AND tech media as faceted bar plot
df %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, Mainstream_share, Tech_Blogs_Share, mainstream_lower, mainstream_upper, techblog_lower, techblog_upper) %>% 
  gather(collection, share, 2:3) %>% 
  mutate(upper = ifelse(collection == "Mainstream_share", mainstream_upper, techblog_upper), 
         lower = ifelse(collection == "Mainstream_share", mainstream_lower, techblog_lower)) %>% 
  select(Date, collection, share, upper,lower) %>%
  ggplot(aes(x=Date, y = share, fill = collection)) + 
  geom_bar(stat='identity') + 
  geom_errorbar(aes(ymin=lower, ymax = upper), color = 'black') + 
  facet_grid(collection~.) + 
  my_theme + 
  scale_colour_hue() + 
  scale_y_continuous(labels = percent) + 
  ggtitle("Mainstream & Tech Media Coverage of Net Neutrality") + 
  labs(subtitle='Percent of sentences in collection including "net neutrality" (daily)') + 
  ylab("Percent of sentences in collection") + 
  xlab("Date")

# Plot share of sentences covering NN in mainstream AND tech media as line plot
df %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, Mainstream_share, Tech_Blogs_Share, mainstream_lower, mainstream_upper, techblog_lower, techblog_upper) %>% 
  gather(collection, share, 2:3) %>% 
  mutate(upper = ifelse(collection == "Mainstream_share", mainstream_upper, techblog_upper), 
         lower = ifelse(collection == "Mainstream_share", mainstream_lower, techblog_lower)) %>% 
  select(Date, collection, share, upper,lower) %>%
  mutate(collection = factor(collection, labels = c("Mainstream", "Tech"))) %>%
  ggplot(aes(x=Date, y = share, colour = collection)) + 
  geom_line(size = 1.4) + 
  geom_point() + 
  geom_errorbar(aes(ymin=lower, ymax = upper), color = 'black') + 
  theme_fivethirtyeight() + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(), 
        legend.text = element_text(size = 15),
        title = element_text(size=25),
        axis.title = element_text(size=18), 
        axis.text = element_text(size=15,face="plain")) + 
  scale_colour_hue() + 
  scale_y_continuous(labels = percent) + 
  ggtitle("Mainstream & Tech Media Coverage of Net Neutrality") + 
  labs(subtitle='Percent of sentences in collection including "net neutrality" (daily)') + 
  ylab("Percent of sentences in collection") + 
  xlab("Date")

### Health Care Debate Comparison

# Adapted from data included here (including just relevant fields): 
# https://docs.google.com/spreadsheets/d/1UKvsSji9UUorXGRdNb2uz6-T2BN9_yxMivFG3ygFkwQ/edit?usp=sharing
df_hc <- read.csv("mediacloud_health_care_comparison_raw_data.csv") #also available in GH

# Compare share of sentences covering net neutrality vs. various health care terms 
df_hc %>% 
  filter(!is.na(net_neutrality_share)) %>% 
  gather(var, val, 2:5) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(var = factor(var, labels = c("health care", "healthcare", "net neutrality", "obamacare"))) %>% 
  mutate(var = factor(var, levels = c("net neutrality", "health care", "healthcare", "obamacare"))) %>% 
  ggplot(aes(x=Date, y = val, colour = var)) + 
  geom_line(size = 1.2) + 
  geom_point() + 
  theme_fivethirtyeight() + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(), 
        legend.text = element_text(size = 15),
        title = element_text(size=25),
        axis.title = element_text(size=18), 
        axis.text = element_text(size=15,face="plain")) + 
  scale_y_continuous(labels = percent) + 
  ggtitle("Media Coverage of Net Neutrality vs. Health Care Debate") + 
  labs(subtitle='Percent of sentences including term (daily; US Mainstream Media)') + 
  ylab("Percent of sentences") + 
  xlab("Date") 