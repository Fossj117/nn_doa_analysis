# This file includes R code supporting our Google Trends analysis of 
# the net neutrality Day of Action

# Setup
library(readr)
library(stringr)
library(gtrendsR)
library(dplyr)
library(ggplot2)
library(directlabels)
library(lubridate)
library(ggthemes)
library(scales)    
library(plotly)

setwd("/Users/jeffreyfossett/Files/research/net_neutrality_day_of_action/")

# theme used for plots
my_theme <- theme_fivethirtyeight() + 
  theme(legend.position = 'none', 
        legend.title = element_blank(), 
        legend.text = element_text(size = 15),
        title = element_text(size=25),
        axis.title = element_text(size=18), 
        axis.text = element_text(size=15,face="plain"))

# short-term net neutrality trends data 
nn_trends_df <- gtrends(
  keyword=c("net neutrality"), 
  geo = c("US"), 
  time="2017-05-01 2017-07-20")

# plot trends data over time
nn_trends_df$interest_over_time %>%
  ggplot(aes(x=date, y = hits, colour = keyword, group = keyword)) + 
  geom_line(size=1.6) + 
  geom_vline(aes(xintercept = as.numeric(as.Date('2017-07-12'))), linetype = 'dashed', size = 1.3) +  
  my_theme + 
  scale_colour_hue() + 
  ggtitle("Day of Action Drove Major Increase in\nNet Neutrality Awareness") + 
  labs(subtitle='Relative Google Search Frequency for "net neutrality" (US Only)') + 
  ylab("Relative Search Frequency") + 
  xlab("Date") 

# pull data to compare NN interest to celebrities 
nn_trends_compare_celebs <- gtrends(
  keyword=c("net neutrality", "kanye west", "kim kardashian", "justin bieber"), 
  geo = c("US"), 
  time="2017-05-01 2017-07-20")

# plot celebrity comparison over time 
nn_trends_compare_celebs$interest_over_time %>%
  mutate(keyword = factor(keyword, levels = c("net neutrality", "kanye west", "kim kardashian", "justin bieber"))) %>% 
  ggplot(aes(x=date, y = hits, colour = keyword, group = keyword)) + 
  geom_line(size=1.6) + 
  my_theme + 
  scale_color_hue() + 
  geom_dl(aes(label = keyword), method = list(dl.trans(y = y + 0.15), "top.bumptwice", cex = 1.0)) + 
  ggtitle("For a Day, Net Neutrality was\nMore Popular than Kim, Kanye or Justin") + 
  labs(subtitle='Relative Google Search Frequency for term (US Only)') + 
  ylab("Relative Search Frequency") + 
  xlab("Date")

# pull data to compare NN interest to hot dog eating contest
nn_trends_compare_dogs <- gtrends(
  keyword=c("net neutrality", "hot dog eating"),
  geo = c("US"), 
  time="2017-05-01 2017-07-20")

# plot hot dog eat contest comparison over time 
nn_trends_compare_dogs$interest_over_time %>%
  mutate(keyword = factor(keyword, levels = c("net neutrality", "hot dog eating"))) %>% 
  ggplot(aes(x=date, y = hits, colour = keyword, group = keyword)) + 
  geom_line(size=1.6) + 
  my_theme + 
  scale_color_hue() + 
  ylim(0,105) + 
  geom_dl(aes(label = keyword), method = list(dl.trans(y = y + 0.15), "top.bumptwice", cex = 1.0)) + 
  ggtitle("Day of Action Drew About as Much Interest as\nNathan's Hot Dog Eating Contest") + 
  labs(subtitle='Relative Google Search Frequency for term (US Only)') + 
  ylab("Relative Search Frequency") + 
  xlab("Date")

# Pull long term net neutrality data (this section removed from final analysis)
nn_trends_long <- gtrends(
  keyword=c("net neutrality"), 
  geo = c("US"), 
  time="2014-01-01 2017-07-20")

# Plot long-term data over time 
nn_trends_long$interest_over_time %>%
  ggplot(aes(x=date, y = hits, colour = keyword, group = keyword)) + 
  geom_line(size=1.6) + 
  geom_vline(aes(xintercept = as.numeric(as.Date('2017-07-12'))), linetype = 'dashed', size = 1.3, color = ggthemes_data$fivethirtyeight["green"]) + 
  theme_fivethirtyeight() + 
  theme(legend.position = 'none', 
        legend.title = element_blank(), 
        legend.text = element_text(size = 15),
        title = element_text(size=25),
        axis.text.y=element_blank(), 
        axis.title = element_text(size=18), 
        axis.text = element_text(size=15,face="plain")) + 
  ggtitle("Public Interest in Net Neutrality Moments") + 
  labs(subtitle='Relative Google Search Volume for "net neturality" (US Only)') + 
  ylab("Relative Search Volume") + 
  xlab("Date") 

# Build plot of key debates in long-term debate
key_dates <- as.POSIXct(c('2014-06-01', 
                          '2014-11-09', 
                          '2015-02-22', 
                          '2017-05-07', 
                          '2017-07-09'))

date_names <- c('First Oliver Segment Airs\n(wk. of 6/1/14)',
                'Internet Slowdown Day\n(wk. of 11/9/14)', 
                '2015 FCC Decision to\nProtect Net Neutrality\n(wk. of 2/22/15)',
                'Second Oliver Segment Airs\n(wk. of 5/7/17)',
                'Net Neutrality "Day of Action"\n(wk. of 7/9/17)')

type <- c('Media', 'Activism', 'Policy', 'Media', 'Activism')

nn_trends_long$interest_over_time %>%
  filter(date %in% key_dates) %>% 
  mutate(names = date_names, 
         type = type) %>% 
  ggplot(aes(x=reorder(names, hits), y = hits, fill = type)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  theme(legend.position = 'right',
        legend.direction = 'vertical',
        legend.title = element_blank(), 
        legend.text = element_text(size = 15),
        title = element_text(size=25),
        axis.title = element_text(size=18), 
        axis.text = element_text(size=15,face="plain")) + 
  ggtitle("Public Interest in Net Neutrality Events") + 
  labs(subtitle='Relative Google Search Frequency for "net neturality" by event (US Only)') + 
  ylab("Relative Search Frequency") + 
  xlab("Event") 

