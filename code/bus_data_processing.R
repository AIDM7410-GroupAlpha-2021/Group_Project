######################################################
#######$$AIDM7410 Group Project - Group Alpha#########
######################################################

#This file is mainly for:
#1. Collect data from data.gov
#2. Transform data for data analysis and visualization
#3. Export the processed data into RData format




#1.Load required libraries----
library(jsonlite) #for JSON handling
library(httr) #Get JSON
library(readr) #for writing and reading CSV with encoding UTF-8

library(magrittr) #for pipe
library(dplyr) #for dataframe manipulation
library(tidyr) #for NA handling
library(lubridate) #for date processing
library(data.table) #for data processing


#2.Random sampling 120 bus stops to investigate----
#Get Stop ID via API, prepare for extracting ETA at stop level
stopList <- GET("https://data.etabus.gov.hk/v1/transport/kmb/stop")
stopList_UTF8 <- rawToChar(stopList$content)
Encoding(stopList_UTF8) <- 'UTF-8'
stopListData <- fromJSON(stopList_UTF8)
df_stopList <- data.frame(stopListData)

#Set seed for random selection of 120 bus stops
set.seed(7410)
#Sampling
df_stopList_120 <- df_stopList[sample(nrow(df_stopList), 120), ]
df_stopList_120$data.lat <- as.numeric(df_stopList_120$data.lat)
df_stopList_120$data.long <- as.numeric(df_stopList_120$data.long)

#Export the 120 sample bus stations info as CSV for transparency to public
write_csv(df_stopList_120,"../data/120_Sample_Bus_Stations.csv")



#3.Collect data for 120 bus stops----
#Get ETA of the shortlisted bus stops for 30 times, around 1 min interval between
#Note that the below code is run at 
#1. 2021-12-04 22:15 to 22:45 to collect non rush hour data
#2. 2021-12-06 18:00 to 18:30 to collect rush hour data

#Below data collection codes are put as comments as they will not be used anymore
#df_stopETA <- NULL
#for (i in 1:30){
#  for (busStop in df_stopList_120$data.stop){
#    path <- paste0("https://data.etabus.gov.hk/v1/transport/kmb/stop-eta/", busStop)
#    data.stop <- busStop
#    stopETA <- GET(path)
#    stopETA_UTF8 <- rawToChar(stopETA$content)
#    Encoding(stopETA_UTF8) <- 'UTF-8'
#    stopETAData <- fromJSON(stopETA_UTF8)
#    df_stopETA <- rbind(df_stopETA, data.frame(stopETAData, data.stop))
#    Sys.sleep(0.5)
#  }
#}

#Store non rush hour raw data into csv
#write_csv(df_stopETA,"../data/ETA_20211204_2215.csv")
#write_csv(df_stopETA,"../data/ETA_20211206_1800.csv")


#4.Data processing and cleansing----
#4.1 Non rush hr data processing----
#Read non rush hr data from CSV

ETA_NonRushHr_Raw <- read_csv("../data/ETA_20211204_2215.csv")

ETA_NonRushHr <- copy(ETA_NonRushHr_Raw) %>%
  select(-data.rmk_tc, -data.rmk_sc, -data.rmk_en) %>%
  arrange(data.stop, data.route, data.dir, generated_timestamp, data.eta_seq) %>%
  spread(data.eta_seq, data.eta) %>%
  rename(eta_1=`1`, eta_2=`2`, eta_3=`3`) %>%
  select(data.stop, generated_timestamp, data.route, eta_1:eta_3, data.dir:data.data_timestamp) %>%
  filter(!is.na(eta_1)) %>%
  mutate(previous_eta_1 = ifelse(lag(data.stop)==data.stop & lag(data.route)==data.route, lag(eta_1), eta_1)) %>%
  mutate(previous_eta_2 = ifelse(lag(data.stop)==data.stop & lag(data.route)==data.route, lag(eta_2), eta_2))

#Datetime conversion
ETA_NonRushHr$generated_timestamp <- as_datetime(ETA_NonRushHr$generated_timestamp, tz='Asia/Taipei')
ETA_NonRushHr$eta_1 <- as_datetime(ETA_NonRushHr$eta_1, tz='Asia/Taipei')
ETA_NonRushHr$eta_2 <- as_datetime(ETA_NonRushHr$eta_2, tz='Asia/Taipei')
ETA_NonRushHr$eta_3 <- as_datetime(ETA_NonRushHr$eta_3, tz='Asia/Taipei')
ETA_NonRushHr$previous_eta_1 <- as_datetime(ETA_NonRushHr$previous_eta_1, tz='Asia/Taipei')
ETA_NonRushHr$previous_eta_2 <- as_datetime(ETA_NonRushHr$previous_eta_2, tz='Asia/Taipei')

#Create the delay flag and calculate seconds of delay 
ETA_NonRushHr <- ETA_NonRushHr %>%
  mutate(is_delay = ifelse(
    previous_eta_1>=eta_1, "no delay", ifelse(
      abs(eta_1-previous_eta_1)>abs(eta_1-previous_eta_2), "no delay", "delay"))) %>%
  replace_na(list(is_delay = "no delay")) %>%
  mutate(delay_seconds = ifelse(is_delay=="delay", eta_1 - previous_eta_1, 0)) %>%
  select(data.stop:eta_3, previous_eta_1, previous_eta_2, is_delay, delay_seconds, data.dir:data.data_timestamp)

save(ETA_NonRushHr, file = "../output/shiny/ETA_NonRushHr.RData")


#4.2 Rush hr data processing----
ETA_RushHr_Raw <- read_csv("../data/ETA_20211206_1800.csv")

ETA_RushHr <- copy(ETA_RushHr_Raw) %>%
  select(-data.rmk_tc, -data.rmk_sc, -data.rmk_en) %>%
  arrange(data.stop, data.route, data.dir, generated_timestamp, data.eta_seq) %>%
  spread(data.eta_seq, data.eta) %>%
  rename(eta_1=`1`, eta_2=`2`, eta_3=`3`) %>%
  select(data.stop, generated_timestamp, data.route, eta_1:eta_3, data.dir:data.data_timestamp) %>%
  filter(!is.na(eta_1)) %>%
  mutate(previous_eta_1 = ifelse(lag(data.stop)==data.stop & lag(data.route)==data.route, lag(eta_1), eta_1)) %>%
  mutate(previous_eta_2 = ifelse(lag(data.stop)==data.stop & lag(data.route)==data.route, lag(eta_2), eta_2))

#Datetime conversion
ETA_RushHr$generated_timestamp <- as_datetime(ETA_RushHr$generated_timestamp, tz='Asia/Taipei')
ETA_RushHr$eta_1 <- as_datetime(ETA_RushHr$eta_1, tz='Asia/Taipei')
ETA_RushHr$eta_2 <- as_datetime(ETA_RushHr$eta_2, tz='Asia/Taipei')
ETA_RushHr$eta_3 <- as_datetime(ETA_RushHr$eta_3, tz='Asia/Taipei')
ETA_RushHr$previous_eta_1 <- as_datetime(ETA_RushHr$previous_eta_1, tz='Asia/Taipei')
ETA_RushHr$previous_eta_2 <- as_datetime(ETA_RushHr$previous_eta_2, tz='Asia/Taipei')

#Create the delay flag and calculate seconds of delay 
ETA_RushHr <- ETA_RushHr %>%
  mutate(is_delay = ifelse(
    previous_eta_1>=eta_1, "no delay", ifelse(
      abs(eta_1-previous_eta_1)>abs(eta_1-previous_eta_2), "no delay", "delay"))) %>%
  replace_na(list(is_delay = "no delay")) %>%
  mutate(delay_seconds = ifelse(is_delay=="delay", eta_1 - previous_eta_1, 0)) %>%
  select(data.stop:eta_3, previous_eta_1, previous_eta_2, is_delay, delay_seconds, data.dir:data.data_timestamp)

save(ETA_RushHr, file = "../output/shiny/ETA_RushHr.RData")

#Data for analysis aggregate overall
ETA_NonRushHr_Summary <- copy(ETA_NonRushHr) %>%
  summarise(
    NRH.count = n(), 
    NRH.frequency_of_delay = sum(is_delay=="delay"), 
    NRH.percentage_of_delay = round(sum(is_delay=="delay")/n()*100, 1), 
    NRH.average_delay = round(sum(delay_seconds)/sum(is_delay=="delay"),1), 
    NRH.maximum_delay = max(delay_seconds))

ETA_RushHr_Summary <- copy(ETA_RushHr) %>%
  summarise(
    RH.count = n(), 
    RH.frequency_of_delay = sum(is_delay=="delay"), 
    RH.percentage_of_delay = round(sum(is_delay=="delay")/n()*100, 1), 
    RH.average_delay = round(sum(delay_seconds)/sum(is_delay=="delay"),1), 
    RH.maximum_delay = max(delay_seconds))

Overall_Summary <- merge(ETA_NonRushHr_Summary, ETA_RushHr_Summary)
Overall_Summary
save(Overall_Summary, file="../output/shiny/Overall_Summary.RData")


#Data for analysis at bus station level
ETA_NonRushHr_Summary_BS <- copy(ETA_NonRushHr) %>%
  group_by(data.stop) %>%
  summarise(
    NRH.count = n(), 
    NRH.frequency_of_delay = sum(is_delay=="delay"), 
    NRH.percentage_of_delay = round(sum(is_delay=="delay")/n()*100), 
    NRH.average_delay = round(sum(delay_seconds)/sum(is_delay=="delay")), 
    NRH.maximum_delay = max(delay_seconds))

ETA_NonRushHr_Summary_BS[is.na(ETA_NonRushHr_Summary_BS)] <- 0

ETA_RushHr_Summary_BS <- copy(ETA_RushHr) %>%
  group_by(data.stop) %>%
  summarise(
    RH.count = n(), 
    RH.frequency_of_delay = sum(is_delay=="delay"), 
    RH.percentage_of_delay = round(sum(is_delay=="delay")/n()*100), 
    RH.average_delay = round(sum(delay_seconds)/sum(is_delay=="delay")), 
    RH.maximum_delay = max(delay_seconds))

ETA_RushHr_Summary_BS[is.na(ETA_RushHr_Summary_BS)] <- 0

Station_ETA_Summary <- merge(ETA_NonRushHr_Summary_BS, ETA_RushHr_Summary_BS, all = TRUE) %>%
  merge(df_stopList_120) %>%
  select(data.stop, data.name_en:data.long, NRH.count:RH.maximum_delay)

Station_ETA_Summary

save(Station_ETA_Summary, file="../output/shiny/Station_ETA_Summary.RData")