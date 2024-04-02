Rprof(interval = 0.005)
start <- Sys.time()

library(tidyverse)
library(car)
library(faraway)
library(dplyr)
library(ggfortify)

#reading in data
#causal data shows when a UPC was on feature or on display 
#transaction data shows transactions with the UPCs by store
tran_classico <- read.csv("tran_classico.csv")
causal_clas <- read.csv("causal_clas.csv")

data.join <- left_join(tran_classico, causal_clas, by = c("store", "week"))

data.select <- data.join[, c(1:4, 6:7)]

data <- data.select %>%
  mutate(feature_desc = ifelse(is.na(feature_desc), "Not on Feature", feature_desc),
         display_desc = ifelse(is.na(display_desc), "Not on Display", display_desc))

#changing data types to factors needed for assignment
data$feature_desc = as.factor(data$feature_desc)
data$display_desc = as.factor(data$display_desc)

data$feature_desc = relevel(data$feature_desc, ref="Not on Feature")
data$display_desc = relevel(data$display_desc, ref = "Not on Display")

#creating column indicating whether or no the item is on Feature, creating a separate df for data that is not on display, for this part of the analysis we did not care about the "Feature" attribute
data$Tr = ifelse(data$feature_desc=="Not on Feature",0,1)
keep.data <- data %>% filter(display_desc == "Not on Display")

#taking stores average units sold when the item is not on display
store_avgs <- keep.data %>%
  filter(Tr == 0) %>%
  group_by(store) %>%
  summarise(avg_unit = mean(total_units, na.rm = TRUE)) %>% as.data.frame()

#separating data when the stores are on feature
store.week.features <- keep.data %>% filter(Tr ==1)

#separating data when the stores are not on feature
store.week.nofeatures <- keep.data %>% filter(Tr == 0)

#for loop that looks through and finds matching stores for the same week that was not on feature (used as a control store)
#within those matches picks out the closest match that has the smallest difference
result_df <- data.frame()
for (i in 1:nrow(store.week.features)) {
  matches <- store.week.nofeatures %>%
    filter(store != as.numeric(store.week.features[i, "store"]),
           week >= as.numeric(store.week.features[i, "week"]) - 6,
           week <= as.numeric(store.week.features[i, "week"]) + 6) %>%
    as.data.frame()  # Convert matches to a dataframe
  
  matches$diff = abs(matches$week - as.numeric(store.week.features[i,"week"]))
  
  closest_match <- matches %>%
    group_by(store) %>%
    summarize(diff = min(diff))
  
  joined_match <- closest_match %>%
    inner_join(matches,by = c("store","diff")) %>% as.data.frame() %>% dplyr::select(1,3,5)
  
  
  joined_match$treat_store = as.numeric(store.week.features[i,"store"])
  joined_match$treat_week = as.numeric(store.week.features[i,"week"])
  
  result_df <- bind_rows(result_df, joined_match)
}

#joining in the control stores average units
result_df.join = result_df %>%
  inner_join(store_avgs,by=c("store"))
result_df.join = result_df.join %>%
  rename(control_avg_units = avg_unit)

#joining in the treatment stores average units
result_df.join = result_df.join %>%
  inner_join(store_avgs,by=c("treat_store"="store"))
result_df.join = result_df.join %>%
  rename(treat_avg_units = avg_unit)

#finding the difference between treatment and control stores
result_df.join$diff = abs(result_df.join$control_avg_units - result_df.join$treat_avg_units)
#if a stores had more than one match this is finding the smallest match
final = result_df.join %>%
  group_by(treat_store,treat_week) %>%
  summarize(diff = min(diff)) %>% as.data.frame()

final.data = final %>%
  inner_join(result_df.join,by=c("treat_store","treat_week","diff")) %>% dplyr::select(1,2,4,5)

final.data.2 = final.data %>% distinct(treat_store, treat_week, .keep_all = TRUE)

Rprof(NULL)
summaryRprof()$by.self
Sys.time()-start
