Rprof()
start <- Sys.time()

library(dplyr)

#read in data
tc_data <- read.csv("test_control.csv")
tc_data <- tc_data %>% filter(MAIN_ITEM_DESCRIPTION == 'P')

test_stores <- sample(unique(tc_data$LOCATION_ID),60)
test_stores <- tc_data %>% filter(LOCATION_ID %in% test_stores)

# Calculate squared differences
for (j in 1:nrow(test_stores)) {
  store_name <- as.character(test_stores$LOCATION_ID[j])
  for (month in c("January", "February", "March", "April", "May", "June", "July",
                  "August", "September", "October", "November", "December")) {
    tc_data[[paste0(store_name, "_", month, "_squared")]] <-
      (tc_data[[month]] - test_stores[[month]][j])^2
  }
}

# Find nearest control store for each test store
for (i in 1:nrow(test_stores)) {
  store_name <- as.character(test_stores$LOCATION_ID[i])
  store_columns <- c(1, grep(store_name, colnames(tc_data)))
  store_data <- tc_data[, store_columns, drop = FALSE]
  store_data$total <- rowSums(store_data[, -1])
  store_data$euclidean <- sqrt(store_data$total)
  store_data <- store_data[store_data$LOCATION_ID != test_stores$LOCATION_ID[i], ]
  control_store <- as.character(store_data$LOCATION_ID[which.min(store_data$euclidean)])
  message <- paste("Test Store", store_name, "assigned to Control Store", control_store)
  print(message)
}

Rprof(NULL)
summaryRprof()$by.self
print(Sys.time()-start)

