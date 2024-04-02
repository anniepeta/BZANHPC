Rprof()
start <- Sys.time()

library(dplyr)

#read in data
tc_data <- read.csv("test_control.csv")
tc_data <- tc_data %>% filter(MAIN_ITEM_DESCRIPTION == 'P')

test_stores <- sample(unique(tc_data$LOCATION_ID),60)
test_stores <- tc_data %>% filter(LOCATION_ID %in% test_stores)


#loop to get squared values for January
for(j in 1:nrow(test_stores)){
  store_name <- test_stores$LOCATION_ID[j]
  store_name <- as.character(store_name)
  for(i in 1:nrow(tc_data)){
    tc_data$Jan_squared[i] <- (tc_data$January[i] - test_stores$January[j])^2
    tc_data$Feb_squared[i] <- (tc_data$February[i] - test_stores$February[j])^2
    tc_data$Mar_squared[i] <- (tc_data$March[i] - test_stores$March[j])^2
    tc_data$Apr_squared[i] <- (tc_data$April[i] - test_stores$April[j])^2
    tc_data$May_squared[i] <- (tc_data$May[i] - test_stores$May[j])^2
    tc_data$Jun_squared[i] <- (tc_data$June[i] - test_stores$June[j])^2
    tc_data$Jul_squared[i] <- (tc_data$July[i] - test_stores$July[j])^2
    tc_data$Aug_squared[i] <- (tc_data$August[i] - test_stores$August[j])^2
    tc_data$Sep_squared[i] <- (tc_data$September[i] - test_stores$September[j])^2
    tc_data$Oct_squared[i] <- (tc_data$October[i] - test_stores$October[j])^2
    tc_data$Nov_squared[i] <- (tc_data$November[i] - test_stores$November[j])^2
    tc_data$Dec_squared[i] <- (tc_data$December[i] - test_stores$December[j])^2
    
  }
  new_column_name <- paste0(store_name[1], "_Jan_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Jan_squared)
  new_column_name <- paste0(store_name[1], "_Feb_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Feb_squared)
  new_column_name <- paste0(store_name[1], "_Mar_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Mar_squared)
  new_column_name <- paste0(store_name[1], "_Apr_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Apr_squared)
  new_column_name <- paste0(store_name[1], "_May_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := May_squared)
  new_column_name <- paste0(store_name[1], "_Jun_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Jun_squared)
  new_column_name <- paste0(store_name[1], "_Jul_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Jul_squared)
  new_column_name <- paste0(store_name[1], "_Aug_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Aug_squared)
  new_column_name <- paste0(store_name[1], "_Sep_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Sep_squared)
  new_column_name <- paste0(store_name[1], "_Oct_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Oct_squared)
  new_column_name <- paste0(store_name[1], "_Nov_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Nov_squared)
  new_column_name <- paste0(store_name[1], "_Dec_squared")
  tc_data <- dplyr::rename(tc_data, !!new_column_name := Dec_squared)
}


for(i in 1:nrow(test_stores)){
  store_name <- test_stores$LOCATION_ID[i]
  store_name <- as.character(store_name)
  store_columns <- grepl(store_name,colnames(tc_data))
  store_columns[1] <- TRUE
  store_data <- tc_data[,store_columns]
  for(j in 1:nrow(store_data)){
    if(store_data$LOCATION_ID[j] != test_stores$LOCATION_ID[i]){
      store_data$total[j] <- store_data[j,1] + store_data[j,2] + store_data[j,3] + store_data[j,4] + store_data[j,5] + store_data[j,6] + store_data[j,7] + store_data[j,8] + store_data[j,9] + store_data[j,10] + store_data[j,11] + store_data[j,12] + store_data[j,13]
      store_data$euclidean[j] <- sqrt(store_data$total[j])
    }
  }
  control_store <- store_data$LOCATION_ID[which.min(store_data$euclidean)]
  control_store <- as.character(control_store)
  message <- paste("Test Store ", store_name, " assigned to Control Store ", control_store)
  print(message)
}

Rprof(NULL)
summaryRprof()$by.self
Sys.time()-start
