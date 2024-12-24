# utils.R
source(here("utils","migration_functions.R"))
month <- lubridate::month 
# Date calculations

calculate_dates <- function(month, year) {
  date2 <- my(paste(month, year))
  month1 <- month.abb[(which(month.abb == month) - 2) %% 12 + 1]
  year1 <- ifelse(month == "Jan", as.numeric(year) - 1, year)
  date1 <- my(paste(month1, year1))
  
  list(date2 = date2, month1 = month1, year1 = year1, date1 = date1)
}

# Captions and colors
get_captions <- function() {
  list(
    caption_blurb = "USAID/LAC analysis of CBP data <br> All information is self-reported.",
    caption_blurb_dap = "USAID/LAC analysis of Migración Panama data <br> All information is self-reported"
  )
}

get_colors <- function() {
  list(main_color_1 = "#002F6C", main_color_2 = "#BA2D42")
}


# Helper function to convert month and year to a Date object
get_date <- function(month, year) {
  
}

# Updated function
setup_migration_datasets <- function(month, year, fy = F, date_csv_path = "fallback_data/latest_dates.csv") {
  # Define fallback datasets
  fallback_dar <- "fallback_data/fallback_dar.csv"
  fallback_mex <- "fallback_data/fallback_mex.csv"
  fallback_ret <- "fallback_data/fallback_ret.csv"
  fallback_swb <- "fallback_data/fallback_cbp.csv"
  fallback_cbp <- "fallback_data/fallback_cbp_all.csv"
  
  # Calculate the date to be checked for all datasets
  data_date <- as.Date(paste(year, month, "01", sep = "-"), "%Y-%b-%d")
  
  # Load the latest dates CSV or create an empty dataframe if it doesn't exist
  if (file.exists(date_csv_path)) {
    latest_dates <- read_csv(date_csv_path, show_col_types = FALSE)
  } else {
    latest_dates <- tibble(
      dataset = c("dar", "mex", "ret", "swb", "cbp_all"),
      latest_date = as.Date(NA)
    )
  }
  
  message("Starting data setup for migration datasets...")
  
  # Function to log errors
  log_error <- function(dataset_name, err) {
    message(paste("Error encountered while pulling", dataset_name, ":", err$message))
  }
  
  # Function to check if an update is needed based on the CSV
  should_update <- function(dataset_name, new_date) {
    latest_record <- latest_dates %>% filter(dataset == dataset_name)
    if (nrow(latest_record) == 0 || is.na(latest_record$latest_date)) {
      return(TRUE)
    }
    return(new_date > latest_record$latest_date)
  }
  
  # Initialize a list to keep track of datasets and max dates
  dataset_latest_dates <- list()
  
  # Pull and check Darien data
  dar <- tryCatch({
    if (should_update("dar", data_date)) {
      data <- pull_dar(year = year, month = month)
      write_csv(data, fallback_dar)
      
      # Update the latest date for this dataset
      dataset_latest_dates$dar <- max(data$date)
      data
    } else {
      message("Skipping Darien update as the data is up-to-date.")
      data <- read_csv(fallback_dar, show_col_types = FALSE)
      dataset_latest_dates$dar <- max(data$date)
      data
    }
  }, error = function(err) {
    log_error("Darien", err)
    read_csv(fallback_dar, show_col_types = FALSE)
  })
  
  # Pull and check Mexico data
  mex <- tryCatch({
    if (should_update("mex", data_date)) {
      data <- pull_mex_full(year = 2018:2024)
      write_csv(data, fallback_mex)
      
      dataset_latest_dates$mex <- max(data$date)
      data
    } else {
      message("Skipping Mexico update as the data is up-to-date.")
      data <- read_csv(fallback_mex, show_col_types = FALSE)
      dataset_latest_dates$mex <- max(data$date)
      data
    }
  }, error = function(err) {
    log_error("Mexico", err)
    read_csv(fallback_mex, show_col_types = FALSE)
  })
  
  # Pull and check MRF data
  ret <- tryCatch({
    if (should_update("ret", data_date)) {
      data <- pull_mrf(2018:2024) %>% rename(returns = encounter_count)
      write_csv(data, fallback_ret)
      
      dataset_latest_dates$ret <- max(data$date)
      data
    } else {
      message("Skipping returns update as the data is up-to-date.")
      data <- read_csv(fallback_ret, show_col_types = FALSE)
      dataset_latest_dates$ret <- max(data$date)
      data
    }
  }, error = function(err) {
    log_error("MRF", err)
    read_csv(fallback_ret, show_col_types = FALSE)
  })
  
  # Pull and check CBP Southwest Border data
  swb <- tryCatch({
    if (should_update("swb", data_date)) {
      data <- pull_cbp(month, year = 2024, swb = T,fy = fy)
      write_csv(data, fallback_swb)
      
      dataset_latest_dates$swb <- max(data$date)
      data
    } else {
      message("Skipping CBP Southwest Border update as the data is up-to-date.")
      data <- read_csv(fallback_swb, show_col_types = FALSE)
      dataset_latest_dates$swb <- max(data$date)
      data
    }
  }, error = function(err) {
    log_error("CBP Southwest Border", err)
    read_csv(fallback_swb, show_col_types = FALSE)
  })
  # Pull and check CBP All data
  cbp_all <- tryCatch({
    if (should_update("cbp_all", data_date)) {
      data <- pull_cbp(month, year = year,fy = fy,swb = F)
      write_csv(data, fallback_cbp)
      
      dataset_latest_dates$cbp_all <- max(data$date)
      data
    } else {
      message("Skipping CBP All update as the data is up-to-date.")
      data <- read_csv(fallback_cbp, show_col_types = FALSE)
      dataset_latest_dates$cbp_all <- max(data$date)
      data
    }
  }, error = function(err) {
    log_error("CBP All", err)
    read_csv(fallback_cbp, show_col_types = FALSE)
  })
  
  # Update the CSV with the new latest dates if any updates were made
  for (dataset_name in names(dataset_latest_dates)) {
    latest_dates <- latest_dates %>%
      mutate(latest_date = ifelse(dataset == dataset_name, dataset_latest_dates[[dataset_name]], latest_date))
  }
  
  write_csv(latest_dates, date_csv_path)
  
  message("Data setup completed successfully or with fallback datasets.")
  
  # Return all datasets and the list of latest dates
  list(
    dar = dar,
    mex = mex,
    ret = ret,
    swb = swb,
    cbp_all = cbp_all,
    latest_dates = latest_dates
  )
}

load_dates <- function(month2, year2 = 2024){
  date2 <- my(paste(month2, year2))
  month1 <- month.abb[(which(month.abb == month2) - 2) %% 12 + 1]
  year1 <- ifelse(month2 == "Jan", year2 |> as.numeric() - 1, year2)
  date1 <- my(paste(month1, year1))
  
  month2_long <- month.name[which(month.abb == month2)]
  month1_long <- month.name[(which(month.abb == month2) - 2) %% 12 + 1]
  
  
  year_3_ago <- as.character(as.numeric(year2) - 3)
  date_3_ago <- my(paste(month2, year_3_ago))
  
  return(
    list(
      month2 = month2,
      year2 = year2,
      date2 = date2,
      month1 = month1,
      year1 = year1,
      date1 = date1,
      month2_long = month2_long,
      month1_long = month1_long,
      date_3_ago = date_3_ago
    )
  )
}

load_lag_dates <- function(){
  datez <- read.csv(here("Migration Update", "fallback_data", "latest_dates.csv"))
  date2 <- min(datez$latest_date) %>% as.Date()
  year2 <- year(date2)
  month2 <- month(date2, label = TRUE,abbr = TRUE) %>% as.character()
  month1 <- month.abb[(which(month.abb == month2) - 2) %% 12 + 1]
  year1 <- ifelse(month2 == "Jan", year2 |> as.numeric() - 1, year2)
  date1 <- my(paste(month1, year1))
  
  month2_long <- month.name[which(month.abb == month2)]
  month1_long <- month.name[(which(month.abb == month2) - 2) %% 12 + 1]
  
  
  year_3_ago <- as.character(as.numeric(year2) - 3)
  date_3_ago <- my(paste(month2, year_3_ago))
  
  return(
    list(
      date2 = date2,
      month2 = month2,
      year2 = year2,
      month1 = month1,
      year1 = year1,
      date1 = date1,
      month2_long = month2_long,
      month1_long = month1_long,
      date_3_ago = date_3_ago
    )
  )
}

