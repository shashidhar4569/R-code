# librarires required
  install.packages("lubridate")
  install.packages("data.table")
  library(data.table)
  library(lubridate)

# Reading Data from Amazon URL provided in to python Environment for Analysis.
  url_address <- readline('Enter the URl of data you want to analyze: ')
  Output_filename <- readline('Enter desired name for output file: ')
  raw_data <- fread(url_address)
  
  # Give the names to four attributes as UserID, ItemID, Rating, Unix_timestamp
    colnames(raw_data) <- c("UserID", "ItemID", "Ratings", "Unix_timestamp")

# Converting the Data attributes to required format for Analysis purpose.
  # Transforming Unix Timestamp to Date and adding a column of name current_review.
    raw_data$date <- as.Date(as.POSIXct(raw_data$Unix_timestamp, origin = "1970-01-01"))
  # Finding a first review done on the item and adding a column of name first_review.
    first_review_date <- (setnames(aggregate(date ~ ItemID, raw_data, function(x)min(x)), c("ItemID","first_review_date")))
  # Merging the raw_data table and first_review_date table using ItemID.
    raw_data <- merge(raw_data, first_review_date, by = "ItemID")
  # Difference between the first_review and current_review in months and creating a column named moths_diff_review.
    # Function for calculating the difference between dates in months.
    function (begin, end) {
      mos1<-as.period(interval(ymd(begin),ymd(end)))
      mos<-mos1@year*12+mos1@month
      mos
    }
    raw_data$month_diff <- mos(raw_data$first_review_date, raw_data$date)

# creating table name:_generated with columns time_period(alias months_diff_review), frequency(count), distinct(unique no of items),
  # Average(frequency/count), cummulative_avg(sum of averages along the rows after making sure the time_period is in increasing order)
    month_frequency <- as.data.frame(table(raw_data$month_diff))
    colnames(month_frequency) <- c("month_diff", "frequency")
    distinc_count <- aggregate(data=raw_data, ItemID ~ month_diff, function(x) length(unique(x)))
    colnames(distinc_count)[2] <- c("count_dist")
  # Merging tables month_frequency and distinct_count by month_diff
    finished_data <- merge(month_frequency, distinc_count, by= 'month_diff')
    # Arranging the data in ascending order based on month_diff for cummulative_avg purpose
      finished_data <- finished_data[order(finished_data$month_diff),]
    # Average between frequency of items and distinct count of items in that months difference
      finished_data$average <- finished_data$frequency/finished_data$count_dist
    # Cummulative average
      finished_data$cummulative_avg <- cumsum(finished_data[, 4])
    
# Plotting a graph between time_period and cummulative_avg.
  plot(finished_data$month_diff, finished_data$cummulative_avg)
# write _generated file into csv format for further analysis and study.
  write.csv(finished_data, file = paste(Output_filename,".csv"))