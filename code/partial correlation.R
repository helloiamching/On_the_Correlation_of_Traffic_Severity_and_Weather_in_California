stationary_cal <- read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/df_ca_timeselected_output.csv")
cleaned_stationary_cal <- na.omit(stationary_cal)
unique_county = c("Solano","Alameda","Contra Costa","Santa Clara","San Francisco","Sonoma","Sacramento","San Mateo","Yolo","Placer","Nevada","El Dorado","Marin","San Joaquin","Mendocino","Lake",
                  "Napa","Santa Cruz","Monterey","Stanislaus","Calaveras","Amador","San Benito","Merced","Sutter","Fresno","Butte","Colusa","Yuba","Glenn","Sierra","Tuolumne","Santa Barbara","Ventura","Los Angeles","Orange",
                  "Riverside","San Diego","San Bernardino","Trinity","Humboldt","Tulare","Kern","Madera","Shasta","San Luis Obispo","Lassen","Kings","Siskiyou","Mono","Tehama","Mariposa","Plumas","Inyo","Modoc","Imperial","Del Norte","Alpine")



# Assuming your data frame is named 'stationary_cal' and contains the relevant columns
# Initialize an empty matrix to store the first rows
first_row_matrix <- matrix(nrow = 0, ncol = ncol(weather_corr) + 1)
colnames(first_row_matrix) <- c("County", colnames(weather_corr))

# Iterate through each county
for (i in 1:58) {
  current_county <- unique_county[i]
  selected_counties <- cleaned_stationary_cal[cleaned_stationary_cal$County %in% current_county, ]
  weather_variables <- selected_counties[, c("Severity", "Temperature.F.", "Wind_Chill.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.", "Precipitation.in.")]
  weather_corr <- cor(weather_variables, method = "pearson")
  
  # Append the first row of the correlation matrix to the matrix
  first_row_matrix <- rbind(first_row_matrix, c(current_county, weather_corr[1, ]))
}

# Print the resulting matrix
cleaned_first_row_matrix <- na.omit(first_row_matrix)

print(first_row_matrix)

csv_file_path <- "/Users/wang.c/Desktop/112-1 NCCU/lfd/corr_county_time_selected.csv"
write.csv(first_row_matrix, file = csv_file_path, row.names = FALSE)
cat("Matrix saved to:", csv_file_path, "\n")

