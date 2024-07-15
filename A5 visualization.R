# Function to install and load libraries
install_and_load =function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}
libraries = c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

data=read.csv('D:/Bootcamp VCU datasets/NSSO68.csv')
head(data)

dim(data)
unique(data$state_1)

df=data%>%filter(state_1=='GUJ')

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))


# Finding missing values
missing_info= colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
gujnew= df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean = function(column) {
  if (any(is.na(column))) {
    column[is.na(column)]=mean(column, na.rm = TRUE)
  }
  return(column)
}
gujnew$Meals_At_Home=impute_with_mean(gujnew$Meals_At_Home)


# Finding outliers and removing them
remove_outliers=function(df, column_name) {
  Q1 =quantile(df[[column_name]], 0.25)
  Q3 = quantile(df[[column_name]], 0.75)
  IQR = Q3 - Q1
  lower_threshold= Q1 - (1.5 * IQR)
  upper_threshold= Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns = c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  gujnew =remove_outliers(gujnew, col)
}

# Summarize consumption
gujnew$total_consumption =rowSums(gujnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption= function(group_col) {
  summary =gujnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary= summarize_consumption("District")
region_summary = summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping =c("1" = "Kachchh","2" = "Banas Kantha","3" = "Patan","4" = "Mahesana","5" = "Sabar Kantha","6" = "Gandhinagar","7" = "Ahmadabad", "8" = "Surendranagar","9" = "Rajkot","10"="Jamnagar","11"="Porbandar","12"="Junagadh","13"="Amreli","14"="Bhavnagar","15"="Anand","16"="Kheda","17"="Panch Mahals","18"="Dohad","19"="Vadodara","20"="Narmada","21"="Bharuch","22"="Surat","23"="The Dangs","24"="Navsari","25"="Valsad")
sector_mapping= c("2" = "URBAN", "1" = "RURAL")

gujnew$District =as.character(gujnew$District)
gujnew$Sector =as.character(gujnew$Sector)
gujnew$District= ifelse(gujnew$District %in% names(district_mapping), district_mapping[gujnew$District],gujnew$District)
gujnew$Sector =ifelse(gujnew$Sector %in% names(sector_mapping), sector_mapping[gujnew$Sector],gujnew$Sector)

View(gujnew)

hist(gujnew$total_consumption, breaks = 10, col = 'cyan', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Gujarat State")

GUJ_consumption= aggregate(total_consumption ~ District, data = gujnew, sum) 
View(GUJ_consumption)

#??barplot
barplot(GUJ_consumption$total_consumption, 
        names.arg = GUJ_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'magenta4', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) 


# b) Plot 'No of Meals per day' on the Gujarat state map using NSSO68.csv data

library(ggplot2) 
#install.packages("sf")
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

GUJ_consumption2= aggregate(No_of_Meals_per_day ~ District, data = gujnew, sum) 
View(GUJ_consumption2)

# Read the Gujarat GeoJSON file
data_map2 =st_read("C:/Users/C J LAKSHMY/Downloads/GUJARAT_DISTRICTS.geojson")
View(data_map2)
data_map2 = data_map2 %>% rename(District = dtname)

# Merge the GeoJSON data with the summarized consumption data
data_map_data2= merge(GUJ_consumption2, data_map2, by = "District")
View(data_map_data2)

# Plot the 'No_of_Meals_per_day' variable on the Gujarat map
ggplot(data_map_data2) + 
  geom_sf(aes(fill = No_of_Meals_per_day, geometry = geometry)) + 
  scale_fill_gradient(low = "aquamarine", high = "deeppink3") + 
  ggtitle("No of Meals per Day by District in Gujarat") +
  theme(legend.position = "bottom") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")


