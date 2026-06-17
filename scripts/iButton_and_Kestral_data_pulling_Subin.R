#Code to pull out i button data from the .csv files, downloaded from device

#Packages
library(readxl)
library(tidyverse)
library(reshape)
library(lattice)
library(writexl)
library(dplyr)
library(lubridate)


# ibutton data
#Copy all the ibutton csv files in to a folder and give the folder path. (Only the csv files from iButton)

folder_path_ibutton <- "../data/raw/ibutton_2023"
ibutton <- list.files(folder_path_ibutton, pattern = ".csv$", full.names = TRUE)
list1 <- lapply(ibutton, read.csv, skip = 18, header = TRUE)

list2 <- lapply(ibutton, read.csv, header = FALSE, skip = 1, nrows = 1)


# Create an empty list to store the merged data frames
merged_list <- list()

# Loop through each data frame in list1 and list2
for (i in 1:length(list1)) {
  # Extract the iButton ID from the corresponding data frame in list2
  ibutton_id <- list2[[i]]$V1 
  
  # Add the iButton ID as a new column named 'ID' to the data frame from list1
  list1[[i]]$ID <- ibutton_id
  
  # Store the updated data frame in the merged_list
  merged_list[[i]] <- list1[[i]]
}


# Initialize merged_data with the first data frame (to keep the header)
merged_data <- merged_list[[1]]

# Loop through the rest of the files (starting from the second file)
for (i in 2:length(merged_list)) {
  # Add the iButton ID to the current data frame
  merged_list[[i]]$ID <- merged_list[[i]]$ID  # Ensure the ID column is included
  
  # Merge the current data frame with the merged_data (without the header)
  merged_data <- rbind(merged_data, merged_list[[i]][, names(merged_data)]) 
}


data_ibutton <- merged_data %>%
  mutate(
    Date.Time = dmy_hms(Date.Time),  # Convert to datetime
    Date = as.Date(Date.Time),        # Extract date
    Time = format(Date.Time, "%H:%M:%S"),  # Extract time
    Month = format(Date.Time, "%m"),   # Extract month
    Year = format(Date.Time, "%Y")     # Extract year
  )
data_ibutton$ID <- str_replace(data_ibutton$ID, "1-Wire/iButton Registration Number: ", "") 

unique(data_ibutton$ID) #check if there 58 unique IDs?


# We keep changing the ibuttons to different plots, so make a excel sheet containing the ibutton id and the plot were it installed/kept.
ibutton_deployment_list_2023<-read_excel("../data/metadata/iButtons info_2023.xlsx") 
ibutton_deployment_list_2023<-ibutton_deployment_list_2023 %>% 
  select(iButton_ID, Plot_ID)

data_with_plot_id <- data_ibutton %>%
  left_join(ibutton_deployment_list_2023, by = c("ID" = "iButton_ID"))  # Merge by matching ID and i Button_ID

data_with_plot_id<-data_with_plot_id %>% 
  select(Plot_ID, Unit, Value,Date, Time,Month,Year, ID)

#The Temperature and humidity files will be read together, Unit shows whether it is temperature or humidity.
#Extract temperature and humidity data separately
#Temperature data
Temperature_ibutton_2023<-data_with_plot_id %>% 
  filter(Unit=="C")

Temperature_ibutton_2023<-Temperature_ibutton_2023 %>% 
  dplyr::rename(Temperature_C = Value) %>%
  select(-Unit, Date)

#Relative humidity data
RH_ibutton_2023<-data_with_plot_id %>% 
  filter(Unit=="%RH")

RH_ibutton_2023<-RH_ibutton_2023 %>% 
  dplyr::rename(Relative_humidity = Value) %>%
  select(-Unit)


#Merging the Temperature & RH
ibutton_data_2023<-Temperature_ibutton_2023 %>%
  left_join(RH_ibutton_2023 %>% select(ID, Date, Time, Relative_humidity), 
            by = c("ID", "Date", "Time")) %>%
  select(Plot_ID,ID,Date, Time, Month, Year,Temperature_C,Relative_humidity)
# Few i button RH data are negative (April 2023) and >100 (March 2023). Please check it before doing any analysis

#To save
write_xlsx(ibutton_data_2023, "../data/processed/ibutton_data_2023.xlsx")






########################################################################################################
########################################################################################################



#Code to pull out Kestrel data from the .csv files, downloaded from device


#Packages
library(readxl)
library(tidyverse)
library(reshape)
library(lattice)
library(writexl)
library(dplyr) #its already in tidyverse, still load it separately; sometime rename wont work
library(lubridate)




#step 1. Download the Kestrel files (the default .csv format) in to one folder, and give the folder path below

folder_path_kestral <- "../data/raw/kestrel_2024" 
kest <- list.files(folder_path_kestral, pattern = ".csv$", full.names = TRUE)
list_1 <- lapply(kest, read.csv, skip = 3, header = TRUE) #list_1 has the data, has 13 columns
#The Kestrel csv file has 3 rows related to the device information , the actual data begins after 3rd row
list_2 <- lapply(kest, read.csv, header = FALSE, nrows = 1)
#The first row has the Kestrel Device name - took it as device ID, we are pulling this out and later merge with our data as a separate column.
#list_2 has one row and two columns, we want the information from second column (like this: D2 - 2815041)

#Step2: Merging the device ID with the data file, removing headers from other data files while joining
# Create an empty list to store the merged data frames
merged_list_ <- list()

# Loop through each data frame in list_1 and list_2
for (i in 1:length(list_1)) {
  # Extract the iButton ID from the corresponding data frame in list_2
  kestral_id <- list_2[[i]]$V2 #Here i put V2 because the second column has the device ID
  
  # Add the iButton ID as a new column named 'ID' to the data frame from list_1
  list_1[[i]]$ID <- kestral_id
  
  # Store the updated data frame in the merged_list
  merged_list_[[i]] <- list_1[[i]]
}


# Initialize merged_data with the first data frame (to keep the header)
merged_data_ <- merged_list_[[1]]

# Loop through the rest of the files (starting from the second file)
for (i in 2:length(merged_list_)) {
  # Add the Kestral ID to the current data frame
  merged_list_[[i]]$ID <- merged_list_[[i]]$ID  # Ensure the ID column is included
  
  # Merge the current data frame with the merged_data (without the header)
  merged_data_ <- rbind(merged_data_, merged_list_[[i]][, names(merged_data_)]) 
}


# Step3: Change the Date/Time format, and removing the characters before device ID

merged_data_$Date_Time <- strptime(merged_data_$FORMATTED.DATE_TIME, format = "%Y-%m-%d %I:%M:%S %p")
merged_data_$Date <- as.Date(merged_data_$Date_Time)
merged_data_$Time <- format(merged_data_$Date_Time, "%H:%M:%S")
merged_data_$Month <- format(merged_data_$Date_Time, "%m")
merged_data_$Year <- format(merged_data_$Date_Time, "%Y")

#Do this check to ensure all the ibutton data are included
unique(merged_data_$ID) #check if there 28 unique IDs?

merged_data_<-merged_data_ %>%
  select(Temperature, Relative.Humidity, Date, Time, Month, Year, ID)
merged_data_ <- merged_data_[-1, ]#The first row has units of temperature and RH, removing it

merged_data_$Temperature<-as.numeric(merged_data_$Temperature) #By default it will be character variable we have to change it
merged_data_$Relative.Humidity<-as.numeric(merged_data_$Relative.Humidity) #By default it will be character variable we have to change it 
merged_data_$Temperature_C <- (merged_data_$Temperature - 32) * 5/9 # Changing temperature from Fahrenheit to degree Celsius

#Remove the temperature column having Fahrenheit values
merged_data_<-merged_data_ %>% 
  select(-Temperature)
#Remove the letters/symbol before the device ID, one device got # instead of "D2 -"
merged_data_$ID_clean <- str_remove_all(merged_data_$ID, "D2 - |#")

#Removing the ID column
Kestrel_data<-merged_data_ %>% 
  select(-ID) %>% 
  dplyr::rename(ID = ID_clean)

#Step 4: Incorporate PlotID information
# locate the excel/csv with the plot_ID and Data logger ID
Kestrel_deployment_list_2024<-read_excel("../data/metadata/Kestrel info_2024.xlsx") 
Kestrel_deployment_list_2024<-Kestrel_deployment_list_2024 %>% 
  select(Kestrel_ID,Plot_ID) #check the column headings
Kestrel_deployment_list_2024$Kestrel_ID<-as.character(Kestrel_deployment_list_2024$Kestrel_ID)

data_with_plot_id <- Kestrel_data %>%
  left_join(Kestrel_deployment_list_2024, by = c("ID" = "Kestrel_ID"))  # Merge by matching ID and i Button_ID

data_with_plot_id<-data_with_plot_id %>% 
  select(Plot_ID, Temperature_C, Relative.Humidity,Date,Time,Month,Year,ID)

#Optional, i have done this because there are few entries in 2000, 2023 and 2025- the data file belongs to 2024 season
### Important: Remove the 2025 if you are working with 2025 kestrel data

Kestrel_final<-data_with_plot_id %>% 
  filter(!Year %in% c(2000,2023,2025))
Kestrel_final <- na.omit(Kestrel_final)# Do this to remove the 1st row after the heads (°F, %), these characters became NAs after converted in to numerical variables
Kestrel_final<-Kestrel_final %>% 
  dplyr::rename(Relative_humidity= Relative.Humidity)
Kestrel_final<-Kestrel_final %>% 
  select(Plot_ID,ID,Date,Time,Month,Year,Temperature_C,Relative_humidity)
summary(Kestrel_final)
# Step 5: Save the data files
#Kestrel_Temperature_RH_data
write_xlsx(Kestrel_final, "../data/processed/Kestrel_Temp_Humidity_data_2024_subin1.xlsx")




########################################################################################################
########################################################################################################

# To merge i button and Kestral data files together
# Add one column named Equipment in each data frames
ibutton_data_2023$Equipment <- "ibutton"
Kestrel_final$Equipment <- "kestrel"

# Combine the two data frames
microclimate_data <- rbind(ibutton_data_2023,ibutton_data_2024, Kestrel_final)
microclimate_data<-microclimate_data %>% 
  select(Equipment,Plot_ID,ID,Date,Time,Month,Year,Temperature_C,Relative_humidity)

########################################################################################################
########################################################################################################

# Summary

# Time coverage per plot
Time_coverage<-microclimate_data %>%
  group_by(Plot_ID) %>%
  summarise(Start_date = min(Date),
    End_date = max(Date))

# Mean and SD of temperature and humidity by Plot_ID and month
plot_month_summary <- microclimate_data %>%
  group_by(Plot_ID, Month) %>%
  summarise(Min_monthly_Temp = min(Temperature_C),
    Mean_monthly_Temp = mean(Temperature_C, na.rm = TRUE),
    Max_monthly_Temp = max(Temperature_C),
    Min_Monthly_RH = min(Relative_humidity),
    Mean_monthly_RH = mean(Relative_humidity, na.rm = TRUE),
    Max_monthly_RH = max(Relative_humidity),
    No_of_Observations = n()) %>%
ungroup()

