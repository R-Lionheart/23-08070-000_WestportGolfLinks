## This script imports the Profiles for Erosion csv and connects the imported
## xyz profiles .out files to the BasePoint information, and each profile's 
## known geographic location. 

source("scripts/src/import_profiles.R")

## Import erosion file for Base Point data
mhhw.info <- read_csv("data_raw/ProfilesForErosion.csv",
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y",
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"),
                            col_select = c("profile", "MHHW"),
                            skip = 3, show_col_types = FALSE) %>%
  drop_na()

## Import BasePoint data
profile.erosion <- read_csv("data_raw/NANOOS_Profile_Ref_Points.csv",
                            col_names = c("profile", "BasePoint_Y", "BasePoint_X"),
                            skip = 1,
                            show_col_types = FALSE) 

wash.data <- read_csv("data_raw/NANOOS_WCEHA_validation.csv",
                      col_names = c("profile", "WCEHA", "Park", "notes"),
                      col_select = c("profile", "Park"),
                      skip = 1,
                      show_col_types = FALSE)

## Combine
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  left_join(wash.data, by = "profile") %>%
  left_join(mhhw.info, by = "profile") %>%
  select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z) %>%
  mutate(MHHW = ifelse(is.na(MHHW), 2.31, MHHW)) %>%
  filter(profile %in% str_extract_all(profile.pattern,"\\(?[0-9,.]+\\)?")[[1]]) %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22")))
