## RLionheart
## 21-0771-003
## Shoreline Conservation Areas, Washington State Parks

## Allows for profile selection. "prof" includes all profiles, 
## "prof_N" allows for specific profile selection. 
profile.pattern <- "prof"

## List and order all files
profile.names <- list.files(path = "data_raw/profiles_1617/", 
                            pattern = profile.pattern)
full.names <- unique(str_extract(profile.names, "[^_]*_[^_]*"))
numbers.only <- unique(sub("^[^_]*_([^_]*).*", "\\1", profile.names)) %>%
  as.numeric() %>%
  sort()

## Print description of files and ensure none are missing.
print(paste("Total number of files:", length(profile.names)))
print("Unique numbers of profiles:")
print(numbers.only)
print("Missing profiles from sequential data:")
setdiff(1:54, numbers.only)

## Import csv with geographic profile locations
profiles.w.location <- read_csv("data_raw/NANOOS_WCEHA_validation.csv",
                      col_names = c("profile", "WCEHA", "location", "notes"),
                      col_select = c(-WCEHA),
                      skip = 1,
                      show_col_types = FALSE)

sequence <- profiles.w.location$profile 
seq2 <- min(sequence, na.rm = TRUE):max(sequence, na.rm = TRUE)
missing <- seq2[!seq2 %in% sequence]

write.csv(profiles.w.location, "data_secondary/profiles_with_location.csv", 
          row.names = FALSE)
