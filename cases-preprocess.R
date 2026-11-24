# load library
library(tidyverse)

# read data
clean_distances <- read_csv('/Users/georgiagrahek/Data sets/death_records.txt')|>
  filter(!is.na(hour))|>
  # calculate total number of cases and create a new column called "total_cases"
  mutate(total_cases = n()) |>
  
  # group by code
  group_by(code) |>
  
  # add a column that contains the total number of cases per code
  mutate(cd_cases = n()) |>
  
  # group cases by hour
  group_by(hour) |>
  
  # calculate the proportion of cases per hour and create a new column named "hour props"
  mutate(hr_props = n() / total_cases) |>
  
  # group cases by code and hour
  group_by(code, hour) |>
  
  # count cases for each code and hour and create a new column for it
  mutate(cd_hr_cases = n()) |>
  
  # group cases by code
  group_by(code) |>
  
  # calculate the proportion of each code in each hour
  mutate(cd_hr_props = cd_hr_cases / n()) |>
  
  # calculate squared difference from the overall baseline
  mutate(sq_diff = (cd_hr_props - hr_props)^2) |>
  
  # calculate the average squared difference for each code
  mutate(
    dst = sum(sq_diff / cd_hr_cases) /
      sum(1 / cd_hr_cases)
  ) |>
  # remove unneeded columns
  select(-year,-month,-day,-sq_diff) |>
  # remove repeated rows
  distinct()

