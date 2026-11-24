# load library
library(MASS)
# load pre-processed case data
source('cases-preprocess.r')
# read code data
code <- read_csv('/Users/georgiagrahek/Data sets/icd_codes.csv')
######
# clean code data
######

#find duplicates
code |>
  count(code, disease) |>
  filter(n > 1)

#remove duplicates
code <- code |>
  distinct()

######################
# start of exploration
# see what codes have high distances 
clean_distances |> arrange(desc(dst))

# plot cases per code by distance and add a robust linear model 
clean_distances |>
  ggplot(aes(x = cd_cases, y = dst)) +
  geom_point() +
  geom_smooth(method = rlm) +
  scale_y_log10() +
  scale_x_log10()

#create a rlm
dst_model <- 
  clean_distances |> 
  rlm(dst |> log() ~ cd_cases |> log(), data = _)

# define abnormal codes by largest residuals  (> 1.5)
abnormal <- dst_model |>
  # add values to table
  augment(clean_distances) |>
  # select useful columns
  select(code, cd_cases, dst, .resid) |>
  # filter for residuals greater than 1.5 (abnormal)
  filter(.resid > 1.5) |>
  # get only distinct values
  distinct()


#join the code table 
abnormal |>
  # use left join to add the "disease" column from the code table
  left_join(code, by = "code") |>
  # arrange by cd_cases in descending order
  arrange(desc(cd_cases))

#now we have 13 codes that have the most abnormal patterns

# look at one code pattern 
clean_distances |>
  filter(code == "X33") |>
  ggplot(aes(x = hour, y = cd_hr_cases)) +
  geom_line() + 
  labs(title = "Deaths by lightning per hour", y = "cases") + 
  scale_x_continuous(n.breaks = 24)

# plot one code against the pattern of all the cases
clean_distances |>
  filter(code == "X33") |>
  ggplot(aes(x = hour)) +
  geom_line(aes(y = cd_hr_props, color = "Lightning deaths")) +
  geom_line(aes(y = hr_props, color = "All deaths")) +
  scale_x_continuous(breaks = 0:23)

# faceted graphs 
clean_distances |>
  # semi join the tables so only the abnormal codes are shown
  semi_join(abnormal, by = "code") |>
  # left join by code 
  left_join(code, by = "code") |>
  # order the graghs by most cases per code
  arrange(desc(cd_cases)) |>
  # plot hour on the x axis
  ggplot(aes(x = hour)) +
  # add a line layer for the abnormal code and have the proportion fo that code per house on the y axis
  geom_line(aes(y = cd_hr_props, color = "Individual code")) +
  # add a line layer for all deaths and put hour proportion on the y axis
  geom_line(aes(y = hr_props, color = "All deaths")) +
  # facet the graphs by the abnormal codes
  facet_wrap(~str_wrap(disease, width = 25), scales = "free_y") +
  # add labels
  labs(
    # add title
    title = "Hourly Death Patterns for Abnormal ICD Codes",
    # add x axis title
    x = "Hour of day",
    # add y axis title
    y = "Proportion of deaths",
    # add legend title
    color = "Death category"
  )
