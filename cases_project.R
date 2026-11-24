# load library
library(MASS)
# load pre-processed case data
source('cases-preprocess.r')
# read code data
code <- read_csv('/Users/georgiagrahek/Data sets/icd_codes.csv')
view(code)

#CLASS CODE
#########

clean_distances |> 
  ggplot(aes(cd_cases, dst)) +
  geom_point() +
  geom_smooth(method = rlm) +
  scale_y_log10() +
  scale_x_log10() 

library(broom)

# create robust model distances ~ code cases
# use log to see residuals better
dst_model <- 
  clean_distances |> 
  rlm(dst |> log() ~ cd_cases |> log(), data = _)

# see fitted values on the model line
dst_model |> 
  augment(clean_distances)|>
  # remove columns to view new info
  select(-hr_props,-total_cases)

# plot residuals against cases per code
dst_model |> 
  augment(clean_distances) |> 
  ggplot() +
  geom_point(aes(cd_cases, .resid)) +
  scale_x_log10()

# plot distance against cases per code and add a robust linear model line
clean_distances |> 
  ggplot(aes(cd_cases, dst)) +
  geom_point() +
  geom_smooth(method = rlm) +
  scale_y_log10() +
  scale_x_log10() 

# plot the residuals and add the robust linear model
dst_model |> 
  augment(clean_distances) |> 
  ggplot(aes(cd_cases, .resid)) +
  geom_point() +
  geom_smooth(method = rlm) +
  scale_x_log10()

#plot only the residuals greater than 1.5 (abnormal)
dst_model |> 
  augment(clean_distances) |> 
  filter(.resid > 1.5) |> 
  ggplot(aes(cd_cases, .resid)) +
  geom_point() +
  scale_x_log10()

# plot residuals and add the linear model line and a line separating the
# residuals above 1.5 (abnormal)
dst_model |> 
  augment(clean_distances) |> 
  ggplot(aes(cd_cases, .resid)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = rlm) +
  geom_hline(yintercept = 1.5, color = 'orchid')

# see fitted values
abnormal <- dst_model |> 
  augment(clean_distances)|>
  # remove columns to be able to see new columns
  select(-total_cases, -hr_props) |>
  #filter for residuals greater than 1.5
  filter(.resid > 1.5)

abnormal |>
  ggplot() +
  geom_point(x = hour, y = )
