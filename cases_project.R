# load librarys
library(MASS)
library(broom)
# load pre-processed case data
source('cases-preprocess.r')
# read code data
code <- read_csv('/Users/georgiagrahek/Data sets/icd_codes.csv')

# First we will see if there is any duplicates in the code data. This will be
# helpful to take care of before we join tables.

#pipe code data into count function
code |>
  # count by code and disease
  count(code, disease) |>
  # filter to only get values greater than 1 (duplicates)
  filter(n > 1)
#assign new distinct data to "code"
code <- code |>
  # pipe code data into distinct to remove dupliactes
  distinct()
code
# Now we have a code table with no duplicate codes in it. 

#Lets first look at the data to see what codes might be abnormal

# pipe data into arrange() and arrange in descending order by dst column
clean_distances |> arrange(desc(dst))

#We can now see the codes with the largest distance from the base line. This tells
# us the most abnormal codes.

# Next we will want to plot the distances of each code and the amount of cases per code
# to see if there is correlation. We will also want to but a robust linear model line to
# see which points are far off. We will need to use log on both the x and y access to visual it better

# pipe data into ggplot
clean_distances |>
  # plot cd_cases on the x axis and dst on the y axis 
  ggplot(aes(x = cd_cases, y = dst)) +
  # add a geom point layer
  geom_point() +
  # add a robust linear model line layer
  geom_smooth(method = rlm) +
  # log the x and y axis
  scale_y_log10() +
  scale_x_log10()

# Now we have a graph that shows us that the more cases a code has the lower its distance from the baseline
# is. This tells us that codes with few cases might not have enough data to look normal. 

#Next I will create a robust linear model using dst and cd_cases. I will need to log both of these
#since we only did it visually before. This model will predict distance with the number of cases per code.

# assign model to "dst_model"
dst_model <- 
  #pipe data into rlm()
  clean_distances |> 
  # y = dst and pip into log(), x = cd_cases and pipe into log(), use data = _ to input the data
  rlm(dst |> log() ~ cd_cases |> log(), data = _)

# now we have a model that gives us an intercept and slope for out line

# Now we want to be able to get the residuals from the graph above to see which codes 
# are deemed as abnormal. We Will graph the residuals against the cases per code to try and
# visualize what is a normal amount of error. I will add the rlm line to the graph and add
# a line that separates the points that are abnormal (spread far apart), residuals that are > 1.5.

#pipe model into augment
dst_model |> 
  # add model values(residuals) to the data table
  augment(clean_distances) |> 
  # pipe new data into ggplot and put cd_cases on the x axis and residuals on the y
  ggplot(aes(cd_cases, .resid)) +
  # add a point layer
  geom_point() +
  # log the x axis to unsquish the points
  scale_x_log10() +
  # add a rlm line 
  geom_smooth(method = rlm) +
  # add a line at y = 1.5
  geom_hline(yintercept = 1.5, color = 'orchid')

# This graph shows that there are 13 points that are above the line of 1.5. The are the
# abnormal codes. 

# Now create a table of just abnormal codes to separate from the rest of the codes.

# assign to "abnormal"
abnormal <- dst_model |>
  #pipe model into augment 
  # add values to table
  augment(clean_distances) |>
  # select useful columns
  select(code, cd_cases, dst, .resid) |>
  # filter for residuals greater than 1.5 (abnormal)
  filter(.resid > 1.5) |>
  # get only distinct values
  distinct()
abnormal

#Now we have a table with the abnormal codes, the number of cases, distance, and its residual. 
# this will be helpful later for plotting.

# Next i want to join the code data to the other data table so i can see the 
# cause of death related the the specific code. 

#pipe abnormal code table into left join
abnormal |>
  # use left join to add the "disease" column from the code table
  left_join(code, by = "code") |>
  # arrange by cd_cases in descending order
  arrange(desc(cd_cases))

#Now we have 13 codes that have the most abnormal patterns. We can now plot all the graphs
# together using facet wrap to compare code patterns to the overall base line patter with all the cases.

# faceted graphs 
clean_distances |>
  # semi join the tables so only the abnormal codes are shown
  semi_join(abnormal, by = "code") |>
  # left join by code 
  left_join(code, by = "code") |>
  # order the graphs by most cases per code
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
    y = "Proportion of cases by hour",
    # add legend title
    color = "Death category"
  )

# Now we can see all the graphs at once compared to all deaths. This helps us see how drastic the 
# patterns are from the abnormal codes to all deaths. 
