airbnb <- read_csv("data/Airbnb_listings.csv")

glimpse(airbnb)
skim(airbnb)

airbnb <- airbnb %>% clean_names()

airbnb_clean <- airbnb %>%
  select(
    listing_id, price, room_type, neighbourhood,
    accommodates, bedrooms, review_scores_rating
    , host_is_superhost
  )

airbnb_clean <- airbnb %>%
  select(
    id,
    price,
    room_type,
    neighbourhood_cleansed,
    accommodates,
    bedrooms,
    review_scores_rating,
    host_is_superhost
  )

colnames(airbnb)

airbnb_clean <- airbnb %>%
  select(
    listing_id,
    price,
    room_type,
    neighbourhood,
    accommodates,
    bedrooms,
    review_scores_rating,
    host_is_superhost
  )

library(stringr)

airbnb_clean <- airbnb_clean %>%
  mutate(
    price = as.numeric(str_remove_all(price, "[$,]"))
  ) %>%
  filter(price > 0, price < 1000)

skim(airbnb_clean)

library(tidyverse)
library(skimr)

airbnb_clean %>%
  summarise(
    total_listings = n(),
    avg_price = mean(price),
    median_price = median(price),
    avg_rating = mean(review_scores_rating, na.rm = TRUE)
  )

