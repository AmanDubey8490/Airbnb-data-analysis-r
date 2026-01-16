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

ggplot(airbnb_clean, aes(room_type, price)) +
  geom_boxplot(fill = "#2c7fb8") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Airbnb Price Distribution by Room Type",
    x = "Room Type",
    y = "Price per Night"
  ) +
  theme_minimal()

ggplot(airbnb_clean, aes(host_is_superhost, price)) +
  geom_boxplot(fill = "#41ab5d") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Price Comparison: Superhost vs Non-Superhost",
    x = "Is Superhost",
    y = "Price per Night"
  ) +
  theme_minimal()

ggplot(airbnb_clean, aes(accommodates, price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Price vs Accommodation Capacity",
    x = "Number of Guests",
    y = "Price per Night"
  ) +
  theme_minimal()

ggplot(airbnb_clean, aes(review_scores_rating, price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(
    title = "Impact of Review Scores on Price",
    x = "Review Score",
    y = "Price per Night"
  ) +
  theme_minimal()

ggsave("plots/price_by_room_type.png", width = 8, height = 5)

airbnb_clean_filtered <- airbnb_clean %>%
  filter(
    !is.na(review_scores_rating),
    price <= 500
  )

ggplot(airbnb_clean_filtered, aes(review_scores_rating, price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Impact of Review Scores on Price",
    x = "Review Score",
    y = "Price per Night"
  ) +
  theme_minimal()

ggsave("plots/price_by_room_type.png")

p1 <- ggplot(airbnb_clean_filtered, aes(room_type, price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Room Type") +
  theme_minimal()

ggsave("plots/price_by_room_type.png", plot = p1, width = 8, height = 5)

p2 <- ggplot(airbnb_clean_filtered, aes(review_scores_rating, price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Impact of Review Scores on Price") +
  theme_minimal()

ggsave("plots/review_score_vs_price.png", plot = p2, width = 8, height = 5)

p1 <- ggplot(airbnb_clean_filtered, aes(room_type, price)) +
  geom_boxplot(fill = "#2C7FB8", alpha = 0.7) +
  labs(
    title = "Price Distribution by Room Type",
    x = "Room Type",
    y = "Price per Night ($)"
  ) +
  theme_minimal()

ggsave(
  "plots/price_by_room_type.png",
  plot = p1,
  width = 8,
  height = 5
)

p3 <- airbnb_clean_filtered %>%
  mutate(host_is_superhost = ifelse(host_is_superhost == "t", "Superhost", "Non-Superhost")) %>%
  ggplot(aes(host_is_superhost, price)) +
  geom_boxplot(fill = "#F03B20", alpha = 0.7) +
  labs(
    title = "Price Comparison: Superhost vs Non-Superhost",
    x = "Host Type",
    y = "Price per Night ($)"
  ) +
  theme_minimal()

ggsave(
  "plots/superhost_price_comparison.png",
  plot = p3,
  width = 7,
  height = 5
)

p4 <- ggplot(top_neighbourhoods,
             aes(reorder(neighbourhood, avg_price), avg_price)) +
  geom_col(fill = "#6A51A3") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Top 10 Most Expensive Neighborhoods",
    x = "Neighborhood",
    y = "Average Price per Night ($)"
  ) +
  theme_minimal()

ggsave(
  "plots/top_10_neighbourhoods.png",
  plot = p4,
  width = 8,
  height = 6
)

p5 <- ggplot(airbnb_clean_filtered,
             aes(accommodates, price)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Price vs Accommodation Capacity",
    x = "Number of Guests",
    y = "Price per Night ($)"
  ) +
  theme_minimal()

ggsave(
  "plots/price_vs_accommodates.png",
  plot = p5,
  width = 8,
  height = 5
)


