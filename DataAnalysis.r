library(tidyverse)
library(lubridate)

nc_hotel <- read.csv("nc_hotel_group.csv")

#glimpse(nc_hotel)

#nc_hotel |> 
  #count(status, canceled)
  #filter(status == "Canceled") |> 
  #select(status, status_date) |> 
  #head()


#class(nc_hotel$status_date)

# Keep actual cancellations and calculate advance notice
cancellations <- nc_hotel |>
  filter(status == "Canceled") |>
  mutate(
    status_date = ymd(status_date),
    arrival_date = make_date(
      year = arrival_year,
      month = match(arrival_month, month.name),
      day = arrival_day_of_month
    ),
    days_before_arrival = as.numeric(arrival_date - status_date)
  )

# Check the resulting dates and differences
cancellations |>
  select(arrival_date, status_date, days_before_arrival) |>
  head()

# Check for missing dates and cancellations recorded after arrival
cancellations |>
  summarise(
    total_cancellations = n(),
    missing_notice = sum(is.na(days_before_arrival)),
    negative_notice = sum(days_before_arrival < 0, na.rm = TRUE),
    same_day = sum(days_before_arrival == 0, na.rm = TRUE)
  )

# Inspect any records with negative notice
cancellations |>
  filter(days_before_arrival < 0) |>
  select(property, arrival_date, status_date, days_before_arrival)

# Inspect the overall distribution
summary(cancellations$days_before_arrival)

cancellations |> 
  summarise(
    total_cancellations = n(),
    late_cancellations = sum(
      days_before_arrival >= 0 & days_before_arrival <= 7,
      na.rm = TRUE
    ),
    late_percentage = late_cancellations / total_cancellations * 100
  )

cancellations |>
  group_by(property) |>
  summarise(
    total_cancellations = n(),
    late_cancellations = sum(
      days_before_arrival >= 0 & days_before_arrival <= 7,
      na.rm = TRUE
    ),
    late_percentage = late_cancellations / total_cancellations * 100
  )

late_cancellation <- cancellations |> 
  group_by(property) |> 
  summarise(
    late_cancellations = sum(
        days_before_arrival >= 0 & days_before_arrival <= 7,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) 

booking_total <- nc_hotel |> 
  group_by(property) |> 
  summarise(
    total_booking = n(),
    .groups = "drop"
  )

booking_total |>
  left_join(late_cancellation, by = "property") |>
  mutate(
    late_rate_all_bookings = late_cancellations / total_booking * 100
  )
   
 channel_totals <- nc_hotel |> 
   group_by(property, booking_channel) |> 
   summarise(
    total_booking = n(),
    .groups = "drop"
)

channel_late <- cancellations |> 
  group_by(property, booking_channel) |> 
   summarise(
    late_cancellations = sum(
        days_before_arrival >= 0 & days_before_arrival <= 7,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

channel_totals |> 
  left_join(channel_late, by = c("property", "booking_channel")) |> 
  replace_na(list(late_cancellations = 0)) |> 
  mutate(
    late_rate = late_cancellations / total_booking * 100
  ) |> 
  arrange(desc(late_cancellations))

nc_hotel <- nc_hotel |>
  mutate(
    total_nights = weekend_stays + week_stays
  )

# Inspect the distribution
summary(nc_hotel$total_nights)

nc_hotel |>
  summarise(
    zero_night_bookings = sum(total_nights == 0, na.rm = TRUE),
    missing_total_nights = sum(is.na(total_nights))
  )

# Examine zero-night bookings by property and outcome
nc_hotel |>
  filter(total_nights == 0) |>
  count(property, status)