# PROJECT MAP --------------------------------------------------------------
# Working question: Where are late cancellations concentrated, and what
# evidence can guide a targeted response by NC Hotel Group?
# Late = canceled 0-7 days before arrival, including arrival day.
# One row = one booking, not one guest. No-shows are analyzed separately
# from actual cancellations here: only status == "Canceled" is selected.
# Booking value = booked nights * daily rate; this is NOT verified revenue loss.
#
# Sections below follow the questions we have explored together:
# Q1 outcomes; Q2 notice timing; Q3 property comparison; Q4 channels;
# Q5 nights/rates quality checks; Q6 booking value; Q7 deposit terms.
# Remaining: reconcile inclusion rules, select one focused follow-up,
# check 3/7/14-day definitions, then synthesize charts and recommendations.

# SETUP: Load packages and the original reservations ------------------------
library(tidyverse)
library(lubridate)

nc_hotel <- read.csv("nc_hotel_group.csv")

# Q1. What does each outcome mean, and how is canceled coded? ----------------
# Earlier output: 7,348 Canceled; 12,462 Check-Out; 190 No-Show.
# Both Canceled and No-Show have canceled == 1.
# The initial inspection commands below are currently commented out.
glimpse(nc_hotel)

nc_hotel |> 
  count(status, canceled)
  filter(status == "Canceled") |> 
  select(status, status_date) |> 
  head()


class(nc_hotel$status_date)

# Q2. How far before arrival do actual cancellations occur? -----------------
# Subtract cancellation date from arrival date; negative values need review.
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

# Q2b. What share of cancellations happen within 0-7 days of arrival? --------
# Denominator: all canceled bookings (7,348), not all reservations.
# Earlier result: 853 / 7,348 = about 11.61%.
# Check missing_notice above: missing notice must not silently count as nonlate.
cancellations |> 
  summarise(
    total_cancellations = n(),
    late_cancellations = sum(
      days_before_arrival >= 0 & days_before_arrival <= 7,
      na.rm = TRUE
    ),
    late_percentage = late_cancellations / total_cancellations * 100
  )

# Q3a. Among cancellations, does the late share differ by property? ----------
# Earlier results: City 11.7%; Resort 11.4%. These are NOT all-booking rates.
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

# Q3b. What fraction of ALL bookings cancel late at each property? -----------
# Numerator comes from cancellations; denominator comes from nc_hotel.
# Earlier results: City 642/13,203 = 4.86%; Resort 211/6,797 = 3.10%.
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
   
# Q4. Which property/channel combinations contribute late cancellations? ----
# Compare both counts (scale) and rates (frequency); small groups are unstable.
# City Online TA: 313 late cancellations among 6,409 bookings (4.88%).
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

# Q5a. Are booked stay lengths suitable for a booking-value calculation? -----
# 105 zero-night records; no missing lengths. Zero nights does not mean
# no reservation or low hotel occupancy. Preserve original records for now.
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

# Q5b. Are daily rates missing, zero, negative, or unusually high? ------------
# Earlier checks: 293 zeros, no negatives/missing; maximum $451.50.
# Zero rates may be legitimate; a high rate alone is not proof of an error.
summary(nc_hotel$average_daily_rate)

nc_hotel |> 
  summarise(
    zero_rate_bookings = sum(average_daily_rate == 0, na.rm = TRUE),
    negative_rate = sum(average_daily_rate < 0, na.rm = TRUE),
    missing_rates = sum(is.na(average_daily_rate)),
    all_booking = n()
  ) 

nc_hotel |> 
  arrange(desc(average_daily_rate)) |> 
  select(property, status, total_nights,average_daily_rate) |> 
  head()

# Q6. Which channels have the most lodging value attached to late cancels? ---
# Updating nc_hotel did not update the separately created cancellations table.
# Current filter below retains zero-night records: they add $0 but may affect
# counts.
cancellations <- cancellations |> 
  mutate(
    total_nights = weekend_stays + week_stays
  )

late_cancellation_values <- cancellations |> 
  filter(
    days_before_arrival >= 0 & days_before_arrival <= 7
  ) |> 
  mutate(
    booking_value = total_nights * average_daily_rate
  )
  

late_cancellation_values |> 
  group_by(property, booking_channel) |> 
  summarise(
    late_cancellations = n(),
    summed_booking = sum(booking_value, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(desc(summed_booking))

# Q7a. What deposit terms do City Online TA late cancellations have? ----------
# Earlier result: 312 No Deposit (~$117,143); 1 Non Refund (~$122).
# These are counts among late cancellations, not deposit-specific risk rates.
late_cancellation_values |> 
  filter(property == "City Hotel", booking_channel == "Online TA") |> 
  group_by(deposit_terms) |> 
  summarise(
    total_booking = n(),
    summed_booking = sum(booking_value, na.rm = TRUE)
  )

# Q7b. What are late-cancellation rates within each deposit category? --------
# Both tables below exclude zero-night bookings consistently.
# Earlier results: No Deposit 312/6,383; Non Refund 1/6; Refundable 0/2.
# Deposit groups are too small for a reliable comparison of deposit effects.
# Terms are not proof of collected payments; observational rates are not causal.
# Count all overnight bookings by deposit terms
deposit_totals <- nc_hotel |>
  filter(
    property == "City Hotel",
    booking_channel == "Online TA",
    total_nights > 0
  ) |>
  group_by(deposit_terms) |>
  summarise(
    all_bookings = n(),
    .groups = "drop"
  )

# Count late-canceled overnight bookings by deposit terms
deposit_late <- late_cancellation_values |>
  filter(
    property == "City Hotel",
    booking_channel == "Online TA",
    total_nights > 0
  ) |>
  group_by(deposit_terms) |>
  summarise(
    late_cancellations = n(),
    .groups = "drop"
  )

# Join the summaries and calculate rates
deposit_comparison <- deposit_totals |>
  left_join(deposit_late, by = "deposit_terms") |>
  replace_na(list(late_cancellations = 0L)) |>
  mutate(
    late_rate = late_cancellations / all_bookings * 100
  )

deposit_comparison

# NEXT STEPS (not yet implemented) -----------------------------------------
# 1. Finish relevant quality checks and align inclusion rules across summaries.
#    Check zero-guest bookings and missing/Undefined categories where relevant.
# 2. Optional focused follow-up: late-cancellation rates by advance_time group
#    within City Online TA. advance_time is booking-to-arrival, not cancel notice.
# 3. Recalculate key comparisons using 3 and 14 days instead of 7 to check
#    whether the main finding depends on our chosen definition of late.
# 4. Choose a small set of charts, recommendations, and explicit limitations.
#    Deposit effectiveness and a safe overbooking number are not established.
# 5. Prepare reproducible analysis, <=10-slide PDF, and interview explanations.
