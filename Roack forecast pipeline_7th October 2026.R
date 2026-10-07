# ============================================================
# NEW ORLEANS COCKROACH SEARCH INTEREST: FORECASTING PIPELINE
# Data window: January 2016 to September 2026
# Redesigned October 7, 2026
# ============================================================

# ============================================================
# SETUP: loading every package the project uses
# RUN this block FIRST in every new R session.
# ============================================================

pkgs <- c("tidyverse",   # dplyr, ggplot2, readr, tibble, and friends
          "tsibble",     # time series data frames and yearmonth()
          "feasts",      # diagnostics like ljung_box and STL
          "fable",       # forecasting models (ARIMA, ETS, SNAIVE)
          "fabletools",  # model(), forecast(), accuracy(), scenarios()
          "ggtime",      # time series graphics (moved here from fabletools)
          "jsonlite",    # reading the weather API response
          "gtrendsR")    # pulling Google Trends data

# Attach each package. If any fails, R stops here with a visible error.
invisible(lapply(pkgs, library, character.only = TRUE))

# Verify: every package should print TRUE.
setNames(paste0("package:", pkgs) %in% search(), pkgs)

# ============================================================
# PARAMETERS: every date decision lives here, in one place.
# Why: next time you extend the project, you change these lines
# only, instead of hunting through the whole script.
# ============================================================

pull_tag       <- "2026_10_07"            # date stamp for saved raw files
start_month    <- yearmonth("2016 Jan")   # first month after Google's 2016 change
end_month      <- yearmonth("2026 Sep")   # last COMPLETE month
test_n         <- 24                      # months held out for testing
step_month     <- yearmonth("2022 Jan")   # Google's data collection change
scenario_end   <- yearmonth("2029 Aug")   # last month of scenario forecasts

# Derived values. yearmonth supports adding and subtracting months.
train_end <- end_month - test_n           # expect 2024 Sep
n_expected <- as.integer(end_month - start_month) + 1   # expect 129

# Google Trends request window: exactly the analysis window.
# Why: Google scales every pull so the highest month in the window
# equals 100. A noisy spike in the thin early years (before 2011)
# can grab the 100 and squash every later value. Requesting only
# 2016 onward keeps the scale anchored to the months to be analyzed.
# Windows longer than about five years still return monthly data.
trends_window <- paste(format(as.Date(start_month), "%Y-%m-%d"),
                       format(as.Date(end_month + 1) - 1, "%Y-%m-%d"))

train_end
n_expected
trends_window   # expect "2016-01-01 2026-09-30"

# ============================================================
# STAGE 1: Importing and checking the saved Google Trends data
# ============================================================

# The project uses ONE search term: "cockroach".
# The data was pulled once from Google Trends on October 7, 2026
# (New Orleans metro, US-LA-622, January 2016 to September 2026)
# and saved to the data folder. Loading the saved file, instead of
# pulling again, keeps every run identical, since Google samples
# its data and a new pull would give slightly different numbers.

roach_df <- read_csv("C:/Roach season/roach_project/data/roach_raw_pulled_2026_10_07.csv", show_col_types = FALSE)

# CHECKS. All three must pass before moving on.
unique(roach_df$time)               # audit trail: must match trends_window
nrow(roach_df) == n_expected        # must print TRUE (129 rows)
roach_df %>% slice_max(hits, n = 3) # the 100 should sit inside the window

glimpse(roach_df)

# ============================================================
# STAGE 2: Cleaning the series and building a tsibble
# ============================================================

roach_ts <- roach_df %>%
  filter(keyword == "cockroach") %>%
  mutate(month = yearmonth(date)) %>%
  filter(month >= start_month, month <= end_month) %>%
  select(month, hits) %>%
  as_tsibble(index = month)

# Sanity checks.
nrow(roach_ts)            # expect 129
has_gaps(roach_ts)        # expect FALSE
sum(roach_ts$hits == 0)   # expect 1 (December 2024); Stage 2b fills it
tail(roach_ts)            # last row should be 2026 Sep


# ============================================================
# STAGE 2b: Replacing threshold zeros with model based estimates
# Why: a zero in a busy season is Google's privacy threshold or
# sampling noise, not "nobody searched." A zero also breaks the
# log transformation, since log(0) is negative infinity.
# Method: mark zeros as missing, fit a seasonal ARIMA on the log
# scale (ARIMA can handle missing months), then let the model
# estimate the missing values from the months around them AND
# the usual seasonal pattern.
# ============================================================

# Record which months get filled, so the write up can report them.
imputed_months <- roach_ts %>%
  filter(hits == 0) %>%
  pull(month)

imputed_months   # expect 2024 Dec

# Work on the log scale, with zeros turned into missing values.
roach_log <- roach_ts %>%
  mutate(log_hits = if_else(hits == 0, NA_real_, log(hits))) %>%
  select(month, log_hits)

# Fit ARIMA, then fill the missing months.
# interpolate() returns the series with every NA replaced.
roach_filled <- roach_log %>%
  model(ARIMA(log_hits)) %>%
  interpolate(roach_log)

# Back to the original 0 to 100 scale.
roach_ts <- roach_filled %>%
  mutate(hits = exp(log_hits)) %>%
  select(month, hits)

# Checks.
sum(is.na(roach_ts$hits))                      # expect 0
sum(roach_ts$hits == 0)                        # expect 0
roach_ts %>% filter(month %in% imputed_months) # the filled value(s)

# ============================================================
# STAGE 3: Exploring trend and seasonality
# ============================================================

roach_ts %>%
  autoplot(hits) +
  labs(title = "Cockroach search interest, New Orleans",
       y = "Search interest (0 to 100)", x = "Month")

roach_ts %>%
  gg_season(hits, labels = "right") +
  labs(title = "Seasonal plot: each line is one year",
       y = "Search interest", x = "Month")

roach_ts %>%
  gg_subseries(hits) +
  labs(title = "Subseries plot by calendar month", y = "Search interest")

# STL decomposition with a fixed seasonal shape.
roach_ts %>%
  model(STL(hits ~ season(window = "periodic"))) %>%
  components() %>%
  autoplot() +
  labs(title = "STL decomposition of cockroach search interest")

# Logged series: checks whether the log evens out the seasonal waves.
roach_ts %>%
  mutate(log_hits = log(hits)) %>%
  autoplot(log_hits) +
  labs(title = "Logged cockroach search interest",
       y = "log(search interest)", x = "Month")


# ============================================================
# STAGE 4: Adding the step variable for Google's 2022 change
# 0 before January 2022, 1 from January 2022 onward.
# ============================================================

roach_ts2 <- roach_ts %>%
  mutate(step_2022 = if_else(month >= step_month, 1, 0))


# ============================================================
# STAGE 5a: Pulling historical weather through the last day of end_month
# ============================================================

# Last day of end_month: first day of the next month, minus one day.
wx_end_date <- format(as.Date(end_month + 1) - 1, "%Y-%m-%d")
wx_end_date   # expect "2026-09-30"

wx_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=29.95&longitude=-90.07",
  "&start_date=", format(as.Date(start_month), "%Y-%m-%d"),
  "&end_date=", wx_end_date,
  "&daily=temperature_2m_mean,precipitation_sum",
  "&temperature_unit=fahrenheit&precipitation_unit=inch",
  "&timezone=America%2FChicago"
)

wx_raw <- fromJSON(wx_url)

wx_daily <- tibble(
  date   = as.Date(wx_raw$daily$time),
  temp   = wx_raw$daily$temperature_2m_mean,   # daily mean temp, °F
  precip = wx_raw$daily$precipitation_sum      # daily rainfall, inches
)

write_csv(wx_daily, paste0("weather_daily_pulled_", pull_tag, ".csv"))

# Roll daily weather up to months.
wx_monthly <- wx_daily %>%
  mutate(month = yearmonth(date)) %>%
  group_by(month) %>%
  summarise(
    temp_mean    = mean(temp, na.rm = TRUE),
    precip_total = sum(precip, na.rm = TRUE),
    n_days       = n(),
    n_missing    = sum(is.na(temp)),
    .groups = "drop"
  )

# Sanity checks. Missing days in September would mean the archive
# hasn't caught up yet, so wait a few days and pull again.
nrow(wx_monthly)            # expect 129
sum(wx_monthly$n_missing)   # expect 0
range(wx_monthly$n_days)    # expect 28 to 31


# ============================================================
# STAGE 5a (exploration): does heavy rain line up with roach
# search surprises?
# Rain gets a model slot in Stage 5c
# only if it shows a real hint of a link here.
# ============================================================

# Monthly rain measures from the daily data.
# heavy_rain_days: days with at least 1 inch, a common cutoff for
# heavy rain. Downpours flood sewers; drizzle doesn't.
rain_monthly <- wx_daily %>%
  mutate(month = yearmonth(date)) %>%
  group_by(month) %>%
  summarise(
    precip_total    = round(sum(precip, na.rm = TRUE), 1),
    heavy_rain_days = sum(precip >= 1, na.rm = TRUE),
    .groups = "drop"
  )

# Past Octobers at a glance: rain and roach searches side by side.
rain_monthly %>%
  filter(lubridate::month(as.Date(month)) == 10) %>%
  left_join(roach_ts %>% as_tibble(), by = "month") %>%
  print(n = Inf)

# Roach "surprises": STL remainder on the log scale, so a surprise
# means a percentage jump, fair to both summer and winter months.
roach_remainder <- roach_ts %>%
  model(STL(log(hits) ~ season(window = "periodic"))) %>%
  components() %>%
  as_tibble() %>%
  select(month, remainder)

rain_check <- roach_remainder %>%
  left_join(rain_monthly, by = "month")

# Correlation between heavy rain days and surprises.
# Near 0: no link. Clearly positive: supports the flooding idea.
# Clearly negative: supports the drought idea.
# Rough guide: below 0.1 in either direction is basically nothing;
# around 0.2 or more is worth testing in a model.
cor(rain_check$heavy_rain_days, rain_check$remainder)

# Picture: each dot is one month.
ggplot(rain_check, aes(x = heavy_rain_days, y = remainder)) +
  geom_jitter(width = 0.15, alpha = 0.6) +   # jitter spreads dots that share a value
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Heavy rain days vs roach search surprises",
       x = "Days with 1 inch of rain or more",
       y = "STL remainder (log scale)") +
  theme_minimal()


# ============================================================
# STAGE 5b: Building temperature anomalies (training months only)
# Why training only: using test months to define "normal" would
# leak future information into the model.
# ============================================================

wx_normals <- wx_monthly %>%
  filter(month <= train_end) %>%
  mutate(cal_month = lubridate::month(as.Date(month))) %>%
  group_by(cal_month) %>%
  summarise(temp_norm = mean(temp_mean), .groups = "drop")

wx_anom <- wx_monthly %>%
  mutate(cal_month = lubridate::month(as.Date(month))) %>%
  left_join(wx_normals, by = "cal_month") %>%
  mutate(temp_anom = temp_mean - temp_norm) %>%
  select(month, temp_anom)

roach_ts3 <- roach_ts2 %>%
  left_join(wx_anom, by = "month")

nrow(roach_ts3)                   # expect 129
sum(is.na(roach_ts3$temp_anom))   # expect 0


# ============================================================
# STAGE 5c: Training and testing split, fitting models, comparing accuracy
# ============================================================

roach_train <- roach_ts3 %>% filter(month <= train_end)
roach_test  <- roach_ts3 %>% filter(month >  train_end)

nrow(roach_train)   # expect 105
nrow(roach_test)    # expect 24

# Both ARIMA models share the same error structure, PDQ(2,1,0),
# so temperature is the only difference between them.
roach_fit <- roach_train %>%
  model(
    snaive     = SNAIVE(log(hits)),
    ets        = ETS(log(hits)),
    arima_step = ARIMA(log(hits) ~ step_2022 +
                         pdq(0, 0, 0) + PDQ(2, 1, 0)),
    arima_temp = ARIMA(log(hits) ~ step_2022 + temp_anom +
                         pdq(0, 0, 0) + PDQ(2, 1, 0))
  )

# No model should show <NULL model>.
roach_fit

roach_fit %>% select(ets) %>% report()
roach_fit %>% select(arima_temp) %>% report()

# Forecast the test months using the actual weather (best case).
roach_fc <- roach_fit %>%
  forecast(new_data = roach_test)

roach_fc %>%
  accuracy(roach_ts3) %>%
  select(.model, MAE, RMSE, MAPE) %>%
  arrange(RMSE)

# Accuracy excluding the imputed month, so every graded value is real.
roach_fc %>%
  filter(month != yearmonth("2024 Dec")) %>%
  accuracy(roach_ts3) %>%
  select(.model, MAE, RMSE, MAPE) %>%
  arrange(RMSE)

# ============================================================
# STAGE 5d: Residual checking for ETS
# ============================================================

roach_fit %>%
  select(ets) %>%
  gg_tsresiduals()

augment(roach_fit) %>%
  filter(.model == "ets") %>%
  features(.innov, ljung_box, lag = 24)


# ============================================================
# STAGE 6: Time series cross validation
# .init = 84 keeps a full year of step = 1 in the first window.
# ============================================================

roach_cv <- roach_ts3 %>%
  stretch_tsibble(.init = 84, .step = 3)

n_distinct(roach_cv$.id)   # number of windows (exams)

cv_fit <- roach_cv %>%
  model(
    snaive     = SNAIVE(log(hits)),
    ets        = ETS(log(hits)),
    arima_step = ARIMA(log(hits) ~ step_2022),
    arima_temp = ARIMA(log(hits) ~ step_2022 + temp_anom)
  )

# Failed fits per model. Each count should be 0.
cv_fit %>%
  as_tibble() %>%
  summarise(across(c(snaive, ets, arima_step, arima_temp),
                   ~ sum(is_null_model(.x))))

# Future data for each window, with actual step and temperature.
cv_future <- new_data(roach_cv, 12) %>%
  left_join(roach_ts3 %>%
              as_tibble() %>%
              select(month, step_2022, temp_anom),
            by = "month") %>%
  filter(!is.na(step_2022))   # drop months beyond the data

cv_fc <- cv_fit %>%
  forecast(new_data = cv_future)

cv_fc %>%
  accuracy(roach_ts3) %>%
  select(.model, MAE, RMSE, MAPE) %>%
  arrange(RMSE)


# Accuracy across all 15 graded windows, all months included.
cv_fc %>%
  accuracy(roach_ts3) %>%
  select(.model, MAE, RMSE, MAPE) %>%
  arrange(RMSE)

# Same, but excluding the imputed December 2024, so every graded
# value is real data. Consistent with the Stage 5c check.
cv_fc %>%
  filter(month != yearmonth("2024 Dec")) %>%
  accuracy(roach_ts3) %>%
  select(.model, MAE, RMSE, MAPE) %>%
  arrange(RMSE)


# ============================================================
# STAGE 7a: Main forecast, October 2026 to scenario_end
# Model: regression with ARIMA errors, step only.
# Why this model: it tied for best in cross validation, and it
# needs no future weather, since the step is known (always 1).
# ARIMA orders are chosen automatically, the same way the model
# was chosen in every cross validation window.
# ============================================================

# Refit on ALL 129 months, since evaluation is done.
roach_main_fit <- roach_ts3 %>%
  model(arima_step = ARIMA(log(hits) ~ step_2022))

roach_main_fit %>% report()

# Forecast horizon in months.
h <- as.integer(scenario_end - end_month)
h   # expect 35

# Future months: the step stays 1 (Google's 2022 system continues).
future_main <- new_data(roach_ts3, h) %>%
  mutate(step_2022 = 1)

roach_main_fc <- roach_main_fit %>%
  forecast(new_data = future_main)

# One tidy table: mean, median, and the 80% prediction interval.
# The median is the "typical" value; the log model's skew pushes
# the mean slightly higher, so the median is reported for peaks.
main_df <- roach_main_fc %>%
  mutate(median_fc = median(hits)) %>%
  hilo(level = 80) %>%
  unpack_hilo(`80%`) %>%
  as_tibble() %>%
  select(month, mean_fc = .mean, median_fc,
         lower = `80%_lower`, upper = `80%_upper`)

# History from 2023 onward, as a plain tibble.
hist_df <- roach_ts3 %>%
  as_tibble() %>%
  filter(month >= yearmonth("2023 Jan")) %>%
  select(month, hits)

# Points to label directly on the curve: every July (peak season)
# and every December (low season) in the forecast.
# vjust_val puts July labels above the line and December labels below.
key_points <- main_df %>%
  filter(lubridate::month(as.Date(month)) %in% c(7, 12)) %>%
  mutate(
    label     = paste0(format(as.Date(month), "%b %Y"), ": ", round(median_fc)),
    vjust_val = if_else(lubridate::month(as.Date(month)) == 7, -1.2, 2)
  )

# First forecast month, taken from the parameters block.
forecast_start <- as.Date(end_month + 1)

# Plot: grey band first, so the lines draw on top of it.
# Saved as an object so it can be printed AND exported to a file.
forecast_plot <- ggplot() +
  geom_ribbon(data = main_df,
              aes(x = as.Date(month), ymin = lower, ymax = upper),
              fill = "grey80") +
  geom_line(data = hist_df,
            aes(x = as.Date(month), y = hits),
            color = "black") +
  geom_line(data = main_df,
            aes(x = as.Date(month), y = median_fc),
            color = "steelblue") +
  # Dots and text labels for every July and December forecast.
  geom_point(data = key_points,
             aes(x = as.Date(month), y = median_fc),
             color = "steelblue", size = 2) +
  geom_text(data = key_points,
            aes(x = as.Date(month), y = median_fc,
                label = label, vjust = vjust_val),
            size = 3) +
  # Dotted line marking where real data ends and the forecast begins.
  geom_vline(xintercept = forecast_start, linetype = "dotted") +
  annotate("text", x = forecast_start, y = 15,
           label = paste("Forecast starts", format(forecast_start, "%b %Y")),
           hjust = -0.05, size = 3) +
  geom_hline(yintercept = 100, linetype = "dashed") +   # the index ceiling
  # One label for EVERY month, set explicitly so each tick lands on
  # the first of the month. Short labels ("Jul 28") keep 80 labels
  # from colliding.
  scale_x_date(breaks = seq(as.Date("2023-01-01"),
                            as.Date(scenario_end),
                            by = "1 month"),
               date_labels = "%b %y",
               expand = expansion(mult = 0.01)) +
  # A labeled gridline every 10 points, so any value can be read
  # straight off the axis. The range runs to 110 because the upper
  # edge of the prediction band crosses the 100 ceiling.
  scale_y_continuous(breaks = seq(0, 110, by = 10)) +
  labs(title = "Cockroach search interest forecast to August 2029",
       subtitle = "Black: actual data. Blue: median forecast. Grey band: 80% prediction interval. Dashed line: index maximum (100)",
       x = "Month", y = "Search interest (0 to 100)") +
  theme_minimal() +
  # Rotate month labels and shrink them slightly so all 80 fit.
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                                   size = 7),
        panel.grid.minor.x = element_blank())

# Show the plot in RStudio. The Plots pane is narrow, so labels may
# look crowded there; the saved file below is the real version.
forecast_plot

# Save a wide version into the output folder. 16 x 6 inches gives
# each of the 80 monthly labels enough room to stay readable.
ggsave(file.path("output", "forecast_to_2029_monthly.png"), forecast_plot,
       width = 16, height = 6, dpi = 300, bg = "white")

# Peak season summary: July of each forecast year.
main_df %>%
  filter(lubridate::month(as.Date(month)) == 7) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Ceiling check: upper bounds above 100 are impossible for this index.
main_df %>%
  summarise(months_above_100 = sum(upper > 100),
            max_upper = round(max(upper), 1))


# ============================================================
# STAGE 7b: Temperature sensitivity note
# Question: IF the small temperature effect is real, how much
# would a hot summer move the forecast?
# Why only a note: cross validation showed temperature did not
# consistently improve accuracy, and its coefficient is weak.
# ============================================================

# Refit the temperature model on all data (automatic orders,
# matching cross validation).
roach_temp_fit <- roach_ts3 %>%
  model(arima_temp = ARIMA(log(hits) ~ step_2022 + temp_anom))

roach_temp_fit %>% report()

# Effect of a summer month 3°F above normal, with a 95% interval.
# exp(3 x coefficient) turns the log scale effect into a multiplier;
# subtracting 1 and multiplying by 100 gives a percent change.
# If the interval includes 0, the data cannot rule out "no effect."
tidy(roach_temp_fit) %>%
  filter(term == "temp_anom") %>%
  transmute(
    estimate,
    std.error,
    pct_per_3F = round(100 * (exp(3 * estimate) - 1), 1),
    pct_low    = round(100 * (exp(3 * (estimate - 1.96 * std.error)) - 1), 1),
    pct_high   = round(100 * (exp(3 * (estimate + 1.96 * std.error)) - 1), 1)
  )

# Two scenarios only: normal weather vs summers 3°F hotter.
future_temp <- new_data(roach_ts3, h) %>%
  mutate(step_2022 = 1,
         cal_month = lubridate::month(as.Date(month)))

roach_scen <- scenarios(
  normal      = future_temp %>% mutate(temp_anom = 0),
  hot_summers = future_temp %>%
    mutate(temp_anom = if_else(cal_month %in% 6:9, 3, 0)),
  names_to = "scenario"
)

roach_scen_fc <- roach_temp_fit %>%
  forecast(new_data = roach_scen)

# July comparison: how far apart are the two scenarios, and how does
# that gap compare with the width of the main forecast's interval?
roach_scen_fc %>%
  mutate(median_fc = median(hits)) %>%
  as_tibble() %>%
  filter(lubridate::month(as.Date(month)) == 7) %>%
  select(scenario, month, median_fc) %>%
  pivot_wider(names_from = scenario, values_from = median_fc) %>%
  left_join(main_df %>% select(month, lower, upper), by = "month") %>%
  mutate(
    gap            = hot_summers - normal,     # scenario difference, in points
    pct_gap        = 100 * (hot_summers / normal - 1),
    interval_width = upper - lower             # width of the main 80% band
  ) %>%
  select(month, normal, hot_summers, gap, pct_gap, interval_width) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

#................... THE END ................#
