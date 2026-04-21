library(ggplot2)
library(tidyverse)
library(refund)
library(gridExtra)

fires <- vroom::vroom("forestfires.csv")

# Variables of interest (Functional)
# FIRE_SIZE
# DISCOVERY_DATE (we want in months)
# X (LATITUDE)
# Y (LONGITUDE)
# CONT_DATE (when the fire was contained)

# Variables of interest (Non-Functional)
# STATE, COUNTY
# NWCG_CAUSE_CLASSIFICATION, NWCG_GENERAL_CAUSE (cause of fire)
# FIRE_YEAR (time grouping)
# FIRE_SIZE_CLASS

# Exploratory Analysis ----------------------------------------------------

# Monthly mean fire size by year
fires_month <- fires |>
  mutate(
    year = FIRE_YEAR,
    month = month(DISCOVERY_DATE, label = TRUE)
  ) |>
  group_by(year, month) |>
  summarise(total_size = mean(FIRE_SIZE, na.rm = TRUE), .groups = "drop")

ggplot(fires_month,
       aes(x = month, y = total_size, color = factor(year), group = year)) +
  geom_line() +
  labs(title = "Total area burned per month",
       x = "Month", y = "Total FIRE_SIZE", color = "Year") +
  theme_minimal()
  

# Monthly fire counts by year
fires_counts <- fires |>
  mutate(
    year  = FIRE_YEAR,
    month = month(DISCOVERY_DATE, label = TRUE)
  ) |>
  count(year, month, name = "n_fires")

ggplot(fires_counts,
       aes(x = month, y = n_fires, color = factor(year), group = year)) +
  geom_line() +
  labs(title = "Fire counts per month",
       x = "Month", y = "Number of fires", color = "Year") +
  theme_minimal()

# Spatial distribution
fires2 <- fires |>
  mutate(
    FIRE_SIZE_CLASS = factor(
      FIRE_SIZE_CLASS,
      levels = c("A","B","C","D","E","F","G"), ordered = TRUE
    ),
    size_num = as.numeric(FIRE_SIZE_CLASS)   # A=1, ..., G=7
  )

ggplot(fires2,
       aes(x = LONGITUDE, y = LATITUDE, color = size_num)) +
  geom_point(alpha = 0.6, size = 1) +
  coord_quickmap() +
  scale_color_gradient(
    low  = "white",
    high = "red",
    name = "Size class",
    breaks = 1:7,
    labels = c("A","B","C","D","E","F","G")
  ) +
  labs(title = "Fire locations by size class",
       x = "Longitude", y = "Latitude")


# Cause exploration
library(forcats)

ggplot(fires,
       aes(x = fct_rev(fct_infreq(NWCG_GENERAL_CAUSE)))) +
  geom_bar() +
  coord_flip() +
  labs(title = "Fire counts by general cause",
       x = "NWCG general cause", y = "Count")

# Analysis ----------------------------------------------------------------
# Is there some relationship between fire counts curve, and total area burned and how do I model or capture that
# - FPCA combined with a linear and non-linear regression model
# - Functional historical linear regression model
# - Fit fpca on the two graphs
# - Two years variables, multivariate response regression


# Practice Using pffr -----------------------------------------------------

## Key Notes
# where z is a constant
# ff -> "curve affects curve"
# s(z) -> "normal variable but flexible", so it might not have a straight line effect and could be curved
# c(z) -> "constant over time"
# z -> time-varying effect


# Modeling ----------------------------------------------------------------

# Making the dataset for our Y matrix
Y_dat <- fires_month |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from = total_size
  )

# Then I want to make this a matrix without the year variable
Y_mat <- as.matrix(Y_dat)[,-1]

# Then do the same for the X matrix

# Create the dataset
X_dat <- fires_counts |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from =n_fires
  )

# Then I want to make this a matrix
X_mat <- as.matrix(X_dat)[,-1]

t <- 1:ncol(Y_mat)  # response index (months)
s <- 1:ncol(X_mat)  # predictor index (months)

# for total area burned take the log
Y_log <- log(Y_mat+1)

## Get our full historical model
FullModel_Hist <- pffr(
  Y_log ~ ff(X_mat, 
             xind = s,
             limits = 's<t'),
  yind = t,
  method = "GCV.Cp",
  bs.int = list(bs = "ps", k = 5, m = c(2,1))
)

FullHistBetas <- coef(FullModel_Hist, n1 = 12, n2 = 12)$smterms[[2]]$value

summary(FullModel_Hist)
# R-squared of .584


## Get just our full model (not historical)
FullModel <- pffr(
  Y_log ~ ff(X_mat, 
             xind = s),
  yind = t,
  method = "GCV.Cp",
  bs.int = list(bs = "ps", k = 5, m = c(2,1))
)

FullBetas <- coef(FullModel, n1 = 12, n2 = 12)$smterms[[2]]$value

summary(FullModel)
# R-squared of .563


FullCoef <- coef(FullModel, n1 = 12, n2 = 12)
FullHistCoef <- coef(FullModel_Hist, n1 = 12, n2 = 12)

FullModel2 <- FullCoef$smterms[[2]]$coef
FullModel2_Hist <- FullHistCoef$smterms[[2]]$coef

## Plot the two betas to compare
FullBetasdf <- data.frame(
  't'    = c(FullModel2$X_mat.tmat, FullModel2_Hist$X_mat.tmat),
  's'    = c(FullModel2$X_mat.smat, FullModel2_Hist$X_mat.smat),
  'beta' = c(FullModel2$value, FullModel2_Hist$value),
  'Fit'  = factor(rep(c('Full pffr', 'Historical pffr'),
                      times = c(nrow(FullModel2),
                                nrow(FullModel2_Hist))))
)

## Plot Full pffr surface
Full <- ggplot(data = FullBetasdf[FullBetasdf$Fit == "Full pffr", ]) +
  geom_raster(aes(x = s, y = t, fill = beta)) +
  scale_fill_viridis_c() +
  xlab('s') + ylab('t') + theme_bw() +
  ggtitle('pffr Surface Fit')

## Plot Historical pffr surface
HistPlotData <- FullBetasdf[FullBetasdf$Fit == "Historical pffr", ]

# Set beta to NA wherever s < t
HistPlotData$beta[HistPlotData$s >= HistPlotData$t] <- NA

Full_Hist <- ggplot(data = HistPlotData) +
  geom_raster(aes(x = s, y = t, fill = beta)) +
  scale_fill_viridis_c(na.value = "white") +
  xlab('s') + ylab('t') + theme_bw() +
  ggtitle('Historical pffr Surface Fit')


grid.arrange(Full, Full_Hist)

## Full pffr (top plot)
# The brightest (yellow/green) values are in the bottom-left corner, 
  # meaning fire counts early in the year (s = 1–3) are the strongest positive 
  # predictors of area burned in early months (t = 0–3)
# This gets darker as time goes on so fire counts in the later year don't have 
  # a big affect on the area burned in early months
# All beta values are positive, meaning more fires always predicts more area 
  # burned — just some months more strongly than others

## Historical pffr (bottom plot) EDIT
# This model restricts predictions to only use past information (s < t), 
  # which is more realistic
# We can see that there is a stronger relationship in the early months, meaning
  # that fire counts in the early year are the strongest positive predictors of
  # fires burned in the early year. But that as time goes on, fire counts in the
  # later year are less strongly positive predictors of area burned in the early
  # year

## The main story is that the relationship between fire counts and area burned 
  # is strongest early in the year and weakens over time.
# Early season fire counts (January–March) are the most important predictors of 
  # area burned — this shows up clearly in both models as the bright 
  # yellow/green in the bottom-left corner
# The effect decays as the year progresses — by mid-to-late year, knowing how 
  # many fires occurred doesn't tell you much about how much area will burn, 
  # suggesting other factors (weather, drought, fuel conditions) become more 
  # dominant later in the season
# The historical model confirms this is a real causal-direction signal — 
  # because it only uses past information (s < t), the fact that the early-month 
  # pattern survives means it's genuinely predictive, not just a spurious   
  # correlation from borrowing future data
#The negative betas visible in the historical plot around mid-year suggest a 
  # possible suppression effect — perhaps after a high-fire early season, resources are depleted or conditions change such that later fire counts actually correspond to less area burned than expected

# Bottom line for your project: Early season fire activity is a meaningful 
  # leading indicator of burn area, and a historical functional model is 
  # appropriate here because it respects the natural time ordering — 
  # you wouldn't know future fire counts when trying to predict current burn area.
 
# Modeling pt.2 ----
# Doing it again but with the log(X) taken. It made no difference so ignore
# X_log <- log(X_mat+1)

# # Get our full historical model
# FullModel_Hist_log <- pffr(
#   Y_log ~ ff(X_log, 
#              xind = s,
#              limits = 's<t'),
#   yind = t,
#   bs.int = list(bs = "ps", k = 5, m = c(2,1))
# )

# FullHistBetas_log <- coef(FullModel_Hist_log, n1 = 12, n2 = 12)$smterms[[2]]$value

# summary(FullModel_Hist_log)

# # Get just our full model (not historical)
# FullModel_log <- pffr(
#   Y_log ~ ff(X_log, 
#              xind = s),
#   yind = t,
#   bs.int = list(bs = "ps", k = 5, m = c(2,1))
# )

# FullBetas_log <- coef(FullModel_log, n1 = 12, n2 = 12)$smterms[[2]]$value

# summary(FullModel_log)

# FullHistCoef_log <- coef(FullModel_Hist_log, n1 = 12, n2 = 12)
# FullCoef_log <- coef(FullModel_log, n1 = 12, n2 = 12)

# FullModel2_log <- FullCoef$smterms[[2]]$coef
# FullModel2_Hist_log <- FullHistCoef$smterms[[2]]$coef

# ## Plot the two betas to compare
# FullBetasdf2 <- data.frame(
#   't'    = c(FullModel2_log$X_mat.tmat, FullModel2_Hist_log$X_mat.tmat),
#   's'    = c(FullModel2_log$X_mat.smat, FullModel2_Hist_log$X_mat.smat),
#   'beta' = c(FullModel2_log$value, FullModel2_Hist_log$value),
#   'Fit'  = factor(rep(c('Full pffr', 'Historical pffr'),
#                       times = c(nrow(FullModel2_log),
#                                 nrow(FullModel2_Hist_log))))
# )

# ## Plot Full pffr surface
# Full2 <- ggplot(data = FullBetasdf2[FullBetasdf2$Fit == "Full pffr", ]) +
#   geom_raster(aes(x = s, y = t, fill = beta)) +
#   scale_fill_viridis_c() +
#   xlab('s') + ylab('t') + theme_bw() +
#   ggtitle('pffr Surface Fit')

# ## Plot Historical pffr surface
# HistPlotData2 <- FullBetasdf2[FullBetasdf2$Fit == "Historical pffr", ]

# # Set beta to NA wherever s < t
# HistPlotData2$beta[HistPlotData2$s >= HistPlotData2$t] <- NA

# Full_Hist2 <- ggplot(data = HistPlotData2) +
#   geom_raster(aes(x = s, y = t, fill = beta)) +
#   scale_fill_viridis_c(na.value = "white") +
#   xlab('s') + ylab('t') + theme_bw() +
#   ggtitle('Historical pffr Surface Fit')


# grid.arrange(Full2, Full_Hist2)




# Prediction -------------------------------------------------------------

a_b_train <- fires_month |> 
  filter(year <= 2015)
a_b_test <- fires_month |> 
  filter(year >= 2016)


fire_c_train <- fires_counts |> 
  filter(year <= 2015)
fire_c_test <- fires_counts |> 
  filter(year >= 2016)

Y_dat_a_train <- a_b_train |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from = total_size
  )

# Then I want to make this a matrix without the year variable
Y_mat_a_train <- as.matrix(Y_dat_a_train)[,-1]

Y_dat_a_test <- a_b_test |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from = total_size
  )

# Then I want to make this a matrix without the year variable
Y_mat_a_test <- as.matrix(Y_dat_a_test)[,-1]

## Then do the same for the X matrix

# Create the dataset
X_dat_c_train <- fire_c_train |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from =n_fires
  )

# Then I want to make this a matrix
X_mat_c_train <- as.matrix(X_dat_c_train)[,-1]

X_dat_c_test <- fire_c_test |>
  pivot_wider(
    names_from = month, # taking the values from month column and makes them each their own column
    values_from =n_fires
  )

# Then I want to make this a matrix
X_mat_c_test <- as.matrix(X_dat_c_test)[,-1]

t <- 1:ncol(Y_mat_a_train)  # response index (months)
s <- 1:ncol(X_mat_c_train)  # predictor index (months)

# for total area burned take the log
Y_log_train <- log(Y_mat_a_train+1)


## Get our full historical model
Train_Model_Hist <- pffr(
  Y_log_train ~ ff(X_mat_c_train, 
             xind = s,
             limits = 's<t'),
  yind = t,
  method = "GCV.Cp",
  bs.int = list(bs = "ps", k = 5, m = c(2,1))
)

Train_HistBetas <- coef(Train_Model_Hist, n1 = 12, n2 = 12)$smterms[[2]]$value

summary(Train_Model_Hist)
# R-squared of .616

## Get just our full model (not historical)
Train_Model <- pffr(
  Y_log_train ~ ff(X_mat_c_train, 
             xind = s),
  yind = t,
  method = "GCV.Cp",
  bs.int = list(bs = "ps", k = 5, m = c(2,1))
)

FullBetas <- coef(FullModel, n1 = 12, n2 = 12)$smterms[[2]]$value

summary(Train_Model)
# 0.609

## Regular Model
train_preds <- predict(Train_Model, newdata = list(X_mat_c_train = X_mat_c_test))

## Historical Prediction (done manually)

# Get intercept
B0 <- coef(Train_Model_Hist, n1 = 12, n2 = 12)$smterms[[1]]$coef$value
B0_matrix <- matrix(B0, nrow = 12, ncol = 5)

# Get the beta surface matrix (12 x 12)
B1 <- Train_HistBetas 
B1 <- matrix(B1, nrow = 12, ncol = 12)
B1[lower.tri(B1, diag = TRUE)] <- 0 # taking out values where s>=t

# Manual prediction

b1_matrix <- X_mat_c_test %*% B1
b1_matrix_t <- t(b1_matrix)

yhats_preds <- B0_matrix + b1_matrix_t

## Get RMSE's
y_test <- t(Y_mat_a_test)
historical_rmse <- sqrt(mean((y_test - yhats_preds)^2))
historical_rmse
# 143.81


train_preds_t <- t(train_preds)
regular_rmse <- sqrt(mean((y_test - train_preds_t)^2))
regular_rmse
# 141.586

# Plot

test_years <- 2016:2020

# --- Actual values (long format)
actual_long <- as.data.frame(Y_mat_a_test) |>
  setNames(1:12) |>
  mutate(year = test_years) |>
  pivot_longer(-year, names_to = "month", values_to = "actual") |>
  mutate(month = as.numeric(month))

# Back-transform both to original scale
train_preds_original <- exp(train_preds) - 1
yhats_preds_original <- exp(yhats_preds) - 1

# Then redo the long format with back-transformed values
reg_long <- as.data.frame(train_preds_original) |>
  setNames(1:12) |>
  mutate(year = test_years) |>
  pivot_longer(-year, names_to = "month", values_to = "pred_reg") |>
  mutate(month = as.numeric(month))

hist_long <- as.data.frame(t(yhats_preds_original)) |>
  setNames(1:12) |>
  mutate(year = test_years) |>
  pivot_longer(-year, names_to = "month", values_to = "pred_hist") |>
  mutate(month = as.numeric(month))

# Re-join and replot
plot_dat <- actual_long |>
  left_join(reg_long, by = c("year", "month")) |>
  left_join(hist_long, by = c("year", "month")) |>
  mutate(year = factor(year))

ggplot(plot_dat, aes(x = month, color = year, group = year)) +
  geom_line(aes(y = actual), linewidth = 0.8) +
  geom_line(aes(y = pred_reg), linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(y = pred_hist), linetype = "dotted", linewidth = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Actual (solid) vs Regular (dashed) vs Historical (dotted) Predictions",
    x = "Month", y = "Mean Fire Size", color = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#REG V ACTUAL

ggplot(plot_dat, aes(x = month, color = year, group = year)) +
  geom_line(aes(y = actual), linewidth = 0.8) +
  geom_line(aes(y = pred_reg), linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Actual (solid) vs Regular Model Predictions (dashed)",
    x = "Month", y = "Mean Fire Size", color = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

