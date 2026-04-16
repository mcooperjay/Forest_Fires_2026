library(ggplot2)
library(tidyverse)
library(refund)

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
HistPlotData$beta[HistPlotData$s < HistPlotData$t] <- NA

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

## Historical pffr (bottom plot)
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



# what is the model set up is
# Try leaving out the most recent 5 years and run prediction, one without s<t and one with
# com
# Take out last 5 years and then re run the model using the bigger dataset without the
