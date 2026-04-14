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
             limits = function(s, t) s < t),
  yind = t,
  bs.int = list(bs = "ps", k = 5, m = c(2,1))
)

FullHistBetas <- coef(FullModel_Hist, n1 = 1, n2 = 12)$smterms[[2]]$value

summary(FullModel_Hist)
# R-squared of .584

## Get just our full model (not historical)
FullModel <- pffr(
  Y_log ~ ff(X_mat, 
             xind = s),
  yind = t,
  bs.int = list(bs = "ps", k = 5, m = c(2,1))
)

FullBetas <- coef(FullModel, n1 = 1, n2 = 12)$smterms[[2]]$value

summary(FullModel)
# R-squared of .563

FullModel2 <- FullModel$smterms[[3]]$coef

## Plot the two betas to compare
FullBetasdf <- data.frame('t' = c(FullModel2$X.tmat),
                          's' = c(FullModel2$X.smat),
                          'beta' = c(FullBetas, FullHistBetas),
                          'Fit' = factor(rep(c('Full pffr', 'Historical pffr'), times = c(length(FullModel$X.smat))))
                          )


ggplot(data = FullBetasdf[FullBetasdf$Fit == "Full pffr",]) + 
  geom_raster(aes(x = s, y = t, fill = beta)) + 
  scale_fill_viridis_c() + 
  xlab('s') + ylab('t') + theme_bw() + 
  ggtitle('pffr Surface Fit')


# what is the model set up is
# Try leaving out the most recent 5 years and run prediction, one without s<t and one with
# com
# Take out last 5 years and then re run the model using the bigger dataset without the
