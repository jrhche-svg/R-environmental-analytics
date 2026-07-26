# GGPlot Visuals Demo
# Author: James R. Henderson, MS, PE







# Load required packages
library(ggplot2)
library(dplyr)
library(gcookbook) # For example datasets
library(nlme)      # For Oxboys dataset

#---------------------------#
# 1. Multiple Subjects in a Plot
#---------------------------#
data(Oxboys, package = "nlme")  # Load the Oxboys dataset
head(Oxboys)

ggplot(Oxboys, aes(x = age, y = height, group = Subject)) +  # Corrected 'subject' to 'Subject'
  geom_point() +
  geom_line() +
  labs(title = "Height Growth by Age for Each Subject",
       x = "Age (years)",
       y = "Height (cm)") +
  theme_minimal()

#---------------------------#
# 2. Boxplots with Ordering
#---------------------------#
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  labs(title = "Highway Mileage by Vehicle Class",
       x = "Vehicle Class",
       y = "Highway MPG") +
  theme_minimal()

# Reordering categories based on median highway mileage
ggplot(mpg, aes(x = reorder(class, hwy), y = hwy)) +
  geom_boxplot() +
  labs(title = "Ordered Highway Mileage by Vehicle Class",
       x = "Vehicle Class (Ordered by Median MPG)",
       y = "Highway MPG") +
  theme_minimal()

#---------------------------#
# 3. Scale Transformations
#---------------------------#
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  scale_y_continuous(trans = "reciprocal") +
  labs(title = "Highway MPG vs. Engine Displacement",
       x = "Engine Displacement (L)",
       y = "Highway MPG (Reciprocal Scale)") +
  theme_minimal()

# Logarithmic transformation for better readability
ggplot(diamonds, aes(x = price, y = carat)) +
  geom_bin2d() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(title = "Diamond Price vs. Carat (Log Scale)",
       x = "Price (Log10)",
       y = "Carat (Log10)") +
  theme_minimal()

#---------------------------#
# 4. Time Series Visualization
#---------------------------#
cut_depth <- diamonds %>%
  group_by(cut, depth) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(depth > 55, depth < 70)

ggplot(cut_depth, aes(x = depth, y = n, colour = cut)) +
  geom_line() +
  labs(title = "Distribution of Diamond Depth by Cut",
       x = "Depth",
       y = "Count") +
  theme_minimal()

# Density plot for depth distribution
ggplot(diamonds, aes(x = depth, fill = cut)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Diamond Depth by Cut",
       x = "Depth",
       y = "Density") +
  theme_minimal()

#---------------------------#
# 5. Cleveland Dot Plot
#---------------------------#
tophit <- tophitters2001[1:25,]
tophit$name <- factor(tophit$name, levels = tophit$name[order(tophit$lg, tophit$avg)])

ggplot(tophit, aes(x = avg, y = name)) +
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = lg)) +
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) +
  labs(title = "Top Baseball Hitters by League",
       x = "Batting Average",
       y = "Player Name") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1, 0.5),
        legend.justification = c(1.2, .2))

#---------------------------#
# 6. Heatmap Visualization
#---------------------------#
prez_rating <- data.frame(
  rating = as.numeric(presidents),
  year = as.numeric(floor(time(presidents))),
  quarter = as.numeric(cycle(presidents))
)

ggplot(prez_rating, aes(x = year, y = quarter, fill = rating)) +
  geom_raster() +
  scale_x_continuous(breaks = seq(1940, 1976, by = 4)) +
  labs(title = "Presidential Approval Ratings by Year & Quarter",
       x = "Year",
       y = "Quarter",
       fill = "Rating") +
  theme_minimal()

#---------------------------#
# 7. Mapping Continuous Variables to Aesthetics
#---------------------------#
cdat <- subset(countries, Year == 2009 &
                 Name %in% c("Canada", "Ireland", "United Kingdom", "United States",
                             "New Zealand", "Iceland", "Japan", "Luxembourg",
                             "Netherlands", "Switzerland"))

# Scatter plot mapping GDP to color
ggplot(cdat, aes(x = healthexp, y = infmortality, colour = GDP)) +
  geom_point() +
  labs(title = "Infant Mortality vs. Health Expenditure",
       x = "Health Expenditure per Capita",
       y = "Infant Mortality Rate",
       colour = "GDP") +
  theme_minimal()

# Scatter plot mapping GDP to size
ggplot(cdat, aes(x = healthexp, y = infmortality, size = GDP)) +
  geom_point(shape = 21, colour = "black", fill = "cornsilk") +
  scale_size_area(max_size = 15) +
  labs(title = "Infant Mortality vs. Health Expenditure (Bubble Plot)",
       x = "Health Expenditure per Capita",
       y = "Infant Mortality Rate",
       size = "GDP") +
  theme_minimal()
