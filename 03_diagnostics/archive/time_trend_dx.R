# Clear Environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(ggrepel)

# Read in input data
mydat <- read.csv('/share/geospatial/mbg/input_data/had_diarrhea_w_kish_2018_05_16.csv',
  stringsAsFactors = F)

# Subset data to modeling period
mydat <- filter(mydat, year >= 2000)

# Calculate weighted means by country-year-nid-source as well as weighted sum of
# sample size post-processing
mydat3 <- mydat %>%
          group_by(svy_id, year, point, country, source) %>%
          summarize(prev = weighted.mean(x = had_diarrhea/N, w = N*weight, na.rm = T),
                    N = sum(weight*N, na.rm = T))

mydat3 <- mutate(mydat3, country = substr(country, 1, 3))
# Specify countries for which to inspect data
s_asia <- c('IND', 'PAK', 'BGD', 'NPL', 'BTN', 'LKA')

# Output plots to PDF at specified path
pdf(paste0('/home/adesh/Documents/stg_2_diarrhea_', Sys.Date(), '.pdf'))
for (j in unique(mydat3$country)) {
  message(j)
  test <- filter(mydat3, country == j)

  if (nrow(test) > 0) {
    print(
      ggplot(test, aes(x = year, y = prev)) +
        geom_point(aes(x = year, y = prev, size = N, shape = as.factor(point),
                       color = source)) +
        geom_text_repel(aes(x = year, y = prev, label = svy_id)) +
        xlim(2000, 2015) + ylim(0, 0.25) +
        ggtitle(j) +
        theme_bw()
    )
  }
}
dev.off()

for (j in unique(mydat3$country)) {
png(paste0('/home/adesh/Documents/stg2_plots/stg_2_diarrhea_', j, '.png'), 1200, 1200)
  message(j)
  test <- filter(mydat3, country == j)

  if (nrow(test) > 0) {
    print(
      ggplot(test, aes(x = year, y = prev)) +
        geom_point(aes(x = year, y = prev, size = N, shape = as.factor(point),
                       color = source), size = 5) +
        geom_text_repel(aes(x = year, y = prev, label = svy_id), size = 5) +
        xlim(2000, 2015) + ylim(0, 0.25) +
        ggtitle(j) +
        theme_bw()
    )
  }
dev.off()
}