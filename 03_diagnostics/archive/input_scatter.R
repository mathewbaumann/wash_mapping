setwd('/home/j/WORK/11_geospatial/10_mbg/input_data/wash')

library(dplyr); library(ggplot2)

mydat <- read.csv('w_piped.csv')
mydat <- mydat %>%
            mutate(weighted_n = N*weight) %>%
            rename(prop = prop, survey_series = source)

pdf(paste0('/home/adesh/Documents/wash/plots/w_piped',Sys.Date(),'.pdf'))
for (i in unique(mydat$country)) {
    plotdat <- filter(mydat, country == i)
    print(ggplot(plotdat) +
            geom_point(aes(x = weighted_n, y = prop, col = survey_series)) +
            ggtitle(i) +
            theme_bw()
         )
}
dev.off()