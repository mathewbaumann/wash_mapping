
cape_town <- c(18.584642, -33.919183)
joburg <- c(28.072928, -26.18445)
durban <- c(31.002417, -29.840618)
port_liz <- c(25.505444, -33.932381)
windhoek <- c(17.074927, -22.572618)

kinshasa <- c(15.311065, -4.346519)
kisangani <- c(25.188174, 0.518850)
libreville <- c(9.437718, 0.417733)
bangui <- c(18.554024, 4.371717)
pt_noire <- c(11.850361, -4.798711)


cssa_list <- list(kinshasa, kisangani, libreville,
                  bangui, pt_noire)
names(cssa_list) <- c('kinshasa', 'kisangani', 'libreville',
                      'bangui', 'pt_noire')
sssa_list <- list(cape_town, joburg, durban,
                  port_liz, windhoek)
names(sssa_list) <- c('cape_town', 'joburg', 'durban',
                      'port_liz', 'windhoek')

for (i in 1:2) {
  plot_list <- list(cssa_list, sssa_list)[[i]]
  plot_results <- list()
  
  for (j in 1:length(plot_list)) {
    plot_point <- SpatialPoints(coords = data.frame(plot_list[[j]][1],
                                                    plot_list[[j]][2]))
    
    results_vec <- c()
    for (k in 1:16) {
      results_vec[k] <- raster::extract(results[[k]], plot_point)
    }
    plot_results[[j]] <- data.frame(mean = results_vec, city = names(plot_list)[j],
                                    year = 2000:2015)
  }
  
  plot_results <- do.call(rbind, plot_results)
  print(ggplot(plot_results) + geom_line(aes(x = year, y = mean, col = city)) +
          ylim(0,1) +
          theme_bw())
}





iquitos <- SpatialPoints(coords = data.frame())
iquitos_values <- c()
for (i in 1:16) {
  
  iquitos_values[i] <- raster::extract(results[[i]], iquitos)
  
}
plot(iquitos_values, type = 'l')
