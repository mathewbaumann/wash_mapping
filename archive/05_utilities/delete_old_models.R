# Delete old rundates

# Define indicator group
indicator_group <- 'wash'

# Define run dates that need to be kept across all indicators in the indicator
# group
needed <- c('2017_11_01_16_42_25', '2017_12_14_11_01_03', '2018_01_03_22_27_09', 
			'2018_01_03_22_27_10', '2018_01_03_22_27_13', '2018_01_03_22_27_14',
	  		'2018_01_04_10_21_31', '2018_01_04_10_21_32', '2018_01_04_10_21_33')


# Clean up
setwd(paste0('/share/geospatial/mbg/', indicator_group, '/'))
indicators <- list.files()

for (i in indicators) {
	print(i)
	setwd(paste0('/share/geospatial/mbg/', indicator_group, '/', i, '/output'))
	deletion <- setdiff(list.files(), needed)

	for (j in deletion) {
		print(paste0('rm -rf ',j))
		system(paste0('rm -rf ',j))
	}
}