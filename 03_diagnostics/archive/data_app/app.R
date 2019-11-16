library(shiny)
library(feather)
library(dplyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(ggrepel)
library(plotly)

rm(list = ls())
setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

#ptdat <- read_feather('ptdat_water_unconditional_country_clean_2017_09_29.feather')
#ptdat$point <- 'pt'
#polydat <- read_feather('polydat_water_unconditional_country_clean_2017_09_29.feather')
#polydat$point <- 'poly'
#isodat <- rbind(ptdat, polydat)

colors <- rev(brewer.pal(10, "RdYlBu"))

# Define UI
ui <- fluidPage (
  
  titlePanel("Geospatial Data Validation Tool"),

  fluidRow(
    column(8, leafletOutput('mymap', width = '100%', height = '1000')),

    column(4,
      plotlyOutput("iso_plot"),
      plotlyOutput("scatter_plot")
      )
  ),

  fluidRow(
	column(3,
		radioButtons("drive", h4("Select Drive for MBG Input Data"),
    		choices = list("J" = '/home/j/', "Share" = '/share/'))
		),
	column(3,
		  selectizeInput("indi", h4("Select Indicator"),
    		choices = NULL, multiple = F)
		),
	column(3,
		  selectizeInput("geo", h4("Select ISO3/Country"),
    		choices = NULL,
			multiple = T)
		),
	column(3,
		  selectizeInput("nid", h4("Select NID"),
    		choices = NULL,
			multiple = T)
		)
  ),

  fluidRow(
  	textInput('collapse_path', h4("Enter Path to Country-Level Data")))
)

# Define server logic
server <- function(input, output, session) {
	observeEvent(input$drive, {

		print('12345')
		print(input$drive)

		if (input$drive == '/home/j/') {
			setwd('/home/j/WORK/11_geospatial/10_mbg/input_data/')
		} else {
			setwd('/share/geospatial/mbg/input_data')
		}
		
		indis <- gsub('.csv','',list.files(pattern = ".csv"))
		updateSelectizeInput(session, "indi",
			choices = indis, selected = indis[1], server = F)
		print(input$indi)
	})

	readdat <- reactive({
		if (input$indi != '') {
			inputdat <- read.csv(paste0(input$indi, '.csv'))
		}
	})

	read_collapse <- reactive({
		if (input$collapse_path != '') {
			isodat_list <- list(load(paste(input$collapse_path)))
			isodat <- get(isodat_list[[1]])
			return(isodat)
		}
	})

	observeEvent(input$indi, {
		if (input$indi != '') {
			inputdat <- readdat()
			inputdat$weighted_n <- inputdat$N*inputdat$weight
			id_iso <- c('Default',as.character(unique(inputdat$country)))
			
			updateSelectInput(session, "geo",
				choices = id_iso, selected = 'Default')
		}
	})


	observeEvent(input$indi,{
		if (input$indi != '') {
			inputdat <- readdat()
			id_nid <- c('Default',as.character(unique(inputdat$nid)))
			
			updateSelectInput(session, "nid",
				choices = id_nid, selected = 'Default')
		}	
	})

	output$iso_plot <- renderPlotly({
			
			inputdat <- readdat()
			if('source' %in% names(inputdat)) {
				inputdat$survey_series <- inputdat$source
			}
			id_iso <- c('Default',as.character(unique(inputdat$country)))
		
	   		if(input$geo == 'Default') {
	   	 		iso_sub <- id_iso[2]
	   	 	} else {iso_sub <- input$geo}
	   	 	
	   	 	print('hiiii')
			isodat <- read_collapse()
			print(isodat)
			names(isodat)[which(names(isodat) == input$indi)] <- 'indicator'
		    isodat_plot <- filter(isodat, iso3 %in% iso_sub)
			ip <- ggplot(isodat_plot) + 
					geom_point(aes(x = year, y = indicator, shape = point, size = N, label = nid)) +
					geom_smooth(aes(x = year, y = indicator, col = 'piped', weight = N),
								method = glm, size = 0.5, se = F, fullrange = F) +
					#geom_text_repel(aes(x = year_start, y = piped, label = nid)) +
					ylim(0,1) + xlim(1998,2015) + 
					xlab('Year') + ylab('Prevalence') + theme_bw()
			ggplotly(ip)
		})
	

    output$scatter_plot <- renderPlotly({
    	if (input$indi != '') {
    		inputdat <- readdat()
			inputdat$weighted_n <- inputdat$N*inputdat$weight
			inputdat$prop <- inputdat[,input$indi]/inputdat$N
			if('source' %in% names(inputdat)) {
				inputdat$survey_series <- inputdat$source
			}

			if (!('nid' %in% names(inputdat))) {
		   		inputdat$nid <- NA
		   	}
			id_iso <- c('Default',as.character(unique(inputdat$country)))
		
	   		if(input$geo == 'Default') {
	   	 		iso_sub <- id_iso[2]
	   	 	} else {iso_sub <- input$geo}

	   	 	if(input$nid == 'Default') {
		   	 	nid_sub <- inputdat$nid
		   	} else {
		    	nid_sub <- input$nid
		   	}

        	plotdat <- filter(inputdat, country %in% iso_sub, nid %in% nid_sub)
        	plotdat$survey_series <- substr(plotdat$survey_series, 1, 16)
        	sp <- ggplot(plotdat) +
        			geom_point(aes(x = weighted_n, y = prop, col = survey_series, label = nid)) +
        			theme_bw()
        	ggplotly(sp)
        }
    })

    output$mymap <- renderLeaflet({

    		inputdat <- readdat()
			id_iso <- c('Default',as.character(unique(inputdat$country)))
			inputdat$weighted_n <- inputdat$N*inputdat$weight
			inputdat$prop <- round(inputdat[,input$indi]/inputdat$N)
			inputdat$latitude <- as.numeric(inputdat$latitude)
			inputdat$longitude <- as.numeric(inputdat$longitude)
			if('source' %in% names(inputdat)) {
				inputdat$survey_series <- inputdat$source
			}
    	  	if(input$geo == 'Default') {
		   	 	iso_sub <- id_iso[2]
		   	 } else {
		   	 	iso_sub <- input$geo
		   	 }

		   	if(input$nid == 'Default') {
		   	 	nid_sub <- inputdat$nid
		   	} else {
		    	nid_sub <- input$nid
		   	}

		   	if (!('nid' %in% names(inputdat))) {
		   		inputdat$nid <- NA
		   	}

	    	mapdat <- filter(inputdat, country %in% iso_sub, nid %in% nid_sub)
	    	my_pal <- colorNumeric(colors, domain = mapdat[, 'prop'], na.color = "transparent")
	    	leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>%
	    		addCircleMarkers(data = mapdat, 
	                       radius = ~(weighted_n) / (quantile(weighted_n, probs=0.9)/2),
	                       fillColor = ~my_pal(prop),
	                       color = 'black',
	                       stroke = TRUE,
	                       weight = 0.5,
	                       fillOpacity = 0.8,
	                       popup = paste("Series: ", mapdat$survey_series, "<br>",
	                                     "Sample: ", mapdat$weighted_n,"<br>",
	                                     "Outcome: ", round(mapdat[, 'prop'], 3), "<br>",
	                                     "NID: ", mapdat$nid)) %>%
		    	addLegend("bottomright",
	              pal = my_pal, 
	              values = mapdat[, 'prop'], 
	              title = "Rate", 
	              opacity = 1)
	    	
	})
}


# Run the app
shinyApp(ui = ui, server = server)
