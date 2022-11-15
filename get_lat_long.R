library(httr)
library(stringr)
library(leaflet)
library(sf)
library(assertthat)
library(jsonlite)


#input_list <- c('US', 'China', 'Australia')
#input_vals <- c(329064917, 1433783686, 	26177413)
input_list <- c('New York', 'London', 'Napoli', 'Tehran')
input_vals <- c(8467513, 	9541000, 967069, 12412414)
input_df <- data.frame(name=input_list, val=input_vals)
input_type <- 'City'
input_ll_type <- 'point'
granularity <- 50
input_colorP <- "YlOrRd"

# check inputs
stopifnot(input_type %in% c('City', 'State', 'Country'))
stopifnot(input_ll_type %in% c('poly', 'point'))
stopifnot(granularity > 0)
stopifnot(is.string(input_colorP))

if (str_detect(input_ll_type, 'poly')) {
  mpoly_result <- c()
  
  for (input in input_list) {
    # replace spaces with '+'
    input <- input %>% str_replace_all(string=., ' ', '+')
    
    # concat to get final query url
    base_url <- 'https://www.openstreetmap.org/geocoder/search_osm_nominatim?query='
    query_url <- paste(c(base_url, input), collapse='')
    
    # get relation number from OSM API
    res <- GET(query_url)
    results <- content(res, as='text')
    results <- results %>% str_extract_all(., 'li class=.+')
    results <- results[[1]]
    
    # Part 1: extract relation number matching input type
    for (result in results) {
      data_pref <- str_extract(result, 'data-prefix=.+?(\\s)')
      # check for match with query
      if (!is.na(str_extract(data_pref, input_type))) {
        rn <- str_extract(result, 'data-id=.+?(\\s)')
        rn <- rn %>% str_replace(., 'data-id=\"', '') %>% 
                str_replace(., '\" ', '')
        break
      }
    }
    
    # Part 2: get latitude and longitude from OSM API
    # TODO: check rn is not null
    poly_base_url <- 'https://polygons.openstreetmap.fr/get_poly.py?id='
    params <- '&params=0'
    poly_url <- paste(c(poly_base_url, rn, params), collapse='')
    
    poly_res <- GET(poly_url)
    poly_ll <- read.table(text=content(poly_res,as='text'), sep='\n')
    
    # Part 3: create polygons for plotting
    mpoly_coords <- list()
    curr_coords <- list()
    count <- 1
    
    for (coords in poly_ll[2:NROW(poly_ll)-1,]) {
      # add to current mpoly list
      if (str_detect(coords, 'END')) {
        if (length(curr_coords) > 1) {
          # verify closed polygon
          if (curr_coords[[length(curr_coords)-1]] != curr_coords[[1]] || 
              curr_coords[[length(curr_coords)]] != curr_coords[[2]]) {
            curr_coords[[length(curr_coords)+1]] <- curr_coords[[1]]
            curr_coords[[length(curr_coords)+1]] <- curr_coords[[2]]
          }
          
          mpoly_coords[[length(mpoly_coords)+1]] <- matrix(as.numeric(curr_coords), ncol=2, byrow=TRUE)
          curr_coords <- list()
          count <- 1
        }
      }
      # check if coords are valid
      else if ((count-1) %% granularity == 0 && nchar(coords) > 1) {
        long = as.numeric(str_split(coords, '\t')[[1]][2])
        lat = as.numeric(str_split(coords, '\t')[[1]][3])
        
        if (!is.na(long) && !is.na(lat)) {
          curr_coords <- append(curr_coords, long)
          curr_coords <- append(curr_coords, lat)
        }
      }
      
      count <- count + 1
    }
    
    mpoly_shape <- st_multipolygon(list(mpoly_coords))
    
    mpoly_result[[length(mpoly_result)+1]] <- mpoly_shape
    
  }
  
  # create sf object to plot
  mpoly_sf <- st_sf(input_df, geometry=mpoly_result)
  
  leaflet(mpoly_sf) %>% 
    addTiles() %>%
    addPolygons(
      fillOpacity = 1, smoothFactor = 0.75,
      color=~colorNumeric(input_colorP, val)(val)
    )
  
} else {
  # single point
  mpoint_list <- c()
  
  for (input in input_list) {
    # replace spaces with '+'
    input <- input %>% str_replace_all(string=., ' ', '+')
    
    # concat to get final query url
    base_url <- 'https://www.openstreetmap.org/geocoder/search_osm_nominatim?query='
    query_url <- paste(c(base_url, input), collapse='')
    
    # get relation number from OSM API
    res <- GET(query_url)
    results <- content(res, as='text')
    results <- results %>% str_extract_all(., 'li class=.+')
    results <- results[[1]]
    
    # Part 1: extract relation number matching input type
    for (result in results) {
      data_pref <- str_extract(result, 'data-prefix=.+?(\\s)')
      # check for match with query
      if (!is.na(str_extract(data_pref, input_type))) {
        rn <- str_extract(result, 'data-id=.+?(\\s)')
        rn <- rn %>% str_replace(., 'data-id=\"', '') %>% 
          str_replace(., '\" ', '')
        break
      }
    }
    
    # Part 2: get latitude and longitude from OSM API
    base_url <- 'https://nominatim.openstreetmap.org/details?osmtype=R&osmid='
    query_url <- paste(c(base_url, rn, '&format=json'), collapse='')
    
    # extract info from OSM API
    res <- fromJSON(query_url)
    
    if ("centroid" %in% names(res)) {
      long <- res$centroid$coordinates[1]
      lat <- res$centroid$coordinates[2]
    } else {
      long <- NULL
      lat <- NULL
    }
    
    if (!is.na(long) && !is.na(lat)) {
      mpoint_list[[length(mpoint_list)+1]] <- st_point(c(long, lat))
    }
  }
  
  mpoint_sf <- st_sf(input_df, geometry=mpoint_list)
  
  leaflet(mpoint_sf) %>% 
    addTiles() %>% 
    addMarkers()
}




