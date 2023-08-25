

#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
ReefCloud_addCovariate <- function(data = data, covariate, method = 'nearest') {
    name <- attr(covariate, "name")
    if (method=='nearest') {
        ## Construct a reference grid from the data
        data.sf  <- data %>%
            mutate(ID = interaction(P_CODE, REEF, SITE_NO)) %>%
            group_by(ID) %>%
            summarise(LATITUDE=mean(LATITUDE),
                      LONGITUDE=mean(LONGITUDE)) %>%
            st_as_sf(coords=c('LONGITUDE', 'LATITUDE'), crs=st_crs(4326)) %>%
            ungroup()
        ## convert stars into sf points
        covar.sf <- covariate %>% st_as_sf() #as_points=TRUE
        ## find the nearest feature
        wch <- data.sf %>% st_nearest_feature(covar.sf)
        nms <- colnames(covar.sf)
        nms <- nms[nms != 'geometry']
        ## Add extract the nearest points to the reference grid
        covar <- data.sf %>%
            st_drop_geometry() %>% 
            bind_cols(covar.sf[wch,] %>% st_drop_geometry()) %>%
            pivot_longer(cols=-ID, names_to = 'fYEAR', values_to = name) %>%
            mutate(fYEAR = factor(format(as.Date(fYEAR), "%Y")))
        ## Join extracted data onto data
        data %>% mutate(ID=interaction(P_CODE, REEF, SITE_NO)) %>%
            left_join(covar) %>%
            dplyr::select(!!sym(name)) %>%
            suppressMessages() %>%
            suppressWarnings()
        ## dplyr::select(-ID)
    } else { #'bilinear'
        covar.sf <- covariate
        data.sf <- data %>%
            st_as_sf(coords=c("Longitude", "Latitude"), crs=st_crs(4326)) %>%
            mutate(time = as.Date(paste0(fYEAR, '-01-01')))
        covar.sf %>% st_extract(data.sf, bilinear = FALSE, time_column = "time", interpolate_time = FALSE) %>%
            st_as_sf() %>%
            st_drop_geometry() %>% 
            select({{name}})
        
        
    }
}
