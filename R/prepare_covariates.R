#' @title Prepare Covariates
#' @description Loads, processes, and joins covariate data to the benthic dataset. Also fills missing years and saves combined covariate layers for Tier 5 spatial units.
#' @param data The benthic data frame used to align and join covariates.
#' @return NULL. Saves processed covariate data to disk and modifies global state.
#' @examples
#' \dontrun{
#' load("benthic_data.RData")
#' prepare_covariates(benthic_data)
#' }
#' @author Murray Logan
#' @export
prepare_covariates <- function() {
  status::status_try_catch(
  {
    load(file=paste0(DATA_PATH, "processed/", RDATA_FILE))
    load(paste0(DATA_PATH, 'primary/tier', BY_TIER, '.sf.RData'))
    files <- list.files(path = paste0(DATA_PATH, "primary"),
      pattern = "covariate.*.RData$",
      full.names = TRUE)
    files <- gsub("//", "/", files)
    if (length(files)>0) {
      cov_list <- vector("list", length(files)) 
      names(cov_list) <- gsub('.*covariate_(.*).RData', '\\1', files)
      for (f in files) {
        cov_name <- gsub('.*covariate_(.*).RData', '\\1', f)
        cov <- get(load(file = f))

        ## join to benthic data
        data <- data %>% reefCloudPackage::add_cov_to_data(cov, cov_name) 

        ## fill in the missing years for each Tier5 in the covariates
        year_range <- data %>% dplyr::pull(REPORT_YEAR) %>% range()
        full_cov_lookup <- data.frame(year = seq(year_range[1], year_range[2], by =  1)) %>%
          tidyr::crossing(Tier5 = unique(tier.sf$Tier5)) %>%
          dplyr::arrange(Tier5)
        cov_list[[cov_name]] <-
          cov %>% reefCloudPackage::lag_covariates(year_range, full_cov_lookup, cov_name) 
      }
      full_cov <- purrr::reduce(cov_list, function(x, y) {
        dplyr::full_join(x, y, by = c("Tier5", "year"))
       })
      save(full_cov, file=paste0(DATA_PATH, "processed/", "covariates_full_tier5.RData"))
      assign("RDATA_COV_FILE", value = str_replace(RDATA_FILE, "_", "_with_covariates"))
      save(data, file=paste0(DATA_PATH, "processed/", RDATA_COV_FILE)) 
    }
  },
  stage_ = 3,
  order_ = 5,
  name_ = "Prepare covariates",
  item_ = "prepare_covariates"
  )
}
