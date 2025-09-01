#' Extract and Summarise Regional Information from Model Outputs
#'
#' This function summarises regional information from the model output data by tier. 
#' It computes total area, year range, proportion of observations from data vs modelled ("new") tiers, 
#' and contribution from each model type (e.g., FRK or INLA).
#'
#' @param post_dist_df_all A data frame of model outputs including predicted values, model types, tiers, and other metadata.
#' @param tier_col A character string specifying the tier level column to group and summarise by (e.g., "Tier4").
#'
#' @return A data frame summarising the tier-wise area, temporal coverage, and model/data contributions.
#' @author Julie Vercelloni
#' @export

extract_info_region <- function(post_dist_df_all, tier_col) {
## Extract sum_area 

sum_area <- reefCloudPackage::get_sum_area(post_dist_df_all, tier_col) %>%
  arrange(!!sym(tier_col))

## Extract year range 

year_range <- post_dist_df_all %>% group_by(!!sym(tier_col)) %>%
  mutate(
    fYEAR_numeric = as.numeric(as.character(fYEAR))) %>%
  summarize(
    year_range = paste0(min(fYEAR_numeric, na.rm = TRUE), "–", max(fYEAR_numeric, na.rm = TRUE))
  ) %>%
  dplyr::select(!!sym(tier_col), year_range) %>%
  arrange(!!sym(tier_col))


# Extract % of data tier 
data_perc <- post_dist_df_all %>% 
  group_by(!!sym(tier_col)) %>%
  dplyr::count(tier_type) %>%
  dplyr::mutate(prop = (n / sum(n))*100) %>%
  dplyr::select(!!sym(tier_col), tier_type, prop) %>%
  pivot_wider(names_from = tier_type, values_from = prop, values_fill = 0) %>%
  arrange(!!sym(tier_col))

# Extract % of model types 
model_perc <- post_dist_df_all %>% 
  group_by(!!sym(tier_col)) %>%
  dplyr::count(model_name) %>%
  dplyr::mutate(prop = (n / sum(n))*100) %>%
  dplyr::select(!!sym(tier_col), model_name, prop) %>%
  pivot_wider(names_from = model_name, values_from = prop, values_fill = 0) %>%
  arrange(!!sym(tier_col))


# List of tables to join
tables <- list(sum_area, year_range, data_perc, model_perc)

# Iteratively left join all by `tier_col`
all_info <- reduce(tables, left_join, by = tier_col) %>%
  dplyr::rename(
    Size.area = sum_area,
    Year.range = year_range,
    data.tier = data,
    new.tier = new
  )

# ensure FRK and INLA exist
if (!"FRK" %in% names(all_info)) all_info$FRK <- 0
if (!"INLA" %in% names(all_info)) all_info$INLA <- 0

all_info <- all_info %>%
  rename(FRK.prop = FRK, INLA.prop = INLA)
  
return(all_info)
}
