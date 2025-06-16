#' @title Fit model at tier level
#' @title Annual Contrasts with Lags Matrix
#' @description Constructs a matrix representing annual contrasts with specified lags for a vector of values.
#' Each column corresponds to a contrast between year i and year i + lag.
#' 
#' @param x A vector of values (e.g., years or time points).
#' @param lags Integer or vector of integers indicating the lag steps between values for contrasts. Default is 1.
#' 
#' @return A matrix with rows corresponding to input values and columns to contrasts between lagged pairs.
#' 
#' @examples
#' years <- 2010:2015
#' annual.contrasts.lags(years, lags = c(1,2))
#' 
#' @author Julie Vercelloni
#' @export
#' 

#https://github.com/ReefCloud/reefCloud/blob/master/scripts/ReefCloud_functions.R (L1368-1497)

make_contrasts <- function() {
  # status::status_try_catch(
  # {

      files <- list.files(
      path = paste0(DATA_PATH, "modelled"),
      pattern = "output", 
      full.names = TRUE
    )

    data.list <- vector('list', length(files))
    contrasts_list <- list()

    for (i in seq_along(data.list)) {
      tier <- str_extract(files[i], "output\\d+")
      obj <- readRDS(files[i])
      contrasts_list[[i]] <- obj
    }

contrast_matrices <- map(contrasts_list, function(df) {
  
  # Identify the 'Tier' column name dynamically
  tier_col <- names(df)[str_detect(names(df), "^Tier")]

  if (length(tier_col) != 1) {
    warning("Unexpected number of Tier columns in df")
    return(NULL)
  }

  # Group by the Tier column and apply contrasts
  df %>%
    group_by(.data[[tier_col]]) %>%
    group_map(~{
      year_vec <- .x$fYEAR
      contrast_mat <- annual_contrasts_lags(   year_vec, lags = 1)
      list(tier = unique(.x[[tier_col]]), contrasts = contrast_mat)
    })
})

 lags <- c(1,2,5)
  years <- rev(sort(unique(cellmeans.tier5$fYEAR)))
  xmat <- annual.contrasts.lags(years, lags = lags) 
  lag.change <-
    cellmeans %>% 
    group_by(Tier5, Rep) %>% 
    arrange(desc(fYEAR)) %>% 
    summarise(
      frac = exp(as.vector(as.vector(log(value)) %*% as.matrix(xmat))),
      value = as.vector(as.vector(value) %*% as.matrix(xmat)),
      YearComp = ifelse(ncol(xmat)>1, colnames(xmat), as.character(years)),
      ## lag = rep(lags, each = max(lags))
      lag = ifelse(ncol(xmat) > 1, extract_lags_from_colnames(xmat), NA)
    ) %>% 
    dplyr::select(YearComp, lag, Tier5, Rep, value, frac) %>% 
    separate(YearComp, into = c("Year1", "Year2"), sep = " - ",
      remove = FALSE) %>% 
    ungroup() %>% 
    left_join(tiers.lookup %>% dplyr::select(Tier5, Tier4, reef_area) %>% distinct()) 

  lag.change.tier5.sum <- lag.change %>%
    group_by(YearComp, lag, Tier5) %>%
    summarise(
      change.value = median_hdci(value),
      change.frac = median_hdci(frac),
      Pl = mean(value < 0),
        Pg = mean(value > 0)) %>%
    unnest("change.value") %>%
    dplyr::select(YearComp,
      lag,
      Tier5,
      median = y,
      lower = ymin,
      upper = ymax,
      Pl, Pg) %>%
    ungroup() %>% 
    mutate(change = ifelse(Pl > 0.90, "Decrease",
      ifelse(Pg > 0.90, "Increase", "No change")))  %>%
    mutate(change = ifelse(median == 0 & lower == 0 & upper == 0,
  "Not calculated", change)) %>%
    separate(YearComp, into = c("fYEAR", "Year2"), sep = " - ",
      remove = FALSE)
  ## number of tier 5's with evidence of change
  lag.change.count <- lag.change %>% 
    group_by(YearComp, lag, Tier5) %>%
    summarise(
      flag = ifelse(all(value == 0), TRUE, FALSE),
      Pl = mean(value < 0),
      Pg = mean(value > 0)) %>%
    ungroup() %>%
    group_by(YearComp, Tier5, lag) %>%
    summarise(
      Cl = sum(Pl > 0.90 & !flag),
      Cg = sum(Pg > 0.9 & !flag),
      N_decrease = Cl/sum(!flag),
      N_increase = Cg/sum(!flag)) %>%
    ungroup() %>%
    suppressMessages()
  lag.change.tier5.sum <- lag.change.tier5.sum %>%
    full_join(lag.change.count %>%
                dplyr::select(YearComp, lag, Tier5, N_decrease, N_increase),
      by = c("YearComp", "lag", "Tier5") ) %>%
    dplyr::select(fYEAR, lag, Tier5, change,
    `%_dec` = N_decrease, `%_inc` = N_decrease) %>%
    pivot_wider(names_from = "lag",
      names_prefix = "lag_",
      values_from = c("change", "%_dec", "%_inc"))

    lag.change.tier5.sum <- lag.change.tier5.sum %>%
    full_join(lag.change.count %>%
                dplyr::select(YearComp, lag, Tier5, N_decrease, N_increase),
      by = c("YearComp", "lag", "Tier5") ) %>%
    dplyr::select(fYEAR, lag, Tier5, change,
    `%_dec` = N_decrease, `%_inc` = N_decrease) %>%
    pivot_wider(names_from = "lag",
      names_prefix = "lag_",
      values_from = c("change", "%_dec", "%_inc"))
  # status metadata
  # stage_ = 4,
  # order_ = 14,
  # name_ = "Annual contrast compiled; constrast table saved to AWS bucket",
  # item_ = "compiled_save_contrasts"

  # }

}

