#' @title Fit model at tier level
#' @description Fits spatio-temporal models to data at a tier level for benthic groups.
#' Selects and fits model types 1 to 6 based on `MODEL_TYPE` and scales up the predictions.s
#' @examples
#' model_fitModelTier_v2()
#' @author Julie Vercelloni
#' @export
model_fitModelTier_v2 <- function() {
  # status::status_try_catch(
  # {

  # ---- Load input data tables for modelling ----
  reefCloudPackage::load_data_for_model()  

  # ---- Define target benthic group ----
  # For now, modelling "hard corals" only
  # for (GROUP in GROUPS) {   # benthic groups
  GROUP <- GROUPS[[2]] 

    # ---- Prepare data for modelling ----
    data.grp <- reefCloudPackage::prep_group_data_for_modelling(data, GROUP) 

    # ---- Model Type 1: Simple cell means ----
    if (MODEL_TYPE == 1) {
      model_fitModelTier_type1(data.grp)
      reefCloudPackage::scale_up_pred("type1")
    }

    # ---- Model Type 2: Quasi-spatiotemporal (INLA) ----
    if (MODEL_TYPE == 2) {
      reefCloudPackage::model_fitModelTier_type2(data.grp)
      reefCloudPackage::scale_up_pred("type2")
    }

    # ---- Model Type 3: Spatio-temporal with buffering ----
    if (MODEL_TYPE == 3) {
      reefCloudPackage::model_fitModelTier_type3(data.grp)
      reefCloudPackage::scale_up_pred("type3")
    }

    # ---- Model Type 4 ----
    if (MODEL_TYPE == 4) {
      reefCloudPackage::model_fitModelTier_type4(data.grp)
      reefCloudPackage::scale_up_pred("type4")
    }

    # ---- Model Type 5: Full spatio-temporal with covariates ----
    if (MODEL_TYPE == 5) {
      reefCloudPackage::model_fitModelTier_type5(data.grp, tier.sf)
      reefCloudPackage::scale_up_pred("type5")
    }

    # # ---- Model Type 6: Hybrid model (auto switch between type5 and type2) ----
     if (MODEL_TYPE == 6) {
       # Define focal tier (e.g. Tier4)
       FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)

       # Filter focal tier based on data volume (≥3 sites, ≥2 years)
       data.grp.enough <- reefCloudPackage::filter_focaltier(data.grp, FOCAL_TIER, n.spat = 3, n.temp = 2)$filtered_data
       reefCloudPackage::model_fitModelTier_type5_v2(data.grp.enough, tier.sf) 

       # Filtered-out tiers (insufficient data) 
       # Define focal tier (e.g. Tier4)
       FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
       data.grp.not.enough <- reefCloudPackage::filter_focaltier(data.grp, FOCAL_TIER, n.spat = 3, n.temp = 2)$removed_tiers
       reefCloudPackage::model_fitModelTier_type6(data.grp.not.enough, tier.sf)

       # Scale-up predictions
       reefCloudPackage::scale_up_pred("type6") 

       # Attribute changes - effect size of disturbances
       reefCloudPackage::attribute_changes()
     }

  # stage_ = 4,
  # order_ = 15,
  # name_ = "Model fitting complete; predictions, changes attribution and contrasts saved to AWS bucket",
  # item_ = "model_fit_save_complete"

  # }
}
