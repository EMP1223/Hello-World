print.amdp = function(amdp_obj){
	cat("AMDP object generated on data with n = ", nrow(amdp_obj$apdps), " for predictor \"", amdp_obj$predictor, "\"\n", sep = "")
	cat("predictor considered", ifelse(amdp_obj$nominal_axis, "discrete", "continuous"), ", logodds", ifelse(amdp_obj$logodds, "on", "off"), "\n", sep = "")
}