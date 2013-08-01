regr_test = function(apdp_obj, inter = FALSE){
	x = apdp_obj$pred_test_values  #the grid
	curves = apdp_obj$apdps
	curve_var = factor(1:nrow(curves))
	grid_var  = factor(1:length(x))
	
	Y = as.vector(t(curves))
	curve = rep(curve_var,each=length(x))
	gridpt = rep(grid_var, nrow(curves))
	if(inter){
		fit = lm(Y~curve * gridpt + 0)
	}else{
		fit = lm(Y~curve + gridpt + 0)
	}
	return(fit)
}
