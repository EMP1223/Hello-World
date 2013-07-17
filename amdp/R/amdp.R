amdp = function(object, X, j, predictfcn, verbose = TRUE, plot = FALSE, frac_to_build = 1, plot_logit = F, ...){

	#check for factor
	if (class(X[, j]) == "factor" || class(X[, j]) == "character"){
		stop("AMDP does not support factor attributes")
	}
	
	#warning
	if(plot_logit){
		warning("logit only defined for binary classification")
	}
	
	######## (1) check inputs
	# (a) check for valid prediction routine...
	if(!missing(predictfcn)){
		fcn_args = names(formals(predictfn))
		if(!("object" %in% fcn_args && "newdata" %in% fcn_args)){
			stop("predictfcn must have 'object' and 'newdata' arguments")
		}
		else{
			use_generic = FALSE
		}
	}else{ 
		#check for default prediction associated with class(object)
		#this is harder than it should be because some classes have class(object)
		#return a vector (randomForest, for instance).
		classvec = class(object)
		found_predict = 0; i = 0
		while(!found_predict && i <= length(classvec)){
			fcns = methods(class=classvec[i])
			found_predict = found_predict + length(grep("predict", fcns))
			i = i+1
		}
		if(found_predict == 0){
			stop("No generic predict method found this object.")
		}
		else{
			use_generic = TRUE
		}
	}

	######## (2)
	N = nrow(X)
	# grid points
	#now create xj-to-predict values
	xj = X[, j]  #fix so predictor can be given as a name. 
	grid_pts = sort(X[,j])
	
	# check fraction to build
	if(frac_to_build < 1){
		# we don't sample randomly -- we ensure uniform sampling across
		# quantiles of xj so as to not leave out portions of the dist'n of x.
		order_xj = order(xj)
		X = X[order_xj, ]  #ordered by column xj 	
		nskip = round( (1 / frac_to_build) )
		X = X[seq(1, N, by = nskip), ]
		xj = X[, j]
	}
	
	# generate partials
	if(use_generic){
		actual_prediction = predict(object, X)
	}else{
		actual_prediction = predictfcn(object = object, newdata = X)
	}
	if(plot_logit){
		actual_prediction = log(actual_prediction) - (1/2)*(log(actual_prediction)+log(1 - actual_prediction)) 
	}
	
	apdps = matrix(NA, nrow=nrow(X), ncol=length(grid_pts))
	colnames(apdps) = grid_pts
	#pred_test_values = seq(from = min_xj_seq, to = max_xj_seq, by = (max_xj_seq - min_xj_seq) / (num_grid_pts - 1))
	
	for (t in 1 : length(grid_pts) ){
		X[, j] = grid_pts[t]
		if(use_generic){
			apdps[, t] = predict(object, X, ...)
		}
		else{
			apdps[, t] = predictfcn(object=object, newdata=X)
		}
		
		if(verbose){cat(".")}			
	}
	if(verbose){cat("\n")}
	
	if(!is.null(colnames(X))){
		predictor = colnames(X)[j]
	}else{
		predictor = j
	}
	obj_to_return = list(apdps=apdps, gridpts = grid_pts, predictor = predictor, xj = xj,
						 actual_prediction = actual_prediction)
	class(obj_to_return) = "amdp"
	
	if(plot){	#call plot function of our definition.
		cat("plotting goes here \n")
	}
	
	invisible(obj_to_return)
}