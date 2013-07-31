MAX_NUM_UNIQUE_PTS_NOMINAL = 5

amdp = function(object, X, y,
		predictor, predictfcn, newdata, 
		verbose = TRUE, plot = FALSE, 
		frac_to_build = 1, indices_to_build = NULL, 
		num_grid_pts, logodds = F, ...){

	#check for factor
	if (class(X[, predictor]) == "factor" || class(X[, predictor]) == "character"){
		stop("AMDP does not support factor attributes")
	}
	
	if(!is.numeric(frac_to_build) || frac_to_build > 1 || frac_to_build < 0 ){
		stop("frac_to_build must be in (0, 1]")
	}

	######## (1) check inputs
	# (a) check for valid prediction routine...
	if (!missing(predictfcn)){
		fcn_args = names(formals(predictfcn))
		if (!("object" %in% fcn_args && "newdata" %in% fcn_args)){
			stop("predictfcn must have 'object' and 'newdata' arguments")
		} else {
			use_generic = FALSE
		}
	} else { 
		#check for default prediction associated with class(object)
		#this is harder than it should be because some classes have class(object)
		#return a vector (randomForest, for instance).
		classvec = class(object)
		found_predict = 0; i = 0
		while (!found_predict && i <= length(classvec)){
			fcns = methods(class=classvec[i])
			found_predict = found_predict + length(grep("predict", fcns))
			i = i+1
		}
		if (found_predict == 0){
			stop("No generic predict method found this object.")
		} else {
			use_generic = TRUE
		}
	}

		
	######## (2)
	N = nrow(X)
	# grid points
	#now create xj-to-predict values
	xj = X[, predictor]  #fix so predictor can be given as a name. 
	grid_pts = sort(X[, predictor])
	
	# check fraction to build
	if (frac_to_build < 1){
		# we don't sample randomly -- we ensure uniform sampling across
		# quantiles of xj so as to not leave out portions of the dist'n of x.
		order_xj = order(xj)
		X = X[order_xj, ]  #ordered by column xj 	
		nskip = round(1 / frac_to_build)
		X = X[seq(1, N, by = nskip), ]
		xj = X[, predictor]
		grid_pts = sort(xj)
	} else if (!missing(indices_to_build)){
		if (frac_to_build < 1){
			stop("\"frac_to_build\" and \"indices_to_build\" cannot both be specified simultaneously")
		}
		X = X[indices_to_build, ]
		xj = X[, predictor]
		grid_pts = sort(xj)		
	}
	
	grid_pts = unique(grid_pts)
	num_unique_pts = length(grid_pts)
	
	
	#now handle less grid pts
	if (!missing(num_grid_pts)){
		if (num_grid_pts > num_unique_pts){
			warning(paste("the number of grid points specified,", num_grid_pts, "is larger than the number of unique values,", num_unique_pts, "(defaulting to number of unique values)"))	
		} else {
			grid_pts = grid_pts[round(seq(from = 1, to = num_unique_pts, length.out = num_grid_pts))]	
		}		
	}
	
	# generate partials
	if (use_generic){
		actual_predictions = predict(object, X)
	} else {
		actual_predictions = predictfcn(object = object, newdata = X)
	}
	if (logodds){	
		min_pred = min(actual_predictions)
		max_pred = max(actual_predictions)
		#do some p_hat \in [0, 1] checking
		if (min_pred < 0){ 
			stop("the logodds option is on but predict returns values less than 0 (these should be probabilities!)")
		} else if (max_pred > 1){
			stop("the logodds option is on but predict returns values greater than 1 (these should be probabilities!)")
		}
		if (min_pred == 0){
			second_lowest = min(actual_predictions[actual_predictions > 0])
			if (is.na(second_lowest)){ 
				second_lowest = .0001
			}
			actual_predictions[(actual_predictions == 0)] = .5 * second_lowest
		}
		actual_predictions = log(actual_predictions) - (1 / 2) * (log(actual_predictions) + log(1 - actual_predictions))
		 
	}
	
	apdps = matrix(NA, nrow = nrow(X), ncol = length(grid_pts))
	colnames(apdps) = grid_pts
	#pred_test_values = seq(from = min_xj_seq, to = max_xj_seq, by = (max_xj_seq - min_xj_seq) / (num_grid_pts - 1))

	#Compute actual pdp. Note that this is averaged over the observations
	#we sample, so this might be different from the 'true' pdp if frac_to_build < 0.
	for (t in 1 : length(grid_pts) ){
		X[, predictor] = grid_pts[t]
		if (use_generic){
			apdps[, t] = predict(object, X, ...)
		}
		else{
			apdps[, t] = predictfcn(object = object, newdata = X)
		}
		
		if(verbose){cat(".")}			
	}
	#return X to its original state.
	X[ ,predictor] = xj

	#do logit if necessary
	if (logodds){
		#prevent log(0) error
		min_val = min(apdps)
		if (min_val < 0){
			stop("logodds is TRUE but predict returns negative values (these should be probabilities!)")
		} 
		if (min_val == 0){
			second_lowest = min(apdps[apdps > 0])
			if (is.na(second_lowest)){ 
				second_lowest = .0001 #arbitrary epsilon value
			} 
			apdps[(apdps == 0)] = (.5 * second_lowest) #arbitrarily, halfway between 0 and second_lowest
		}
		apdps = log(apdps) - (1 / 2) * (log(apdps) + log(1 - apdps)) 
	}
	if (verbose){cat("\n")}
	
	if (class(predictor) != "character"){
		xlab = paste("x", predictor, sep = "_")  #x_1, x_2 etc.
	} else {
		xlab = predictor #the actual name of the feature.
	}
	
	if (num_unique_pts <= MAX_NUM_UNIQUE_PTS_NOMINAL){
		nominal_axis = TRUE
	} else {
		nominal_axis = FALSE
	}	

	if(!missing(y)){
		range_y = max(y) - min(y)
	}else{
		range_y = (max(apdps)-min(apdps))
		cat("y not passed, so range_y is range of amdps\n")
	}

	amdp_obj = list(apdps = apdps, gridpts = grid_pts, predictor = predictor, xj = xj, actual_prediction = actual_predictions, 
			logodds = logodds, xlab = xlab, nominal_axis = nominal_axis, N = N, range_y = range_y, Xamdp=X)
	class(amdp_obj) = "amdp"
	
	if (plot){	#if the user wants to use a default plotting, they can get the plot in one line
		plot(amdp_obj)
	}
	
	invisible(amdp_obj)
}
