amdp = function(object, X, j, predictfcn, verbose = TRUE, plot = FALSE, ...){

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
	# generate partials
	xj = X[, j]  #fix so predictor can be given as a name.
	if(use_generic){
		actual_prediction = predict(object, X)
	}else{
		actual_prediction = predictfcn(object = object, newdata = X)
	}

	#now create xj-to-predict values
	grid_pts = sort(unique(xj))
	
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
	
	if(!is.null(colnames(X)){
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

