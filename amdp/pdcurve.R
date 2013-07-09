#creates curves.

pdcurve = function(object, X, j, subsamp=FALSE, xgrid=NULL,predictfcn){

	######## (1) check inputs
	# (a) check for valid prediction routine...
	if(!is.missing(predictfcn)){
		fcn_args = names(formals(predictfn))
		if(!("object" %in% fcn_args && "newdata" %in% fcn_args)){
			stop("predictfcn must have 'object' and 'newdata' arguments")
		}
	}else{ 
		#check for default prediction associated with class(object)
		#this is harder than it should be because some classes have class(object)
		#return a vector (randomForest, for instance).
		classvec = class(object)
		found_predict = 0; i = 0
		while(!found_predict && i < length(classvec)){
			fcns = methods(class=classvec[i])
			found_predict = found_predict + length(grep("predict", fcns))
			i = i+1
		}
		if(found_predict == 0){
			stop("No generic predict method found this object.")
		}
	}

	######## (2) 
	xj = X[, j]  #fix so predictor can be given as a name.
	n = nrow(X)

	
	#now create xj-to-predict values
	min_xj = NULL
	max_xj = NULL
	vals = unique(xj)
	if (length(vals) <= ORD_VAR_MAX_NUM_LEVELS){
		pred_test_values = vals
		#override num_grid_pts because there's not too many vals
		num_grid_pts = length(pred_test_values)
	} else {
		min_xj = min(xj, na.rm = TRUE)
		max_xj = max(xj, na.rm = TRUE)
		pred_test_values = seq(from = min_xj_seq, to = max_xj_seq, by = (max_xj_seq - min_xj_seq) / (num_grid_pts - 1))		
	}
} 


