require(raster)

#' Calculate AIC for MaxEnt model.
#' 
#' Function will calculate AIC, AICc and BIC for MaxEnt model. It is using the procedure
#' as outlined in Warren and Siefert 2011 with ideas of implementation from MaxEnt google 
#' mailing list. Function works on presence data, one layer of prediction scores and a
#' MaxEnt model object where lambda files is extracted from. Implementation hints from
#' https://groups.google.com/forum/#!topic/maxent/EsXZKlpvdTI and reference cited at the end.
#' 
#' @param model MaxEnt model.
#' @param p SpatialPoints or SpatialPointsDataFrame. Presence data.
#' @param sc RasterStack. Prediction (suitability) scores.
#' @references Dan L. Warren and Stephanie N. Seifert 2011. Ecological niche modeling in Maxent: the importance of model complexity and the performance of model selection criteria. Ecological Applications 21:335–342. http://dx.doi.org/10.1890/10-1171.1
#' @author Roman Luštrik
AICmaxent <- function(model, p, sc) {
   # Based on 
   
#  Get number of parameters from lambdas file as per Warren and Seifert 2011:
#  For interpretation of lambda values see pdf at https://groups.google.com/forum/#!topic/maxent/gdN9D5hPZOg
   model.lambdas <- slot(model, "lambdas")
   exclude.lambdas <- grepl("linearPredictorNormalizer|densityNormalizer|numBackgroundPoints|entropy", model.lambdas)
   model.lambdas <- model.lambdas[!exclude.lambdas]
   model.lambdas <- gsub("`|'", "", model.lambdas) # remove ticks
   model.lambdas <- do.call("rbind", strsplit(model.lambdas, ","))   
   model.lambdas <- data.frame(variable = model.lambdas[, 1], apply(model.lambdas[, 2:4], MARGIN = 2, as.numeric))
   npar <- nrow(model.lambdas[model.lambdas[, "X1"] != 0, ])
   
   # Continued from Warren and Seifert 2011:
   #   These metrics
   #   are assessed by standardizing raw scores for each ENM
   #   so that all scores within the geographic space sum to 1
   #   and then calculating the likelihood of the data given
   #   each ENM by taking the product of the suitability scores
   #   for each grid cell containing a presence.
   sc.sum <- cellStats(sc, stat = sum, na.rm = TRUE)
   stand.sc <- sc/sc.sum
   pres.sc <- raster::extract(stand.sc, p)
   ll <- sum(log(pres.sc), na.rm = TRUE)
   
   npts <- nrow(p)
#   From Warren and Seifert 2011 again:
#   We exclude models with zero parameters (which
#   occur in some cases with small sample sizes and
#   extremely high values for b) and models with more
#   parameters than data points (which violate the assump-
#   tions of AICc).
   # I do not remove the calculations, but a warning is issued.
   if (npar >= npts - 1) warning(paste("Many parameters compared to number of presence points."))
   
   AICcscore <- (2 * npar - 2 * ll) + (2 * (npar) * (npar + 1) / (npts - npar - 1))
   AICscore <- 2 * npar - 2 * ll
   BICscore <- npar * log(npts) - 2 * ll
   
   out <- c(AIC = AICscore, AICc = AICcscore, BIC = BICscore, LL = ll, 
      npar = npar, npoints = npts)
   out
}