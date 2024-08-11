library(sampling)

gencal <-  function(totals, nonresponse, instrumental_variables = NULL, target_variables, initial_weights) {
  if (sum(dim(nonresponse) == dim(instrumental_variables)) == ncol(nonresponse) -1) {
    weights <- gencalib(nonresponse,
                        instrumental_variables,
                        initial_weights,
                        total = totals,
                        method = "raking")
    return(weights)}
                        }
  