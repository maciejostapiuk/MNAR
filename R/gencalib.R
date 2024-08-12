library(sampling)
library(devtools)
library(rstudioapi)

gencal <-  function(totals, calib_var, instr_var = NULL, target_var, initial_weights) {
  if (sum(dim(calib_var) == dim(instr_var)) == ncol(calib_var) -1) {
    weights <- gencalib(calib_var,
                        instr_var,
                        initial_weights,
                        total = totals,
                        method = "raking")
    return(weights)}
                        }
