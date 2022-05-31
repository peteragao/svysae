#' Smooth via unit level model
#' 
#' Generates small area estimates by smoothing direct estimates using a unit 
#' level model
#'
#' @param formula an object of class "formula" describing the model to be fitted.
#' @param domain formula specifying variable containing domain labels
#' @param design an object of class "svydesign" containing the data for the model
#' @param family of the response variable, currently supports 'binomial' (default with logit link function) or 'gaussian'.
#' @param Amat Adjacency matrix for the regions. If set to NULL, the IID spatial effect will be used.
#' @param X.pop unit-level covariates data frame. One of the column name needs to match the domain specified, in order to be linked to the data input. Currently only supporting time-invariant domain-level covariates.
#' @param domain.size Domain size data frame. One of the column name needs to match the domain specified, in order to be linked to the data input and there must be a size column containing domain sizes.
#' @param pc.u 	hyperparameter U for the PC prior on precisions.
#' @param pc.alpha hyperparameter alpha for the PC prior on precisions.
#' @param pc.u.phi hyperparameter U for the PC prior on the mixture probability phi in BYM2 model.
#' @param pc.alpha.phi hyperparameter alpha for the PC prior on the mixture probability phi in BYM2 model.
#' @param CI 	the desired posterior credible interval to calculate
#' @param n.sample number of draws from posterior used to compute summaries
#' @param ... additional parameters
#'
#' @return A list with elements
#' \item{direct.est}{direct estimates}
#' \item{model.fit}{fitted INLA object for iid domain effects model}
#' \item{model.est}{smoothed estimates}
#' @export
#'
#' @examples
#' \dontrun{
#' data(DemoData2)
#' data(DemoMap2)
#' des0 <- svydesign(ids = ~clustid+id, strata = ~strata,
#'                  weights = ~weights, data = DemoData2, nest = T)
#'                  
#' EXAMPLE 1: Continuous response model
#' cts.res <- svysmoothunit(formula = tobacco.use ~ 1,
#'                          domain = ~region,
#'                          design = des0, X.pop = DemoData2)
#'                       
#' EXAMPLE 2: Binary response model
#' bin.res <- svysmoothunit(formula = tobacco.use ~ 1,
#'                          family = "binomial",
#'                          domain = ~region,
#'                          design = des0, X.pop = DemoData2)
#'}

svysmoothunit <- function(formula,
                          domain, 
                          design,
                          family = c("gaussian", "binomial")[1],
                          Amat = NULL,
                          X.pop = NULL,
                          domain.size = NULL,
                          pc.u = 1,
                          pc.alpha = 0.01, 
                          pc.u.phi = 0.5,
                          pc.alpha.phi = 2/3,
                          CI = .95, n.sample = 250,
                          ...) {
  return(
    SUMMER::smoothUnit(
      formula,
      domain, 
      design,
      family,
      Amat,
      X.pop,
      domain.size,
      pc.u,
      pc.alpha, 
      pc.u.phi,
      pc.alpha.phi,
      CI, 
      n.sample,
      ...
    )
  )
  

}



