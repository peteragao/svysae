# questions
# 1.allow users to supply direct estimates?
# 2. what to do with direct estimates with zero variance?
# 3. lonely.psu??

#' Smooth via area level model
#'
#' Generates small area estimates  by smoothing direct estimates using an area
#' level model
#'
#' @param formula an object of class 'formula' describing the model to be fitted.
#' @param domain formula specifying variable containing domain labels
#' @param design an object of class "svydesign" containing the data for the model
#' @param family of the response variable, currently supports 'binomial' (default with logit link function) or 'gaussian'.
#' @param Amat adjacency matrix for the regions. If set to NULL, the IID spatial effect will be used.
#' @param X.area areal covariates data frame. One of the column names needs to match 'domain', in order to be linked to the data input. Currently only supporting time-invariant domain-level covariates.
#' @param domain.size domain size data frame. One of the column names needs to match 'domain' in order to be linked to the data input and there must be a size column containing domain sizes.
#' @param pc.u 	hyperparameter U for the PC prior on precisions.
#' @param pc.alpha hyperparameter alpha for the PC prior on precisions.
#' @param pc.u.phi hyperparameter U for the PC prior on the mixture probability phi in BYM2 model.
#' @param pc.alpha.phi hyperparameter alpha for the PC prior on the mixture probability phi in BYM2 model.
#' @param CI 	the desired posterior credible interval to calculate
#' @param n.sample number of draws from posterior used to compute summaries
#' @param var.tol tolerance parameter; if variance of an area's direct estimator is below this value, that direct estimator is dropped from model
#' @param ... additional parameters
#'
#' @return A list with elements
#' \item{direct.est}{direct estimates}
#' \item{s.dir.iid.fit}{fitted INLA object for iid domain effects model}
#' \item{s.dir.iid.est}{non-spatial smoothed estimates}
#' \item{s.dir.sp.fit}{fitted INLA object for spatial domain effects model}
#' \item{s.dir.sp.est}{spatially smoothed estimates (if adjacency matrix provided)}
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(DemoData2)
#' data(DemoMap2)
#' des0 <- svydesign(ids = ~clustid+id, strata = ~strata,
#'                   weights = ~weights, data = DemoData2, nest = T)
#' Xmat <- aggregate(age~region, data = DemoData2, FUN = mean)
#'
#' EXAMPLE 1: Continuous response model
#' cts.res <- svysmootharea(tobacco.use ~ 1, domain = ~region,
#'                          Amat = DemoMap2$Amat, design = des0,
#'                          pc.u = 1,
#'                          pc.alpha = 0.01,
#'                          pc.u.phi = 0.5,
#'                          pc.alpha.phi = 2/3)
#'
#' EXAMPLE 2: Including area level covariates
#' cts.cov.res <- svysmootharea(tobacco.use ~ age, domain = ~region,
#'                              Amat = DemoMap2$Amat, design = des0,
#'                              X.area = Xmat,
#'                              pc.u = 1,
#'                              pc.alpha = 0.01,
#'                              pc.u.phi = 0.5,
#'                              pc.alpha.phi = 2/3)
#'
#' EXAMPLE 3: Binary response model
#' bin.res <- svysmootharea(tobacco.use ~ 1, domain = ~region,
#'                          family = "binomial",
#'                          Amat = DemoMap2$Amat, design = des0,
#'                          pc.alpha = 0.01,
#'                          pc.u.phi = 0.5,
#'                          pc.u = 1,
#'                          pc.alpha.phi = 2/3)
#'
#' EXAMPLE 4: Including area level covariates in binary response model
#' bin.cov.res <- svysmootharea(tobacco.use ~ age, domain = ~region,
#'                              family = "binomial",
#'                              Amat = DemoMap2$Amat, design = des0,
#'                              X.area = Xmat,
#'                              pc.u = 1,
#'                              pc.alpha = 0.01,
#'                              pc.u.phi = 0.5,
#'                              pc.alpha.phi = 2/3)
#'}
#'
#'
svysmootharea <- function(formula,
                          domain,
                          design = NULL,
                          family = c("gaussian", "binomial")[1],
                          Amat = NULL,
                          X.area = NULL,
                          domain.size = NULL,
                          pc.u = 1,
                          pc.alpha = 0.01,
                          pc.u.phi = 0.5,
                          pc.alpha.phi = 2/3,
                          CI = .95,
                          n.sample = 250,
                          var.tol = 1e-10,
                          ...) {
  responseType = ifelse(family == "binomial", "binary", "gaussian")
  return(
    SUMMER::smoothArea(
      formula,
      domain,
      design,
      responseType = responseType,
      Amat,
      X.area,
      domain.size,
      pc.u,
      pc.alpha,
      pc.u.phi,
      pc.alpha.phi,
      CI,
      n.sample,
      var.tol,
      ...
    )
  )

}
