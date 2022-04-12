#' Import nlmixr2 output into R
#'
#' @description Convert 'nlmixr2' model output into an 'xpose' database.
#'
#' @param obj nlmixr2 fit object to be evaluated.
#' @param pred Name of the population prediction variable to use for
#'     plotting. If unspecified, it will choose either "NPDE",
#'     "CWRES", and "RES" (in that order) if the column exists in the
#'     data.
#' @param wres Name of the weighted residual variable to use for
#'     plotting. If unspecified, it will choose either "NPDE",
#'     "CWRES", and "RES" (in that order) if the column exists in the
#'     data.
#' @param gg_theme A ggplot2 theme object.
#' @param xp_theme An xpose theme or vector of modifications to the
#'     xpose theme (eg. \code{c(point_color = 'red', line_linetype =
#'     'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the
#'     console.
#' @param skip Character vector be used to skip the import/generation
#'     of: 'data', 'files', 'summary' or any combination of the three.
#' @param ... Additional arguments to be passed to the
#'     \code{\link[readr]{read_delim}} functions.
#'
#' @return An \code{\link[xpose]{xpose_data}} object suitable for use in 'xpose'.
#'
#' @importFrom nlmixr2est nlmixr2
#' @importFrom dplyr group_by mutate tibble case_when
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect
#' @importFrom xpose theme_readable theme_xp_default
#' @importFrom stats coef rnorm
#'
#' @examples
#'
#' library(nlmixr2)
#'
#' one.cmt <- function() {
#'   ini({
#'     ## You may label each parameter with a comment
#'     tka <- 0.45 # Ka
#'     tcl <- log(c(0, 2.7, 100)) # Log Cl
#'    ## This works with interactive models
#'     ## You may also label the preceding line with label("label text")
#'     tv <- 3.45; label("log V")
#'     ## the label("Label name") works with all models
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     linCmt() ~ add(add.sd)
#'   })
#' }
#'
#' theo_sd_fit <- nlmixr(one.cmt, theo_sd, "focei", control=foceiControl(print=0))
#'
#' xpdb <- xpose_data_nlmixr2(obj = theo_sd_fit)
#'
#' @export
xpose_data_nlmixr2 <- function(obj         = NULL,
                              pred        = NULL, #"CPRED",
                              wres        = NULL, #"CWRES",
                              gg_theme    = theme_readable(),
                              xp_theme    = theme_xp_default(),
                              quiet,
                              skip        = NULL,
                              ...) {
  runname <- deparse(substitute(obj))

  . = NULL
  ID = NULL
  RES = NULL
  DV = NULL
  PRED = NULL
  msg = NULL

  get_wres <- function(res, dv, pred) {
    suppressWarnings(res / (sqrt(stats::cov(dv, pred))))
  }

  if (is.null(obj)) {
    stop('Argument `obj` required.', call. = FALSE)
  }


  if (missing(quiet)) quiet <- !interactive()

  if (!inherits(obj,  "nlmixr2FitData")) {
    stop("needs to be a nlmixr2 fit")
  }
  mtype <- obj$est
  software <- "nlmixr2"
  if (is.null(wres)){
    if (any(names(obj) == "CWRES")) {
      wres <- "CWRES"
    } else  if (any(names(obj) == "NPDE")){
      wres <- "NPDE"
    } else if (any(names(obj) == "RES")) {
      wres <- "RES"
      obj <- nlmixr2::addCwres(obj)
      if (any(names(obj) == "CWRES")){
        wres <- "CWRES"
        warning(sprintf("Added CWRES to fit (using %s%s)",
                        crayon::blue("nlmixr2::"), crayon::yellow("addCwres")))
      } else {
        warning(sprintf("Using RES; Consider adding NPDE (%s%s) to fit",
                        crayon::blue("nlmixr2::"), crayon::yellow("addNpde")))
      }
    }
  }
  if (is.null(pred)){
    if (any(names(obj) == "EPRED") & wres == "NPDE"){
      pred <- "EPRED"
    } else if (any(names(obj) == "CPRED") & wres == "CWRES"){
      pred <- "CPRED"
    } else if (any(names(obj) == "PRED") & wres == "RES"){
      pred <- "PRED"
    } else if (any(names(obj) == "EPRED")){
      pred <- "EPRED"
    } else if (any(names(obj) == "CPRED")){
      pred <- "CPRED"
    } else if (any(names(obj) == "PRED")){
      pred <- "PRED"
    }
  }


  if (any("nlmixr2FitData" == class(obj))) {
    data <- as.data.frame(obj)
    data_a <- data %>%
      dplyr::group_by(ID)

    data_a <- tibble::as_tibble(data_a)
  }

  if(!(wres %in% names(data_a))) {
    stop(paste(wres, ' not found in nlmixr2 fit object.', sep=""), call. = FALSE)
  }

  if(!(pred %in% names(data_a))) {
    stop(paste(pred, ' not found in nlmixr2 fit object.', sep=""), call. = FALSE)
  }

  if (!inherits(obj, "nlmixr2FitData")) {
    stop("needs to be a nlmixr2 fit object")
  }
  data_a <- obj$dataMergeInner
  names(data_a) <- toupper(names(data_a))
  data_a <- data_a[, !duplicated(names(data_a))]
  full.dat <- data_a

  # check for ETAs
  # if(!any(stringr::str_detect(names(data_a), 'ETA\\d+|ET\\d+|eta.*'))) {
  #   data_a <- merge(data_a, obj$eta)
  # }
  if(!all(names(diag(obj$omega)) %in% names(data_a))) {
    data_a <- merge(data_a, obj$eta)
  }

  if (!("EVID" %in% names(data_a))) {
    data_a$EVID <- 0
  }
  print(summary(data_a))

  data <- NULL
  data_ind <- data_a %>%
    colnames() %>%
    dplyr::tibble(table = 'nlmixr2',
                  col   = .,
                  type  = NA_character_,
                  label = NA_character_,     # Feature to be added in future release
                  units = NA_character_) %>% # Feature to be added in future release
    dplyr::mutate(type = dplyr::case_when(
      .$col == 'ID' ~ 'id',
      .$col == 'DV' ~ 'dv',
      .$col == 'TIME' ~ 'idv',
      .$col == 'OCC' ~ 'occ',
      .$col == 'DVID' ~ 'dvid',
      .$col == 'AMT' ~ 'amt',
      .$col == 'MDV' ~ 'mdv',
      .$col == 'EVID' ~ 'evid',
      .$col == 'IPRED' ~ 'ipred',
      .$col == pred ~ 'pred',
      .$col %in% c('RES', 'WRES', 'CWRES', 'IWRES', 'EWRES', 'NPDE','IRES','CRES') ~ 'res',
      .$col %in% c('WT','AGE','HT','BMI','LBM') ~ 'contcov',
      .$col %in% c('SEX','RACE') ~ 'catcov',
      .$col %in% c('CL','V','V1','V2','V3','Q','Q2','Q3','KA','K12','K21','K','K13','K31','K23','K32','K24','K42','K34','K43',
                   'cl','v','v1','v2','v3','q','q2','q3','ka','k12','k21','k','k13','k31','k23','k32','k24','k42','k34','k43',
                   'tcl','tv','tv1','tv2','tv3','tq','tq2','tq3','tka','tk12','tk21','tk','tk13','tk31','tk23','tk32','tk24','tk42','tk34','tk43') ~ 'param',
      stringr::str_detect(.$col, 'ETA\\d+|ET\\d+|eta.*') ~ 'eta'))

  data_ind$type[is.na(data_ind$type)] <- 'na'

  data <- list()
  data <- dplyr::tibble(problem = 1,
                        simtab = F,
                        index = list(data_ind),
                        data = list(data_a),
                        modified = F)

  # Generate model summary
  if ('summary' %in% skip) {
    msg('Skipping summary generation', quiet)
    summary <- NULL
  } else if (software == 'nlmixr2') {
    summary <- summarise_nlmixr2_model(obj, '', software, rounding = xp_theme$rounding, runname=runname)
  }

  # The weighted residuals are calculated by dividing the vector of each
  # individual's residuals (res_i) by the square root of the matrix of
  # covariances of that individual's data conditional on the population model:
  #
  #   WRES_i = RES_i / SQRT(COV(data_i | F_pop))
  #
  # This means that for each WRES calculated we include covariances between data
  # points of an individual.  If the correlations between some of these data
  # points are negative then the resulting WRES could also be negative, while
  # the RES could be positive.
  #
  # -Andy

  files <- NULL
  if(mtype=="saem") {
    tracedat <- tibble::as_tibble(as.data.frame(obj$par.hist))
    names(tracedat)[grep("iter", names(tracedat))] <-
      "ITERATION"

    files <- dplyr::tibble(name = deparse(substitute(obj)),
                           extension = 'ext',
                           problem = 1,
                           subprob = 0,
                           method = 'saem',
                           data = list(tracedat),
                           modified = FALSE)
  }

  # Label themes
  attr(gg_theme, 'theme') <- as.character(substitute(gg_theme))
  attr(xp_theme, 'theme') <- as.character(substitute(xp_theme))

  # Output xpose_data
  list(code = obj$uif, summary = summary, data = data,
       files = files, gg_theme = gg_theme, xp_theme = xp_theme,
       options = list(dir = NULL, quiet = quiet,
                      manual_import = NULL), software = 'nlmixr2') %>%
    structure(class = c('xpose_data', 'uneval'))
}

#' @rdname xpose_data_nlmixr2
#' @export
xpose_data_nlmixr <- xpose_data_nlmixr2
