#' Data summary function
#'
#' @description Convert 'nlmixr2' model output into an 'xpose' database
#'
#' @param obj nlmixr2 fit object to be evaluated
#' @param model Model. Can be blank
#' @param software Software that generated the model fit
#' @param rounding Number of figures to round estimates to
#' @param runname Name of the model object being converted
#'
#' @return A summary data object used by \link{xpose_data_nlmixr2}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows filter mutate arrange_at case_when select one_of tibble
#' @importFrom tidyr complete
#' @importFrom rlang syms
#' @importFrom methods is
summarise_nlmixr2_model <- function(obj, model, software, rounding, runname) {
    sum <- dplyr::bind_rows(
                      sum_nlmixr2_software(software),                    # Software name
    sum_nlmixr2_version(software),                     # Software version
    sum_nlmixr2_file(runname, software),               # Model file
    sum_nlmixr2_run(obj, runname, software),           # Model run
    sum_nlmixr2_directory(obj, software),              # Model directory
    sum_nlmixr2_reference(model, software),            # Reference model
    sum_nlmixr2_timestart(obj, model, software),            # Run start time
    sum_nlmixr2_timestop(obj, model, software),             # Run stop time
    sum_nlmixr2_probn(model, software),                # Problem no.
    sum_nlmixr2_label(model, software),                # Model label
    sum_nlmixr2_description(model, software),          # Model description
    sum_nlmixr2_input_data(obj, model, software),           # Model input data used
    sum_nlmixr2_nind(model, software, obj),               # Number of individuals
    sum_nlmixr2_nobs(model, software, obj),               # Number of observations
    sum_nlmixr2_subroutine(model, software),           # Des solver
    sum_nlmixr2_runtime(model, software, obj, rounding),   # Estimation runtime
    sum_nlmixr2_simseed(obj, model, software),   # Simulation seed
    sum_nlmixr2_covtime(model, software, obj, rounding),   # Covariance matrix runtime
    sum_nlmixr2_term(model, software),                 # Run termination message
    sum_nlmixr2_warnings(model, software),             # Run warnings (e.g. boundary)
    sum_nlmixr2_errors(model, software),               # Run errors (e.g termination error)
    sum_nlmixr2_nsig(model, software),                 # Number of significant digits
    sum_nlmixr2_nsim(model, software),                 # Number of simulations
    sum_nlmixr2_condn(model, software, rounding),      # Condition number
    sum_nlmixr2_nesample(model, software),             # Number of esample
    sum_nlmixr2_esampleseed(model, software),          # esample seed number
    sum_nlmixr2_ofv(model, software, obj, rounding),   # Objective function value
    sum_nlmixr2_method(model, software, obj),          # Estimation method or sim
    sum_nlmixr2_shk(model, software, 'eps', obj, rounding), # Epsilon shrinkage
    sum_nlmixr2_shk(model, software, 'eta', obj, rounding)  # Eta shrinkage
  )


  . = NULL
  problem <- label <- NULL

  # Complete missing cases for consistency
  tmp <- sum %>%
    dplyr::filter(.$problem != 0)

  if (nrow(tmp) == 0) return(sum)


  tmp %>%
    tidyr::complete(!!!rlang::syms(c('problem', 'label')),
                    fill = list(subprob = 0, value = 'na')) %>%
    dplyr::bind_rows(dplyr::filter(sum, sum$problem == 0)) %>%
    dplyr::arrange_at(.vars = c('problem', 'label', 'subprob')) %>%
    dplyr::mutate(descr = dplyr::case_when(
      .$label == 'software' ~ 'Software',
      .$label == 'version' ~ 'Software version',
      .$label == 'file' ~ 'Run file',
      .$label == 'run' ~ 'Run number',
      .$label == 'dir' ~ 'Run directory',
      .$label == 'ref' ~ 'Reference model',
      .$label == 'probn' ~ 'Problem number',
      .$label == 'timestart' ~ 'Run start time',
      .$label == 'timestop' ~ 'Run stop time',
      .$label == 'descr' ~ 'Run description',
      .$label == 'label' ~ 'Run label',
      .$label == 'data' ~ 'Input data',
      .$label == 'nobs' ~ 'Number of observations',
      .$label == 'nind' ~ 'Number of individuals',
      .$label == 'nsim' ~ 'Number of simulations',
      .$label == 'simseed' ~ 'Simulation seed',
      .$label == 'subroutine' ~ 'ADVAN',
      .$label == 'runtime' ~ 'Estimation runtime',
      .$label == 'covtime' ~ 'Covariance step runtime',
      .$label == 'term' ~ 'Termination message',
      .$label == 'warnings' ~ 'Run warnings',
      .$label == 'errors' ~ 'Run errors',
      .$label == 'nsig' ~ 'Number of significant digits',
      .$label == 'condn' ~ 'Condition number',
      .$label == 'nesample' ~ 'Number of ESAMPLE',
      .$label == 'esampleseed' ~ 'ESAMPLE seed number',
      .$label == 'ofv' ~ 'Objective function value',
      .$label == 'method' ~ 'Estimation method',
      .$label == 'epsshk' ~ 'Epsilon shrinkage',
      .$label == 'etashk' ~ 'Eta shrinkage')) %>%
    dplyr::select(dplyr::one_of('problem', 'subprob', 'descr', 'label', 'value'))
}

# Default template for function output
sum_tpl <- function(label, value) {
  dplyr::tibble(problem = 0,
                subprob = 0,
                label   = label,
                value   = value)
}

# Software name
sum_nlmixr2_software <- function(software) {
  sum_tpl('software', software)
}

# Software version
sum_nlmixr2_version <- function(software) {
    sum_tpl('version', as.character(utils::packageVersion('nlmixr2est')))
}

# Model object name
sum_nlmixr2_file <- function(runname, software) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'file', value = runname)
  }
}

# Model run name
sum_nlmixr2_run <- function(obj, runname, software) {
  if (software == 'nlmixr2') {
    if(!is.null(obj$model.name)) {
      dplyr::tibble(problem = 1, subprob = 0, label = 'run', value = obj$model.name)
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'run', value = runname)
    }
  }
}

# Model file directory
sum_nlmixr2_directory <- function(obj, software) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'dir', value = getwd())
  }
}

# Reference model
sum_nlmixr2_reference <- function(model, software) {
  if (software == 'nlmixr2') {
    sum_tpl('ref', 'not implemented')
  }
}

# Run start time
sum_nlmixr2_timestart <- function(obj, model, software) {
  if (software == 'nlmixr2') {
    st <- "not implemented"
    if (!is.null(obj$start.time)) {
      st <- as.character(obj$start.time)
    }
    dplyr::tibble(problem = 0, subprob = 0, label = 'timestart', value = st)
  }
}

# Run stop time
sum_nlmixr2_timestop <- function(obj, model, software) {
  if (software == 'nlmixr2') {
    st <- "not implemented"
    if (!is.null(obj$stop.time)) {
      st <- as.character(obj$stop.time)
    }
    dplyr::tibble(problem = 0, subprob = 0, label = 'timestop', value = st)
  }
}

# Problem no.
sum_nlmixr2_probn <- function(model, software) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'probn', value = '1')
  }
}

# Model Label
sum_nlmixr2_label <- function(model, software) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'label', value = 'na')
  }
}

# Model description
sum_nlmixr2_description <- function(model, software) {
  if (software == 'nlmixr2') {
    sum_tpl('descr', 'not implemented')
  }
}

# Input data
sum_nlmixr2_input_data <- function(obj, model, software) {
  if (software == 'nlmixr2') {
    if(!is.null(obj$data.name)) {
        dplyr::tibble(problem = 1, subprob = 0, label = 'data', value = obj$data.name)
    } else {
      sum_tpl('data', 'not available')
    }
  }
}

# Number of observations
sum_nlmixr2_nobs <- function(model, software, obj) {
  if (software == 'nlmixr2') {
      dplyr::tibble(problem = 1, subprob = 0, label = 'nobs', value = as.character(obj$nobs))
  }
}

# Number of individuals
sum_nlmixr2_nind <- function(model, software, obj) {
  if (software == 'nlmixr2') {
    nind <- obj$nsub
    dplyr::tibble(problem = 1, subprob = 0, label = 'nind', value = as.character(nind))
  }
}

# Simulation number
sum_nlmixr2_nsim <- function(model, software) {
  if (software == 'nlmixr2') {
    sum_tpl('nsim', 'not implemented')
  }
}

# Simulation seed
sum_nlmixr2_simseed <- function(obj, model, software) {
  if (software == 'nlmixr2') {
    seed_ch <- "not implemented"
    if (!is.null(obj$seed)) {
      seed_ch <- as.character(obj$seed)
    }
    dplyr::tibble(problem = 1, subprob = 0, label = 'simseed', value = seed_ch)
  }
}

# DES solver
sum_nlmixr2_subroutine <- function(model, software) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'subroutine', value = 'na')
  }
}

# Estimation runtime
sum_nlmixr2_runtime <- function(model, software, obj, rounding) {
  if (software == 'nlmixr2') {
    rt <- 'na'
    if (is(obj, "nlmixr2FitData")) {
        rt <- sum(as.matrix(obj$time[, names(obj$time) != "covariance"]))
    }
    if (rt!='na') {
      dplyr::tibble(problem = 1, subprob = 0, label = 'runtime', value = as.character(round(rt, rounding)))
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'runtime', value = 'not available')
    }
  }
}

# Covariance matrix runtime
sum_nlmixr2_covtime <- function(model, software, obj, rounding) {
  if (software == 'nlmixr2') {
    rt <- obj$time$covariance
    if (!is.null(rt)) {
      dplyr::tibble(problem = 1, subprob = 0, label = 'covtime', value = as.character(round(rt, rounding)))
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'covtime', value = 'not available')
    }
  }
}

# Run termination
sum_nlmixr2_term <- function(model, software) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'term', value = 'na')
  }
}


# Run warnings (e.g. boundary)
sum_nlmixr2_warnings <- function(model, software) {
  if (software == 'nlmixr2') {
    # Can get warnings in $warnings
    dplyr::tibble(problem = 1, subprob = 0, label = 'warnings', value = 'na')
  }
}

# Run errors (e.g termination error)
sum_nlmixr2_errors <- function(model, software) {
  if (software == 'nlmixr2') {
    sum_tpl('errors', 'na')
  }
}

# Number of significant digits
sum_nlmixr2_nsig <- function(model, software) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'nsig', value = 'na')
  }
}

# Condition number
sum_nlmixr2_condn <- function(model, software, rounding) {
  if (software == 'nlmixr2') {
    dplyr::tibble(problem = 1, subprob = 0, label = 'condn', value = 'not implemented')
  }
}

# Number of ESAMPLE (i.e. NPDE)
sum_nlmixr2_nesample <- function(model, software) {
  if (software == 'nlmixr2') {
    sum_tpl('nesample', 'na')
  }
}

# ESAMPLE seed number
sum_nlmixr2_esampleseed <- function(model, software) {
  if (software == 'nlmixr2') {
    sum_tpl('esampleseed', 'na')
  }
}

# Objective function value
sum_nlmixr2_ofv <- function(model, software, obj, rounding) {
  if (software == 'nlmixr2') {
    ofv <- obj$objective
    if(!is.null(ofv)) {
      dplyr::tibble(problem = 1, subprob = 0, label = 'ofv', value = as.character(round(ofv, digits=rounding)))
    } else {
      dplyr::tibble(problem = 1, subprob = 0, label = 'ofv', value = 'not available')
    }
  }
}

# Estimation method or sim
sum_nlmixr2_method <- function(model, software, obj) {
  if (software == 'nlmixr2') {
      dplyr::tibble(problem = 1, subprob = 0, label = 'method', value = obj$est)
  }
}

# Epsilon/Eta shrinkage
sum_nlmixr2_shk <- function(model, software, type, obj, rounding) {
  if (software == 'nlmixr2') {
    shk <- 'na'
    lab <- paste(type, 'shk', sep='')
    if (any("nlmixr2FitData" == class(obj))) {
      if(type=="eps") {
        shk <- paste(round((1 - stats::sd(obj$IWRES))*100, digits = rounding), "[1]", sep=" ")
      }
      if(type=="eta") {
        omega <- diag(obj$omega)
        d <- as.data.frame(obj[!duplicated(obj$ID),])

        ## add ETA if missing
        if(!all(names(omega) %in% names(d))) {
          d <- merge(d, obj$eta)
        }

        ## account for 1-eta systems
        if(length(names(omega)) == 1) {
          d <- data.frame(eta = d[,names(d) %in% names(omega)])
          names(d) <- names(omega)
        } else {
          d <- d[,names(d) %in% names(omega)]
        }

        eshr <- c()
        for (i in 1:length(omega)) {
          shr <- (1 - (stats::sd(d[,i]) / sqrt(omega[i])))*100
          eshr <- c(eshr, round(shr, 3))
        }
        shk <- paste(paste(round(eshr, digits = rounding), ' [', 1:length(eshr), ']', sep=''), collapse=', ')
      }
    }
    dplyr::tibble(problem = 1, subprob = 0, label = lab, value = shk)
  }
}
