test_that("xpose_data object is valid", {
  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }

  theo_sd_fit <- nlmixr2::nlmixr2(one.cmt, nlmixr2data::theo_sd, "focei", control=nlmixr2::foceiControl(print=0))

  xpdb_nlmixr2 <- xpose_data_nlmixr2(theo_sd_fit)

  # Basic checks
  expect_true(xpose::is.xpdb(xpdb_nlmixr2))

  expect_no_error(xpose::check_xpdb(xpdb_nlmixr2))

  # Summary checks
  typical_labels <- xpose::xpdb_ex_pk$summary$label
  expect_setequal(
    typical_labels, xpdb_nlmixr2$summary$label
  )
})
