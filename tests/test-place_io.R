# imports ----

library(logger)
library(tidyverse)
library(glue)
library(testthat)

# working directory is different when called with test_file()
setwd(rprojroot::find_rstudio_root_file())

source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))
source("R/place_io.R", local = (place_io <- new.env()))


# config ----

# clear cached outputs
clear_outputs <- TRUE
if (clear_outputs) {
  place_io$clear_outputs()
} else {
  log_warn("running tests without clearing output cache")
}

# control which test groups to run
run_tests <- list(
  agg_vs_bea = TRUE,
  ind_vs_com = TRUE,
  outsupdem = TRUE
)
if (!all(as.logical(run_tests))) {
  log_warn("some tests are disabled: {str_c(names(run_tests)[!as.logical(run_tests)], collapse = ',')}")
}




# output ----

## county totals ----
# county output is sometimes zero in cbp_raw,sum and cbp_raw,det.
# for now, do not run this test
# and do not assume that all counties have positive output
# if (run_tests$county_out) {
#   for (bus_data in c("cbp_imp", "cbp_raw", "infogroup")) {
#     for (ilevel in c("sec", "sum", "det")) {
#       
#       x <- place_io$call_output(year = 2012, class_system = "industry", ilevel = ilevel, bus_data = bus_data) %>%
#         summarize(output = sum(output), .by = "place") %>%
#         pull(output)
#       
#       test_that(glue("positive output in all reported counties
#                      bus_data={bus_data}, ilevel={ilevel}"), {
#         all_true <- is.finite(x)
#         expect_equal(x > 0, all_true) 
#       })
#       
#     }
#   }
# }


## BEA industry totals ----

if (run_tests$agg_vs_bea) {
  for (ilevel in c("sec", "sum", "det")) {
    
    xbea <- bea_io$call_industry_output(year = 2012, ilevel = ilevel, condense = TRUE) %>%
      as_tibble(rownames = "indcode") %>%
      rename(output_nat = "T017")
    
    for (bus_data in c("cbp_imp", "cbp_raw", "infogroup")) {
      
      xagg <- place_io$call_output(year = 2012, class_system = "industry", ilevel = ilevel, bus_data = bus_data) %>%
        summarize(output_sum = sum(output) / 1000, .by = "indcode")
      
      test_that(glue("output frame and BEA nat totals share same set of industry codes
                     bus_data={bus_data}, ilevel={ilevel}"), {
                       expect_equal(
                         sort(xagg$indcode), 
                         sort(xbea$indcode))
                     })
      
      test_that(glue("output sum equals BEA nat totals for every industry
                     bus_data={bus_data}, ilevel={ilevel}"), {
                       # do not compare industries that are missing in the business data
                       x <- inner_join(xagg, xbea, by = "indcode") %>%
                         filter(output_sum > 0)
                       expect_equal(x$output_sum, x$output_nat)
                     })
    }
  }
}


## industry vs commodity ----

if (run_tests$ind_vs_com) {
  for (bus_data in c("cbp_imp", "cbp_raw", "infogroup")) {
    for (ilevel in c("sec", "sum", "det")) {
      
      xind <- place_io$call_output(year = 2012, class_system = "industry", ilevel = ilevel, bus_data = bus_data)
      xcom <- place_io$call_output(year = 2012, class_system = "commodity", ilevel = ilevel, bus_data = bus_data)
      
      test_that(glue("national output equals between industry and commodity
                     bus_data={bus_data}, ilevel={ilevel}"), {
        expect_equal(sum(xind$output), sum(xcom$output))
      })
      
      test_that(glue("industry and commodity span same set of counties
                     bus_data={bus_data}, ilevel={ilevel}"), {
        expect_setequal(
          distinct(xind, place) %>% pull(), 
          distinct(xcom, place) %>% pull())
      })
      
      test_that(glue("county output equals between industry and commodity
                     bus_data={bus_data}, ilevel={ilevel}"), {
        x <- inner_join(
          summarize(xind, output_ind = sum(output), .by = "place"),
          summarize(xcom, output_com = sum(output), .by = "place"),
          by = "place")
        expect_equal(x$output_ind, x$output_com)
      })
      
    }
  }
}

# Output, supply and demand ----


if (run_tests$outsupdem) {
  for (bus_data in c("cbp_imp", "cbp_raw", "infogroup")) {
    for (ilevel in c("sec", "sum", "det")) {
      x <- place_io$call_outsupdem(year = 2012,
                                   ilevel = ilevel,
                                   bus_data = bus_data)
      xind <- x %>%
        summarize(across(c(output, supply, demand), sum), .by = "indcode")
      
      test_that(glue(
        "aggregate supply == demand
        bus_data={bus_data}, ilevel={ilevel}"),
        expect_equal(xind$supply, xind$demand)
      )
      
      xcty <- x %>%
        summarize(across(c(output, supply, demand), sum), .by = "place")
      test_that(glue(
        "county supply <= output
        bus_data={bus_data}, ilevel={ilevel}"),
        expect_equal(pmax(xcty$supply - xcty$output, 0), rep(0, nrow(xcty)))
      )
      test_that(glue(
        "county demand <= output
        bus_data={bus_data}, ilevel={ilevel}"),
        expect_equal(pmax(xcty$demand - xcty$output, 0), rep(0, nrow(xcty)))
      )
      
    }
  }
}

