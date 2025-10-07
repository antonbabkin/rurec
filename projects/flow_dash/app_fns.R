cat("app_fns.R loaded\n")

#' Aggregate dataframe values at an ilevel
#' 
#' @param x dataframe to aggregate
#' @param id_cols additional columns to group by
#' @param detail_col column name with detail level code
#' @param value_cols columns to aggregate
#' @param ilevel level of aggregation
#' @param com_code only aggregate this code
agg_ilevel <- function(x,
                       id_cols = character(),
                       detail_col = "detail",
                       value_cols = c("value"),
                       ilevel = c("total", "sector", "summary", "u_summary", "detail"), 
                       com_code = NULL) {
  ilevel <- match.arg(ilevel)
  
  
  if (ilevel == "total") {
    y <- x %>%
      summarize(across(all_of(value_cols), sum),
                .by = (if (length(id_cols) > 0) id_cols else NULL))
  } else if (ilevel == "detail") {
    y <- x %>%
      select(all_of(id_cols), all_of(detail_col), all_of(value_cols)) %>%
      filter(.data[[detail_col]] == {{ com_code }})
  } else {
    if (is.null(com_code)) {
      concordance <- filter(appdata$com_codes, !is.na(detail))
    } else {
      concordance <- filter(appdata$com_codes, .data[[ilevel]] == {{ com_code }}, !is.na(detail))
    }
    y <- concordance %>%
      inner_join(x, join_by(detail == {{ detail_col }})) %>%
      summarize(across(all_of(value_cols), sum),
                .by = (if (length(id_cols) > 0) id_cols else NULL))
  }
  y
}


#' Trade flows aggregated for a selected place and commodity
agg_flows <- function(place,
                      dir = c("in", "out"),
                      ilevel = c("total", "sector", "summary", "u_summary", "detail"),
                      code = NULL) {
  dir <- match.arg(dir)
  ilevel <- match.arg(ilevel)
  switch(
    dir,
    `in` = appdata$flows %>%
      filter(to == place) %>%
      agg_ilevel(id_cols = "from", detail_col = "com_code", value_cols = "flow", ilevel = ilevel, com_code = code),
    `out` = appdata$flows %>%
      filter(from == place) %>%
      agg_ilevel(id_cols = "to", detail_col = "com_code", value_cols = "flow", ilevel = ilevel, com_code = code)
  )
}