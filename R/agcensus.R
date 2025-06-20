# Data preparation of Census of Agriculture

# R libraries ----
library(logger)
library(tidyverse)
loadNamespace("arrow")



# Data objects ----
ipath <- list(
  agcensus = "data/pubdata/agcensus/2012/part.pq"
)

opath <- list(
  imputed = "data/agcensus/2012_imputed.pq"
)

# Imputation ----

#' Agricultural commodity sales in $ and number of operations with sales
#' at national, state and county level, augmented with a special hierarchical 
#' system of id's that is used to name values-to-impute and to assemble constraints.
farm_sales <- function() {
  
  renames <- tribble(
    ~name,            ~short_desc,
    "nsal.00",          "COMMODITY TOTALS - OPERATIONS WITH SALES",
    "nsal.1c",          "CROP TOTALS - OPERATIONS WITH SALES",
    "nsal.1c_1gr",      "GRAIN - OPERATIONS WITH SALES",
    "nsal.1c_1gr_1cor", "CORN - OPERATIONS WITH SALES",
    "nsal.1c_1gr_2whe", "WHEAT - OPERATIONS WITH SALES",
    "nsal.1c_1gr_3soy", "SOYBEANS - OPERATIONS WITH SALES",
    "nsal.1c_1gr_4sor", "SORGHUM - OPERATIONS WITH SALES",
    "nsal.1c_1gr_5bar", "BARLEY - OPERATIONS WITH SALES",
    "nsal.1c_1gr_6ric", "RICE - OPERATIONS WITH SALES",
    "nsal.1c_1gr_7oth", "GRAIN, OTHER - OPERATIONS WITH SALES",
    "nsal.1c_2to",      "TOBACCO - OPERATIONS WITH SALES",
    "nsal.1c_3co",      "COTTON, LINT & SEED - OPERATIONS WITH SALES",
    "nsal.1c_4ve",      "VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN - OPERATIONS WITH SALES",
    "nsal.1c_5fr",      "FRUIT & TREE NUT TOTALS - OPERATIONS WITH SALES",
    "nsal.1c_5fr_1tre", "FRUIT & TREE NUT TOTALS, (EXCL BERRIES) - OPERATIONS WITH SALES",
    "nsal.1c_5fr_2ber", "BERRY TOTALS - OPERATIONS WITH SALES",
    "nsal.1c_6ho",      "HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS) - OPERATIONS WITH SALES",
    "nsal.1c_7tr",      "CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS - OPERATIONS WITH SALES",
    "nsal.1c_7tr_1chr", "CUT CHRISTMAS TREES - OPERATIONS WITH SALES",
    "nsal.1c_7tr_2sho", "SHORT TERM WOODY CROPS - OPERATIONS WITH SALES",
    "nsal.1c_8ot",      "FIELD CROPS, OTHER, INCL HAY - OPERATIONS WITH SALES",
    "nsal.2a",          "ANIMAL TOTALS, INCL PRODUCTS - OPERATIONS WITH SALES",
    "nsal.2a_1po",      "POULTRY TOTALS, INCL EGGS - OPERATIONS WITH SALES",
    "nsal.2a_2ca",      "CATTLE, INCL CALVES - OPERATIONS WITH SALES",
    "nsal.2a_3mi",      "MILK - OPERATIONS WITH SALES",
    "nsal.2a_4hg",      "HOGS - OPERATIONS WITH SALES",
    "nsal.2a_5sh",      "SHEEP & GOATS TOTALS, INCL WOOL & MOHAIR & MILK - OPERATIONS WITH SALES",
    "nsal.2a_6hr",      "EQUINE, (HORSES & PONIES, OWNED) & (MULES & BURROS & DONKEYS, ANY) - OPERATIONS WITH SALES",
    "nsal.2a_7aq",      "AQUACULTURE TOTALS - OPERATIONS WITH SALES & DISTRIBUTION",
    "nsal.2a_8ot",      "SPECIALTY ANIMAL TOTALS, (EXCL EQUINE) - OPERATIONS WITH SALES",
    "sale.00",          "COMMODITY TOTALS - SALES, MEASURED IN $",
    "sale.1c",          "CROP TOTALS - SALES, MEASURED IN $",
    "sale.1c_1gr",      "GRAIN - SALES, MEASURED IN $",
    "sale.1c_1gr_1cor", "CORN - SALES, MEASURED IN $",
    "sale.1c_1gr_2whe", "WHEAT - SALES, MEASURED IN $",
    "sale.1c_1gr_3soy", "SOYBEANS - SALES, MEASURED IN $",
    "sale.1c_1gr_4sor", "SORGHUM - SALES, MEASURED IN $",
    "sale.1c_1gr_5bar", "BARLEY - SALES, MEASURED IN $",
    "sale.1c_1gr_6ric", "RICE - SALES, MEASURED IN $",
    "sale.1c_1gr_7oth", "GRAIN, OTHER - SALES, MEASURED IN $",
    "sale.1c_2to",      "TOBACCO - SALES, MEASURED IN $",
    "sale.1c_3co",      "COTTON, LINT & SEED - SALES, MEASURED IN $",
    "sale.1c_4ve",      "VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN - SALES, MEASURED IN $",
    "sale.1c_5fr",      "FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $",
    "sale.1c_5fr_1tre", "FRUIT & TREE NUT TOTALS, (EXCL BERRIES) - SALES, MEASURED IN $",
    "sale.1c_5fr_2ber", "BERRY TOTALS - SALES, MEASURED IN $",
    "sale.1c_6ho",      "HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS) - SALES, MEASURED IN $",
    "sale.1c_7tr",      "CUT CHRISTMAS TREES & SHORT TERM WOODY CROPS - SALES, MEASURED IN $",
    "sale.1c_7tr_1chr", "CUT CHRISTMAS TREES - SALES, MEASURED IN $",
    "sale.1c_7tr_2sho", "SHORT TERM WOODY CROPS - SALES, MEASURED IN $",
    "sale.1c_8ot",      "FIELD CROPS, OTHER, INCL HAY - SALES, MEASURED IN $",
    "sale.2a",          "ANIMAL TOTALS, INCL PRODUCTS - SALES, MEASURED IN $",
    "sale.2a_1po",      "POULTRY TOTALS, INCL EGGS - SALES, MEASURED IN $",
    "sale.2a_2ca",      "CATTLE, INCL CALVES - SALES, MEASURED IN $",
    "sale.2a_3mi",      "MILK - SALES, MEASURED IN $",
    "sale.2a_4hg",      "HOGS - SALES, MEASURED IN $",
    "sale.2a_5sh",      "SHEEP & GOATS TOTALS, INCL WOOL & MOHAIR & MILK - SALES, MEASURED IN $",
    "sale.2a_6hr",      "EQUINE, (HORSES & PONIES, OWNED) & (MULES & BURROS & DONKEYS, ANY) - SALES, MEASURED IN $",
    "sale.2a_7aq",      "AQUACULTURE TOTALS - SALES & DISTRIBUTION, MEASURED IN $",
    "sale.2a_8ot",      "SPECIALTY ANIMAL TOTALS, (EXCL EQUINE) - SALES, MEASURED IN $"
  )
  
  ipath$agcensus %>%
    arrow::open_dataset() %>%
    rename_with(tolower) %>%
    filter(agg_level_desc %in% c("NATIONAL", "STATE", "COUNTY"), domain_desc == "TOTAL", short_desc %in% renames$short_desc) %>%
    mutate(
      id_sp1 = if_else(state_alpha == "US", "00", state_fips_code),
      id_sp2 = if_else(is.na(county_code), "000", county_code),
      id_sp = paste(id_sp1, id_sp2, sep = "_"),
    ) %>%
    select(id_sp1, id_sp2, id_sp, short_desc, value, value_f) %>%
    collect() %>%
    left_join(renames, "short_desc") %>%
    separate_wider_delim(name, delim = ".", names = c("measure", "commodity")) %>%
    separate_wider_delim(commodity, delim = "_", names = c("id_co1", "id_co2", "id_co3"), too_few = "align_start", cols_remove = FALSE) %>%
    mutate(
      id_co2 = replace_na(id_co2, "000"),
      id_co3 = replace_na(id_co3, "0000"),
      id_co = paste(id_co1, id_co2, id_co3, sep = "_"),
      id = paste(id_sp, id_co, sep = "_")
    ) %>%
    # ID hierarchy level
    mutate(
      lv_sp = case_when(
        id_sp1 == "00" ~ 0,
        id_sp2 == "000" ~ 1,
        .default = 2
      ),
      lv_co = case_when(
        id_co1 == "00" ~ 0,
        id_co2 == "000" ~ 1,
        id_co3 == "0000" ~ 2,
        .default = 3
      )
    ) %>%
    arrange(id, measure) %>%
    relocate(starts_with("id_"), measure, value)
}


#' Table of constraints.
#' Each unique constraint is represented by rows with a single "id_par":
#' sum(id) == first(value_bal), by = id_par
#' "component" column identifies sets of connected constraints.
constraints <- function(df_sales) {
  loadNamespace("tidygraph")
  
  df_con <- list()
  
  f <- function(x) {
    x %>%
      select(starts_with("id"), value) %>%
      # balance = parent value - sum of non-missing child values
      mutate(value_bal = sum(if_else(id == id_par, value, -value), na.rm = TRUE), .by = id_par) %>%
      filter(is.na(value)) %>%
      select(!value)
  }
  
  # nation = sum(state)
  df_con[["sp1"]] <- df_sales %>%
    filter(lv_sp %in% c(0, 1)) %>%
    mutate(id_par = `str_sub<-`(id, 1, 2, value = "00")) %>%
    f()
  
  # state = sum(county)
  df_con[["sp2"]] <- df_sales %>%
    filter(lv_sp %in% c(1, 2)) %>%
    mutate(id_par = `str_sub<-`(id, 4, 6, value = "000")) %>%
    f()
  
  # total = crop + animal
  df_con[["co1"]] <- df_sales %>%
    filter(lv_co %in% c(0, 1)) %>%
    mutate(id_par = `str_sub<-`(id, 8, 9, value = "00")) %>%
    f()
  
  # crop and animal types
  df_con[["co2"]] <- df_sales %>%
    filter(lv_co %in% c(1, 2)) %>%
    mutate(id_par = `str_sub<-`(id, 11, 13, value = "000")) %>%
    f()
  
  # subcrops
  df_con[["co3"]] <- df_sales %>%
    filter(lv_co %in% c(2, 3), paste(id_co1, id_co2, sep = "_") %in% c("1c_1gr", "1c_5fr", "1c_7tr")) %>%
    mutate(id_par = `str_sub<-`(id, 15, 18, value = "0000")) %>%
    f()
  
  df_con <- bind_rows(df_con, .id = "id_con") %>%
    arrange(id_con, id_par, id) %>%
    relocate(id_con, id_par, id, value_bal)

  # identify groups of connected constraints using a graph
  groups <- tidygraph::tbl_graph(edges = select(df_con, id_par, id)) %>%
    mutate(group = tidygraph::group_components()) %>%
    as_tibble() %>% 
    rename(id = name)
  df_con <- df_con %>%
    left_join(groups, "id")
  stopifnot(!any(is.na(df_con$group)))
  
  df_con
}


priors <- function(df_raw) {
  df0 <- df_raw %>%
    pivot_wider(id_cols = c(id_co, lv_sp, id_sp, id_sp1, id), names_from = measure) %>%
    arrange(id_co, id_sp)
  
  # parent level 0 (nation) - parent value always known
  x_st_p1 <- df0 %>%
    filter(lv_sp %in% c(0, 1)) %>%
    group_by(id_co) %>%
    filter(any(is.na(sale))) %>%
    mutate(bal = sum(if_else(lv_sp == 0, sale, -sale), na.rm = TRUE)) %>%
    filter(is.na(sale)) %>%
    mutate(prior = nsal / sum(nsal) * bal) %>%
    ungroup()
  
  # parent level 1 (state)
  ## parent value known
  x_cty_p1 <- df0 %>%
    filter(lv_sp %in% c(1, 2)) %>%
    group_by(id_co, id_sp1) %>%
    filter(any(is.na(sale))) %>%
    filter(any(lv_sp == 1 & !is.na(sale))) %>%
    mutate(bal = sum(if_else(lv_sp == 1, sale, -sale), na.rm = TRUE)) %>%
    filter(is.na(sale)) %>%
    mutate(prior = nsal / sum(nsal) * bal) %>%
    ungroup()
  
  ## parent value unknown, some children known
  x_cty_p2 <- df0 %>%
    filter(lv_sp %in% c(1, 2)) %>%
    group_by(id_co, id_sp1) %>%
    filter(any(is.na(sale))) %>%
    filter(!any(lv_sp == 1 & !is.na(sale))) %>%
    filter(any(lv_sp == 2 & !is.na(sale))) %>%
    mutate(
      sum_sale = sum(sale, na.rm = TRUE),
      sum_nsal = sum(if_else(is.na(sale), 0, nsal)),
      prior = nsal * sum_sale / sum_nsal
    ) %>%
    filter(is.na(sale), lv_sp == 2) %>%
    ungroup()
  
  ## parent value unknown, all children unknown
  x_cty_p3 <- df0 %>%
    filter(lv_sp %in% c(1, 2)) %>%
    group_by(id_co, id_sp1) %>%
    filter(any(is.na(sale))) %>%
    filter(!any(lv_sp == 1 & !is.na(sale))) %>%
    filter(!any(lv_sp == 2 & !is.na(sale))) %>%
    left_join(
      df0 %>%
        filter(lv_sp == 0) %>%
        mutate(avg_sale = sale / nsal) %>%
        select(id_co, avg_sale),
      by = "id_co"
    ) %>%
    mutate(prior = nsal * avg_sale) %>%
    filter(lv_sp == 2) %>%
    ungroup()
  
  df1 <- df0 %>%
    left_join(
      bind_rows(
        x_st_p1 %>% select(id, prior_1 = prior),
        x_cty_p1 %>% select(id, prior_1 = prior)
      ),
      by = "id"
    ) %>%
    left_join(x_cty_p2 %>% select(id, prior_2 = prior), by = "id") %>%
    left_join(x_cty_p3 %>% select(id, prior_3 = prior), by = "id")
  
  x <- df1 %>%
    mutate(across(starts_with("prior"), round)) %>%
    filter(is.na(sale)) %>%
    rowwise() %>%
    mutate(np = sum(!is.na(prior_1), !is.na(prior_2), !is.na(prior_3)))
  stopifnot(all(x$np == 1))
  
  df2 <- df1 %>%
    filter(is.na(sale)) %>%
    mutate(prior = case_when(
      !is.na(prior_1) ~ prior_1,
      !is.na(prior_2) ~ prior_2,
      .default = prior_3
    ))
  
  df2
}


#' Add slack variables and non-negativity constraints to (A, b)
slack_and_zero <- function(a, b) {
  nv <- ncol(a)
  nc <- length(b)
  nmv <- colnames(a)
  nmc <- rownames(a)
  stopifnot(nrow(a) == nc)
  
  asl <- diag(1, nc) %>%
    `rownames<-`(nmc) %>%
    `colnames<-`(paste0("s_", nmc))
  
  aze <- diag(1, nv) %>%
    `rownames<-`(paste0("z_", nmv)) %>%
    `colnames<-`(nmv)
  bze <- rep(0, nv)
  
  a1 <- rbind(
    cbind(a, asl),
    cbind(aze, matrix(0, nv, nc))
  )
  b1 <- c(b, bze)
  
  list(a = a1, b = b1)
}


solve_group <- function(d_con, d_pri) {
  
  t0 <- Sys.time()
  
  # constraints: A-mat and b-vec
  d1 <- d_con %>%
    mutate(
      a = if_else(id == id_par, -1, 1),
      id_conpar = paste(id_con, id_par, sep = ":")
    ) %>%
    pivot_wider(id_cols = c(id_conpar, value_bal), names_from = id, values_from = a, values_fill = 0)  
  bvec <- d1 %>%
    pull(value_bal, name = id_conpar)
  amat <- d1 %>%
    select(!value_bal) %>%
    column_to_rownames("id_conpar") %>%
    as.matrix()
  ab <- slack_and_zero(amat, bvec)
  
  # dimensions and names
  nv <- ncol(amat)
  nmv <- colnames(amat)
  nc <- nrow(amat)
  nmc <- rownames(amat)
  nms <- paste0("s_", nmc)
  
  # objective function: D-mat and d-vec
  pri <- d_pri %>%
    pull(prior, name = id)
  pri <- pri[nmv]
  dvec <- c(1 / pri, rep(0, nc))
  names(dvec) <- c(nmv, nms)
  dmat <- diag(c(1 / pri**2, rep(1, nc))) %>%
    `rownames<-`(names(dvec)) %>%
    `colnames<-`(names(dvec))
  
  # solve the problem
  sol <- quadprog::solve.QP(dmat, dvec, t(ab$a), ab$b, meq = nc)
  
  list(
    vars = nmv,
    cons = nmc,
    slvs = nms,
    mats = list(
      amat = amat,
      bvec = bvec,
      amatfull = ab$a,
      bvecfull = ab$b,
      dmat = dmat,
      dvec = dvec
    ),
    sol = sol,
    solv = sol$solution[1:nv] %>% `names<-`(nmv),
    sols = sol$solution[nv + 1:nc] %>% `names<-`(nms),
    stat = c(
      variables = nv,
      constraints = nc,
      iterations = sol$iterations,
      time = as.numeric(Sys.time() - t0, units = "secs")
    )
  )
}


#' Impute missing sales numbers in the 2012 Census of Agriculture
build_imputed <- function() {
  
  # raw data
  df_raw <- farm_sales()
  
  # adding-up constraints
  df_con <- df_raw %>%
    filter(measure == "sale") %>%
    constraints()
  
  # prior
  df_pri <- priors(df_raw) %>%
    select(id, prior)
  
  gr = 46
  d_con <- df_con %>% filter(group == gr)
  d_pri <- df_pri %>% filter(id %in% d_con$id)
  sol_gr <- solve_group(d_con, d_pri)
  sol_gr$sol
}





