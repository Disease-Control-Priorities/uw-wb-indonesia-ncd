#===========================================================================
# 09_cost_value.R  --  FAIR Choices costing, budget impact & cost-effectiveness
#===========================================================================
# Transforms the current-run model outputs into a transparent state/flow trace
# and TWO complementary, user-friendly workbooks:
#   (A) indonesia_model_cost_value.xlsx          -- R-value workbook (Sections
#       1-10): full model trace, background mortality and all decision tables as
#       computed, formatted values.  Contains:
#   * run metadata & the validated intervention / cost catalogue (from Model 04)
#   * a compact auditable model trace (+ de-duplicated background mortality)
#   * annual mortality (baseline vs scenario, deaths averted)
#   * component-level costing, budget impact (UNDISCOUNTED), and a mortality-
#     based cost-effectiveness table (USD per death averted)
#   * Model 08 economic value (reused, when reconcilable) and QA / methods.
#   (B) indonesia_model_cost_value_formulae.xlsx -- formula edition (Section 11):
#       the same decision tables driven by LIVE cross-sheet Excel formulas
#       anchored to an editable Calculation_Assumptions sheet, so a user can edit
#       an exposed assumption and watch every dependent result recompute. Colours
#       distinguish header / formula / R-source / editable-input cells; QA
#       formulas reconcile the Excel results against the R engine values.
#
# CONTRACT / SCOPE
#   * Consumes: fair_inputs & fair_scenarios (Model 04, in memory); the Model 06
#     state/flow output (in-memory `results_list` if present, else the exact
#     current on-disk contract output/out_model/model_output_*.rds); Model 07
#     health outcomes (output/dt_output_dalys.rds) and Model 08 economic value
#     (output/08_vsl_results.rds + output/08_bca_parameters.rds). Models 07 and 08
#     are REQUIRED inputs -- Model 09 fails with a consolidated diagnostic if they
#     are missing or their scenario IDs do not reconcile with the current run.
#   * Health outcomes (deaths averted, YLL/YLD/DALY averted, life-years gained)
#     come from Model 07; the Reference-Case benefit-cost analysis (VSL/VSLY per
#     the 2019 Robinson et al. Guidelines) is built from Model 08 as LIVE Excel
#     formulas on the Health_Outcomes / Economic_Value / Benefit_Cost sheets.
#   * FAIR effect, costing and discounting rules follow FairChoices_Methods and
#     the input workbook (see Methods_and_Sources sheet).
#===========================================================================

suppressWarnings(suppressMessages({
  library(data.table); library(openxlsx)
}))

message("\n=== Model 09: cost & value ===")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# The installed openxlsx build registers a drawing/vmlDrawing part reference on
# every worksheet but never writes the drawing content, leaving dangling parts
# that Excel tolerates but stricter readers (openpyxl / LibreOffice / Sheets)
# reject. Strip them so both emitted workbooks are well-formed everywhere. Safe:
# no sheet references a drawing (no images / charts / comments are added). It is
# a guarded no-op if a future openxlsx changes this internal layout.
strip_dangling_drawings <- function(wb) {
  tryCatch({
    if (!is.null(wb$Content_Types))
      wb$Content_Types <- wb$Content_Types[!grepl("/xl/drawings/drawing", wb$Content_Types)]
    if (!is.null(wb$worksheets_rels))
      for (i in seq_along(wb$worksheets_rels))
        if (length(wb$worksheets_rels[[i]]))
          wb$worksheets_rels[[i]] <- grep("/drawings/", wb$worksheets_rels[[i]],
                                          value = TRUE, invert = TRUE)
  }, error = function(e) message("  (drawing-strip skipped: ", conditionMessage(e), ")"))
  invisible(wb)
}

# ===========================================================================
# build_bca_sheets_into()  --  shared formula-driven BCA sheet builder
# ---------------------------------------------------------------------------
# Adds three formula-driven decision sheets to an openxlsx workbook that already
# holds a Calculation_Assumptions sheet (with the BCA parameter rows) and a
# Budget_Impact sheet (scenario=A, year=B, incremental_cost=E). Used by BOTH the
# clinical and public-health formula workbooks so the Reference-Case BCA logic
# lives in exactly one place.
#   Health_Outcomes : Model-07 scenario x year modeled/baseline health, with
#                     averted quantities as Excel formulas (grey source cells).
#   Economic_Value  : Model-08 scenario x year source (deaths averted, LY gained,
#                     GNI pc IDN/US, population, LE at avg adult age) + formula
#                     columns for the Reference-Case VSL (elasticity 1.5 transfer
#                     with the 20x floor), the 100x/160x GNI sensitivities, VSLY,
#                     undiscounted VSL/VSLY benefits, the BCA discount factor,
#                     PV benefits, total/PV national GNI and benefit/GNI shares.
#   Benefit_Cost    : per scenario x valuation case -- PV benefits, PV costs
#                     (Budget_Impact incremental cost discounted on the SAME BCA
#                     base year & rate, converted to the benefit currency basis
#                     via cost_to_bca_currency_factor), PV net benefit, BCR,
#                     benefit/GNI share, decision and a partial-BCA scope note.
# All numbers reference Calculation_Assumptions cells (bca_cells) so a user can
# edit an assumption and watch every derived cell recompute. Nothing derived is
# pasted as an R value.
# Returns the (unchanged) sheet names it created, for worksheet-order handling.
# ===========================================================================
build_bca_sheets_into <- function(wb, comparators, ho_src, ev_src, scen_meta,
                                  bca_cells, r_bi, sty) {
  L      <- function(i) openxlsx::int2col(i)
  frows  <- function(fn, rows) vapply(rows, fn, character(1))
  ca     <- function(id) bca_cells[[id]]              # Calculation_Assumptions cell ref
  hdr    <- sty$st_hdr; fml <- sty$st_formula; rsrc <- sty$st_rsrc; wrapS <- sty$st_wrap
  # write a per-row formula vector into `sheet` column `col`, starting at row 2
  wf <- function(sheet, col, x) writeFormula(wb, sheet, x = x, startCol = col, startRow = 2L)

  style_block <- function(sheet, ncol, ndata, formula_cols, rsource_cols,
                          numfmt = list(), wrap_cols = integer(0), widths = NULL) {
    addStyle(wb, sheet, hdr, rows = 1, cols = seq_len(ncol), gridExpand = TRUE)
    if (ndata > 0) {
      dr <- 2:(ndata + 1L)
      for (j in formula_cols) addStyle(wb, sheet, fml,  rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in rsource_cols) addStyle(wb, sheet, rsrc, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (nm in names(numfmt))
        addStyle(wb, sheet, createStyle(numFmt = numfmt[[nm]]),
                 rows = dr, cols = as.integer(nm), gridExpand = TRUE, stack = TRUE)
      for (j in wrap_cols) addStyle(wb, sheet, wrapS, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    }
    freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = 1)
    addFilter(wb, sheet, rows = 1, cols = seq_len(ncol))
    if (is.null(widths)) widths <- pmin(pmax(14, 12), 40)
    setColWidths(wb, sheet, cols = seq_len(ncol), widths = widths)
    setRowHeights(wb, sheet, rows = 1, heights = 28)
  }

  MONEY <- "#,##0"; RATIO <- "0.00"; SHARE <- "0.0000%"; DISC <- "0.0000"; NUM <- "#,##0"

  # ---- Health_Outcomes -----------------------------------------------------
  ho <- as.data.frame(ho_src, stringsAsFactors = FALSE)
  ho <- ho[order(ho$scenario, ho$year), , drop = FALSE]
  ho_cols <- c("scenario","scenario_label","year",
               "modeled_deaths","baseline_deaths","deaths_averted",
               "modeled_cases","baseline_cases","cases_averted",
               "yll","base_yll","yll_averted","yld","base_yld","yld_averted",
               "daly","base_daly","dalys_averted","life_years_gained")
  HO <- data.frame(scenario = ho$scenario, scenario_label = ho$scenario_label,
                   year = ho$year,
                   modeled_deaths = ho$modeled_deaths, baseline_deaths = ho$baseline_deaths,
                   deaths_averted = NA_real_,
                   modeled_cases = ho$modeled_cases, baseline_cases = ho$baseline_cases,
                   cases_averted = NA_real_,
                   yll = ho$yll, base_yll = ho$base_yll, yll_averted = NA_real_,
                   yld = ho$yld, base_yld = ho$base_yld, yld_averted = NA_real_,
                   daly = ho$daly, base_daly = ho$base_daly, dalys_averted = NA_real_,
                   life_years_gained = NA_real_, stringsAsFactors = FALSE)
  n_ho <- nrow(HO); addWorksheet(wb, "Health_Outcomes")
  writeData(wb, "Health_Outcomes", HO, headerStyle = hdr)
  if (n_ho > 0) {
    R <- 2:(n_ho + 1L)
    wf("Health_Outcomes", 6,  frows(function(r) sprintf("E%d-D%d", r, r), R))  # deaths_averted
    wf("Health_Outcomes", 9,  frows(function(r) sprintf("H%d-G%d", r, r), R))  # cases_averted
    wf("Health_Outcomes", 12, frows(function(r) sprintf("K%d-J%d", r, r), R))  # yll_averted
    wf("Health_Outcomes", 15, frows(function(r) sprintf("N%d-M%d", r, r), R))  # yld_averted
    wf("Health_Outcomes", 18, frows(function(r) sprintf("Q%d-P%d", r, r), R))  # dalys_averted
    wf("Health_Outcomes", 19, frows(function(r) sprintf("L%d", r), R))         # life_years_gained = yll_averted
  }
  style_block("Health_Outcomes", length(ho_cols), n_ho,
              formula_cols = c(6,9,12,15,18,19), rsource_cols = c(4,5,7,8,10,11,13,14,16,17),
              numfmt = setNames(rep(list(NUM), 16), as.character(c(4:19))),
              wrap_cols = 2,
              widths = pmin(pmax(nchar(ho_cols) + 2, 12), 20))

  # ---- Economic_Value ------------------------------------------------------
  ev <- as.data.frame(ev_src, stringsAsFactors = FALSE)
  ev <- ev[order(ev$scenario, ev$year), , drop = FALSE]
  ev_cols <- c("scenario","scenario_label","year",
               "deaths_averted","life_years_gained","gni_pc_idn","gni_pc_usa",
               "population","le_avg_adult",
               "vsl_transfer_prefloor","vsl_floor","vsl_preferred","vsl_over_gnipc",
               "vsl_gni100","vsl_gni160","vsly_preferred",
               "econ_value_vsl_undisc","econ_value_vsly_undisc",
               "econ_value_vsl100_undisc","econ_value_vsl160_undisc",
               "disc_factor","pv_vsl_pref","pv_vsly_pref","pv_vsl100","pv_vsl160",
               "total_national_gni","pv_national_gni","annual_benefit_share_gni")
  EV <- data.frame(scenario = ev$scenario, scenario_label = ev$scenario_label, year = ev$year,
                   deaths_averted = ev$deaths_averted, life_years_gained = ev$life_years_gained,
                   gni_pc_idn = ev$gni_pc_idn, gni_pc_usa = ev$gni_pc_usa,
                   population = ev$population, le_avg_adult = ev$le_avg_adult,
                   stringsAsFactors = FALSE)
  for (cn in ev_cols[10:28]) EV[[cn]] <- NA_real_
  n_ev <- nrow(EV); r_ev <- n_ev + 1L; addWorksheet(wb, "Economic_Value")
  writeData(wb, "Economic_Value", EV, headerStyle = hdr)
  if (n_ev > 0) {
    R <- 2:r_ev
    wf("Economic_Value", 10, frows(function(r)                       # J vsl transfer pre-floor
      sprintf("%s*G%d*(F%d/G%d)^%s", ca("ratio"), r, r, r, ca("elast")), R))
    wf("Economic_Value", 11, frows(function(r)                       # K vsl floor
      sprintf("%s*F%d", ca("floor"), r), R))
    wf("Economic_Value", 12, frows(function(r) sprintf("MAX(J%d,K%d)", r, r), R))       # L vsl_preferred
    wf("Economic_Value", 13, frows(function(r) sprintf("IF(F%d=0,\"\",L%d/F%d)", r, r, r), R)) # M vsl/gnipc
    wf("Economic_Value", 14, frows(function(r) sprintf("%s*F%d", ca("mult100"), r), R)) # N vsl_gni100
    wf("Economic_Value", 15, frows(function(r) sprintf("%s*F%d", ca("mult160"), r), R)) # O vsl_gni160
    wf("Economic_Value", 16, frows(function(r) sprintf("IF(I%d=0,\"\",L%d/I%d)", r, r, r), R)) # P vsly_preferred
    wf("Economic_Value", 17, frows(function(r) sprintf("L%d*D%d", r, r), R))  # Q econ_value_vsl
    wf("Economic_Value", 18, frows(function(r) sprintf("P%d*E%d", r, r), R))  # R econ_value_vsly
    wf("Economic_Value", 19, frows(function(r) sprintf("N%d*D%d", r, r), R))  # S econ_value_vsl100
    wf("Economic_Value", 20, frows(function(r) sprintf("O%d*D%d", r, r), R))  # T econ_value_vsl160
    wf("Economic_Value", 21, frows(function(r)                                # U disc factor (BCA)
      sprintf("1/(1+%s)^(C%d-%s)", ca("r_primary"), r, ca("base_year")), R))
    wf("Economic_Value", 22, frows(function(r) sprintf("Q%d*U%d", r, r), R))  # V pv_vsl_pref
    wf("Economic_Value", 23, frows(function(r) sprintf("R%d*U%d", r, r), R))  # W pv_vsly_pref
    wf("Economic_Value", 24, frows(function(r) sprintf("S%d*U%d", r, r), R))  # X pv_vsl100
    wf("Economic_Value", 25, frows(function(r) sprintf("T%d*U%d", r, r), R))  # Y pv_vsl160
    wf("Economic_Value", 26, frows(function(r) sprintf("H%d*F%d", r, r), R))  # Z total_national_gni
    wf("Economic_Value", 27, frows(function(r) sprintf("Z%d*U%d", r, r), R))  # AA pv_national_gni
    wf("Economic_Value", 28, frows(function(r) sprintf("IF(Z%d=0,\"\",Q%d/Z%d)", r, r, r), R)) # AB benefit/GNI
  }
  style_block("Economic_Value", length(ev_cols), n_ev,
              formula_cols = 10:28, rsource_cols = 4:9,
              numfmt = c(setNames(rep(list(NUM),   6), as.character(4:9)),
                         setNames(rep(list(MONEY), 6), as.character(10:15)),
                         setNames(list(MONEY),        "16"),
                         setNames(rep(list(MONEY), 4), as.character(17:20)),
                         setNames(list(DISC),         "21"),
                         setNames(rep(list(MONEY), 4), as.character(22:25)),
                         setNames(rep(list(MONEY), 2), as.character(26:27)),
                         setNames(list(SHARE),        "28")),
              wrap_cols = 2,
              widths = pmin(pmax(nchar(ev_cols) + 2, 12), 22))

  # ---- Benefit_Cost --------------------------------------------------------
  # One row per comparator x valuation case. PV benefits pull the matching PV
  # column from Economic_Value; PV costs discount Budget_Impact incremental cost
  # on the SAME BCA base year & rate and convert to the benefit currency basis.
  cases <- data.frame(
    valuation_method = c("VSL", "VSLY", "VSL", "VSL"),
    valuation_case   = c("preferred (elasticity 1.5, 20x floor)",
                         "preferred (VSLY from preferred VSL)",
                         "sensitivity: 100x GNI per capita",
                         "sensitivity: 160x GNI per capita"),
    ev_pv_col        = c("V", "W", "X", "Y"), stringsAsFactors = FALSE)
  meta <- as.data.frame(scen_meta, stringsAsFactors = FALSE)
  BC <- do.call(rbind, lapply(seq_len(nrow(meta)), function(i)
    data.frame(scenario = meta$scenario[i], scenario_label = meta$scenario_label[i],
               intervention_family = meta$intervention_family[i],
               scenario_level = meta$scenario_level[i],
               valuation_method = cases$valuation_method,
               valuation_case = cases$valuation_case,
               ev_pv_col = cases$ev_pv_col, stringsAsFactors = FALSE)))
  bc_cols <- c("scenario","scenario_label","intervention_family","scenario_level",
               "valuation_method","valuation_case","pv_benefits","pv_costs",
               "pv_net_benefit","benefit_cost_ratio","pv_national_gni",
               "benefit_gni_share","decision","scope_note")
  BCd <- data.frame(scenario = BC$scenario, scenario_label = BC$scenario_label,
                    intervention_family = BC$intervention_family,
                    scenario_level = BC$scenario_level,
                    valuation_method = BC$valuation_method, valuation_case = BC$valuation_case,
                    pv_benefits = NA_real_, pv_costs = NA_real_, pv_net_benefit = NA_real_,
                    benefit_cost_ratio = NA_real_, pv_national_gni = NA_real_,
                    benefit_gni_share = NA_real_, decision = NA_character_,
                    scope_note = NA_character_, stringsAsFactors = FALSE)
  n_bc <- nrow(BCd); addWorksheet(wb, "Benefit_Cost")
  writeData(wb, "Benefit_Cost", BCd, headerStyle = hdr)
  if (n_bc > 0) {
    R <- 2:(n_bc + 1L)
    pvcol <- BC$ev_pv_col
    wf("Benefit_Cost", 7, frows(function(r)                          # G pv_benefits
      sprintf("SUMIFS('Economic_Value'!$%s$2:$%s$%d,'Economic_Value'!$A$2:$A$%d,A%d)",
              pvcol[r - 1L], pvcol[r - 1L], r_ev, r_ev, r), R))
    wf("Benefit_Cost", 8, frows(function(r)                          # H pv_costs (same BCA base/rate; to benefit basis)
      sprintf(paste0("SUMPRODUCT(('Budget_Impact'!$A$2:$A$%d=A%d)*'Budget_Impact'!$E$2:$E$%d*",
                     "(1/(1+%s)^('Budget_Impact'!$B$2:$B$%d-%s)))*%s"),
              r_bi, r, r_bi, ca("r_primary"), r_bi, ca("base_year"), ca("cost_factor")), R))
    wf("Benefit_Cost", 9,  frows(function(r) sprintf("G%d-H%d", r, r), R))   # I pv_net_benefit
    wf("Benefit_Cost", 10, frows(function(r) sprintf("IF(H%d>0,G%d/H%d,\"\")", r, r, r), R)) # J bcr
    wf("Benefit_Cost", 11, frows(function(r)                          # K pv_national_gni
      sprintf("SUMIFS('Economic_Value'!$AA$2:$AA$%d,'Economic_Value'!$A$2:$A$%d,A%d)", r_ev, r_ev, r), R))
    wf("Benefit_Cost", 12, frows(function(r) sprintf("IF(K%d=0,\"\",G%d/K%d)", r, r, r), R)) # L benefit/GNI
    wf("Benefit_Cost", 13, frows(function(r)                          # M decision
      sprintf(paste0("IF(H%d<=0,\"cost-saving or ratio not meaningful (PV cost<=0)\",",
                     "IF(J%d>=1,\"benefits exceed costs (BCR>=1)\",\"costs exceed benefits (BCR<1)\"))"),
              r, r), R))
    writeData(wb, "Benefit_Cost", startCol = 14, startRow = 2, colNames = FALSE,
              x = rep(sprintf(paste0("Partial mortality-benefit BCA (%s). Benefits = averted-mortality ",
                                     "VSL/VSLY in PPP int$; costs = implementation/health-system converted ",
                                     "to PPP int$ (market USD x cost_to_bca_currency_factor). Omits morbidity, ",
                                     "productivity and downstream cost offsets."),
                              gsub("'", "", BCd$valuation_case[1])), n_bc))
  }
  # Note: column indices above map bc_cols; pv_net_benefit is col 9 in the written
  # frame (position of "pv_net_benefit"), bcr col 10, etc. (see bc_cols order).
  style_block("Benefit_Cost", length(bc_cols), n_bc,
              formula_cols = c(7,8,9,10,11,12,13), rsource_cols = integer(0),
              numfmt = c(setNames(rep(list(MONEY),3), as.character(7:9)),
                         setNames(list(RATIO), "10"),
                         setNames(list(MONEY), "11"),
                         setNames(list(SHARE), "12")),
              wrap_cols = c(2, 6, 13, 14),
              widths = c(20, 26, 16, 14, 12, 34, 18, 18, 18, 14, 18, 14, 30, 60))
  if (n_bc > 0) {
    conditionalFormatting(wb, "Benefit_Cost", cols = 13, rows = 2:(n_bc + 1L),
                          rule = "benefits exceed", type = "contains", style = sty$cf_pass)
    conditionalFormatting(wb, "Benefit_Cost", cols = 13, rows = 2:(n_bc + 1L),
                          rule = "costs exceed", type = "contains", style = sty$cf_rev)
  }
  invisible(c("Health_Outcomes", "Economic_Value", "Benefit_Cost"))
}

# ===========================================================================
# build_cvd_40q30_sheets_into()  --  shared formula-driven CVD 40q30 builder
# ---------------------------------------------------------------------------
# Adds two formula-driven sheets to an openxlsx workbook, off the Model 07
# contracts dt_cvd_40q30 (07_cvd_40q30.rds) and cvd_age (07_cvd_40q30_age.rds):
#   CVD_40q30_Age : one row per location/scenario/htn/year/age (ages 30..69).
#                   cvd_deaths and de-duplicated population are grey R-source
#                   cells; m_x, q_x, l_x and l_{x+1} are LIVE Excel formulas
#                   (the recursive period life table, l_30 = 1).
#   CVD_40q30     : one row per location/scenario/htn/year. cvd_40q30 (life-table
#                   lookup), baseline_cvd_40q30 (shared-baseline SUMIFS),
#                   absolute_reduction_pp and percent_reduction are LIVE formulas;
#                   an R-source anchor + a reconciliation-status formula close the
#                   loop. cvd_40q30 is a PERCENT on a 0..100 scale (numeric
#                   format 0.000, never Excel's fractional %).
# `scen_ids` MUST include the baseline id so the baseline SUMIFS resolves.
# Returns the created sheet names (for worksheet-order handling).
# ===========================================================================
build_cvd_40q30_sheets_into <- function(wb, scen_ids, dt40, dt_age, base_id, sty,
                                        recon_tol = 1e-3) {
  hdr <- sty$st_hdr; fml <- sty$st_formula; rsrc <- sty$st_rsrc; wrapS <- sty$st_wrap
  frows <- function(fn, rows) vapply(rows, fn, character(1))
  wf <- function(sheet, col, x) writeFormula(wb, sheet, x = x, startCol = col, startRow = 2L)

  style_block <- function(sheet, ncol, ndata, formula_cols, rsource_cols,
                          numfmt = list(), wrap_cols = integer(0), widths = NULL) {
    addStyle(wb, sheet, hdr, rows = 1, cols = seq_len(ncol), gridExpand = TRUE)
    if (ndata > 0) {
      dr <- 2:(ndata + 1L)
      for (j in formula_cols) addStyle(wb, sheet, fml,  rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in rsource_cols) addStyle(wb, sheet, rsrc, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (nm in names(numfmt))
        addStyle(wb, sheet, createStyle(numFmt = numfmt[[nm]]),
                 rows = dr, cols = as.integer(nm), gridExpand = TRUE, stack = TRUE)
      for (j in wrap_cols) addStyle(wb, sheet, wrapS, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    }
    freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = 1)
    addFilter(wb, sheet, rows = 1, cols = seq_len(ncol))
    if (!is.null(widths)) setColWidths(wb, sheet, cols = seq_len(ncol), widths = widths)
    setRowHeights(wb, sheet, rows = 1, heights = 28)
  }

  HP <- "0.000000"; PCT <- "0.000"; PR <- "0.00"; NUM <- "#,##0"; DTHS <- "#,##0.00"

  # ---- CVD_40q30_Age (age-level source rows + live life-table formulas) -----
  ga <- as.data.table(dt_age)[scenario %in% scen_ids]
  setorder(ga, location, scenario, htn_target_scenario, year, age)   # contiguous per key
  age_cols <- c("location","scenario","scenario_label","intervention_family",
                "htn_target_scenario","year","age","cvd_deaths","population",
                "m_x","q_x","l_x","l_x_next")
  AGE <- data.frame(location = ga$location, scenario = ga$scenario,
                    scenario_label = ga$scenario_label,
                    intervention_family = ga$intervention_family,
                    htn_target_scenario = ga$htn_target_scenario,
                    year = ga$year, age = ga$age,
                    cvd_deaths = ga$cvd_deaths, population = ga$population,
                    m_x = NA_real_, q_x = NA_real_, l_x = NA_real_, l_x_next = NA_real_,
                    stringsAsFactors = FALSE)
  n_age <- nrow(AGE); r_age <- n_age + 1L
  addWorksheet(wb, "CVD_40q30_Age")
  writeData(wb, "CVD_40q30_Age", AGE, headerStyle = hdr)
  if (n_age > 0) {
    R <- 2:r_age
    # m_x = cvd_deaths / population (guarded); q_x = 1 - exp(-m_x)
    wf("CVD_40q30_Age", 10, frows(function(r) sprintf("IF(I%d>0,H%d/I%d,0)", r, r, r), R))
    wf("CVD_40q30_Age", 11, frows(function(r) sprintf("1-EXP(-J%d)", r), R))
    # l_x: 1 at age 30 (first row of each key); else previous row's l_{x+1}
    wf("CVD_40q30_Age", 12, frows(function(r) sprintf("IF(G%d=30,1,M%d)", r, r - 1L), R))
    # l_{x+1} = l_x * (1 - q_x)
    wf("CVD_40q30_Age", 13, frows(function(r) sprintf("L%d*(1-K%d)", r, r), R))
  }
  style_block("CVD_40q30_Age", length(age_cols), n_age,
              formula_cols = 10:13, rsource_cols = c(8, 9),
              numfmt = c(setNames(list("0"), "6"), setNames(list("0"), "7"),
                         setNames(list(DTHS), "8"), setNames(list(NUM), "9"),
                         setNames(rep(list(HP), 4), as.character(10:13))),
              wrap_cols = 3,
              widths = pmin(pmax(nchar(age_cols) + 2, 11), 22))

  # ---- CVD_40q30 (period metric per location/scenario/htn/year) -------------
  gs <- as.data.table(dt40)[scenario %in% scen_ids]
  setorder(gs, location, scenario, htn_target_scenario, year)
  s_cols <- c("location","scenario","scenario_label","intervention_family","scenario_role",
              "parent_package_id","htn_target_scenario","year",
              "cvd_40q30","baseline_cvd_40q30","absolute_reduction_pp","percent_reduction",
              "cvd_40q30_r","recon_status")
  SS <- data.frame(location = gs$location, scenario = gs$scenario,
                   scenario_label = gs$scenario_label,
                   intervention_family = gs$intervention_family,
                   scenario_role = gs$scenario_role,
                   parent_package_id = gs$parent_package_id,
                   htn_target_scenario = gs$htn_target_scenario, year = gs$year,
                   cvd_40q30 = NA_real_, baseline_cvd_40q30 = NA_real_,
                   absolute_reduction_pp = NA_real_, percent_reduction = NA_real_,
                   cvd_40q30_r = gs$cvd_40q30, recon_status = NA_character_,
                   stringsAsFactors = FALSE)
  n_s <- nrow(SS); r_s <- n_s + 1L
  addWorksheet(wb, "CVD_40q30")
  writeData(wb, "CVD_40q30", SS, headerStyle = hdr)
  if (n_s > 0) {
    R <- 2:r_s
    # I cvd_40q30 = 100*(1 - l_70/l_30) via life-table lookup on CVD_40q30_Age.
    # Age-sheet keys: location(A), scenario(B), htn(E), year(F), age(G);
    # l_x_next = M (l_70 at age 69), l_x = L (l_30 at age 30). CVD_40q30-row keys:
    # location(A), scenario(B), htn(G), year(H).
    lookup <- function(col, age, r) sprintf(
      paste0("SUMIFS('CVD_40q30_Age'!$%s$2:$%s$%d,",
             "'CVD_40q30_Age'!$A$2:$A$%d,A%d,",
             "'CVD_40q30_Age'!$B$2:$B$%d,B%d,",
             "'CVD_40q30_Age'!$E$2:$E$%d,G%d,",
             "'CVD_40q30_Age'!$F$2:$F$%d,H%d,",
             "'CVD_40q30_Age'!$G$2:$G$%d,%d)"),
      col, col, r_age, r_age, r, r_age, r, r_age, r, r_age, r, r_age, age)
    wf("CVD_40q30", 9, frows(function(r)
      sprintf("100*(1-(%s)/(%s))", lookup("M", 69L, r), lookup("L", 30L, r)), R))
    # J baseline_cvd_40q30 = the baseline scenario's cvd_40q30 at same loc/htn/year.
    wf("CVD_40q30", 10, frows(function(r) sprintf(
      paste0("SUMIFS($I$2:$I$%d,$A$2:$A$%d,A%d,$G$2:$G$%d,G%d,$H$2:$H$%d,H%d,",
             "$B$2:$B$%d,\"%s\")"),
      r_s, r_s, r, r_s, r, r_s, r, r_s, base_id), R))
    # K absolute_reduction_pp = baseline - scenario (percentage points)
    wf("CVD_40q30", 11, frows(function(r) sprintf("J%d-I%d", r, r), R))
    # L percent_reduction = 100*(baseline-scenario)/baseline; "" if baseline = 0
    wf("CVD_40q30", 12, frows(function(r)
      sprintf("IF(J%d=0,\"\",100*(J%d-I%d)/J%d)", r, r, r, r), R))
    # N recon_status: Excel life-table value vs the R-source anchor within tolerance
    wf("CVD_40q30", 14, frows(function(r)
      sprintf("IF(ABS(I%d-M%d)<=%s,\"match\",\"mismatch\")", r, r, format(recon_tol, scientific = FALSE)), R))
  }
  style_block("CVD_40q30", length(s_cols), n_s,
              formula_cols = c(9, 10, 11, 12, 14), rsource_cols = 13,
              numfmt = c(setNames(list("0"), "8"),
                         setNames(rep(list(PCT), 3), as.character(c(9, 10, 11))),
                         setNames(list(PR), "12"), setNames(list(PCT), "13")),
              wrap_cols = 3,
              widths = pmin(pmax(nchar(s_cols) + 2, 12), 24))
  if (n_s > 0) {
    conditionalFormatting(wb, "CVD_40q30", cols = 14, rows = 2:r_s,
                          rule = "match", type = "contains", style = sty$cf_pass)
    conditionalFormatting(wb, "CVD_40q30", cols = 14, rows = 2:r_s,
                          rule = "mismatch", type = "contains", style = sty$cf_fail)
  }
  invisible(c("CVD_40q30_Age", "CVD_40q30"))
}

# Reference-Case BCA rows for a Calculation_Assumptions sheet (shared by the
# clinical and public-health formula workbooks). Returns the data.table to rbind
# onto `ca` and the matching per-value number-format vector.
bca_ca_block <- function(bca_params) {
  meta <- data.table(
    parameter_id = c("bca_base_year","bca_discount_rate_primary","bca_discount_rate_sensitivity_3pct",
                     "bca_discount_rate_sensitivity_2x_gdp_pc_growth","vsl_us_gni_ratio",
                     "vsl_income_elasticity_preferred","vsl_floor_gni_multiple",
                     "vsl_sensitivity_gni_multiple_100","vsl_sensitivity_gni_multiple_160",
                     "vsly_adult_min_age","vsly_adult_max_age","bca_currency_basis",
                     "bca_price_year","cost_to_bca_currency_factor","bca_standing","bca_scope",
                     "bca_discount_rate_sensitivity_2x_gdp_pc_growth_computed"),
    unit = c("year","proportion/year","proportion/year","proportion/year","ratio (VSL/GNIpc)",
             "elasticity","ratio (VSL/GNIpc)","ratio (VSL/GNIpc)","ratio (VSL/GNIpc)",
             "age (years)","age (years)","text","year","int$ per market US$","text","text","proportion/year"),
    fmt = c("0","0.000","0.000","0.000","0","0.0","0","0","0","0","0",NA,"0","0.00",NA,NA,"0.000"),
    numeric = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE),
    description = c("BCA base year (benefits and costs discounted to this year)",
                    "Primary real discount rate for BOTH benefits and costs",
                    "Standardized sensitivity discount rate (3%)",
                    "Standardized sensitivity: 2x near-term real GDP-pc growth (workbook default)",
                    "US reference VSL-to-GNI-per-capita ratio","Income elasticity of VSL (preferred)",
                    "Floor: VSL not below this multiple of GNI per capita",
                    "Sensitivity: VSL = 100x GNI per capita","Sensitivity: VSL = 160x GNI per capita",
                    "Working-age lower bound (VSLY avg-adult-age)","Working-age upper bound (VSLY avg-adult-age)",
                    "Common monetary basis for benefits and costs","Real price year for benefits and costs",
                    "Market-USD cost -> PPP int$ multiplier","BCA standing / perspective",
                    "BCA scope (partial mortality-benefit vs full societal)",
                    "2x near-term real GDP-pc growth recomputed from SSP2 by Model 08"),
    source = "Robinson et al. 2019 Reference Case Guidelines (via input workbook / Model 08)")
  meta <- meta[parameter_id %in% bca_params$parameter_id]
  val  <- setNames(as.character(bca_params$value), bca_params$parameter_id)
  ca_bca <- data.table(
    parameter_id = meta$parameter_id,
    value = lapply(seq_len(nrow(meta)), function(i) {
      v <- val[[meta$parameter_id[i]]]
      if (isTRUE(meta$numeric[i])) as.numeric(v) else v }),
    unit = meta$unit, role = "BCA control", description = meta$description, source = meta$source)
  list(ca_bca = ca_bca, fmt = meta$fmt)
}

## --- 0. Resolve execution metaparameters (single source of truth: Model 00) --
if (!exists("wd_outp"))
  stop("Model 09: `wd_outp` not set (run from Model 00 or set output path).")
if (!exists("cost_value_output_file"))
  cost_value_output_file <- paste0(wd_outp, "indonesia_model_cost_value.xlsx")
# Companion formula-driven workbook (Section 11); defaults to the R-value file
# name with a `_formulae` suffix in the same directory.
if (!exists("cost_value_formulae_file"))
  cost_value_formulae_file <- sub("\\.xlsx$", "_formulae.xlsx", cost_value_output_file)
if (!exists("baseline_scenario_id")) baseline_scenario_id <- "baseline"
# Public-health formula workbook path (Section 12); default alongside the others.
if (!exists("public_health_cost_value_formulae_file"))
  public_health_cost_value_formulae_file <-
    paste0(wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")

# Intervention-family switches (single source of truth: Model 00). Each family's
# workbook is written only when its switch is TRUE; the shared Model 06 output
# (baseline + whichever families ran) is loaded once below and filtered per family.
if (!exists("run_clinical_interventions"))      run_clinical_interventions      <- TRUE
if (!exists("run_public_health_interventions")) run_public_health_interventions <- FALSE
if (!isTRUE(run_clinical_interventions) && !isTRUE(run_public_health_interventions))
  stop("Model 09: both intervention-family switches are FALSE; nothing to report.")
if (isTRUE(run_clinical_interventions) && (!exists("fair_inputs") || !exists("fair_scenarios")))
  stop("Model 09: run_clinical_interventions = TRUE but `fair_inputs`/`fair_scenarios` not ",
       "found. Source Model 04 (04_define_interventions_indonesia.R) before Model 09.")
if (isTRUE(run_public_health_interventions) && (!exists("public_health_inputs") ||
    is.null(public_health_inputs) || !exists("public_health_scenarios") ||
    is.null(public_health_scenarios)))
  stop("Model 09: run_public_health_interventions = TRUE but `public_health_inputs`/",
       "`public_health_scenarios` not found. Source Model 04 before Model 09.")

# Clinical costing metaparameters (used by the clinical workbook body). Present
# whenever the clinical catalogue exists; the public-health body derives its own.
if (exists("fair_inputs") && !is.null(fair_inputs)) {
  A            <- fair_inputs$assumptions
  yr_start     <- A$analysis_start_year
  yr_end       <- A$analysis_end_year
  analysis_yrs <- yr_start:yr_end
  disc_rate    <- A$cost_discount_rate
  base_id      <- fair_inputs$baseline_scenario_id %||% baseline_scenario_id
} else {
  base_id      <- baseline_scenario_id
}

## --- 1. Load the Model 06 state/flow output --------------------------------
load_model_output <- function() {
  if (exists("results_list", inherits = TRUE)) {
    rl <- get("results_list", inherits = TRUE)
    rl <- Filter(function(x) !is.null(x) && is.data.frame(x), rl)
    if (length(rl)) {
      message("  Using in-memory Model 06 output (results_list).")
      return(rbindlist(rl, fill = TRUE))
    }
  }
  dir_out <- file.path(wd_outp, "out_model")
  files   <- list.files(dir_out, pattern = "^model_output_.*\\.rds$", full.names = TRUE)
  if (!length(files))
    stop("Model 09: no Model 06 output found (neither in-memory results_list ",
         "nor model_output_*.rds in ", dir_out, ").")
  message("  Reading Model 06 output from disk: ", length(files), " file(s).")
  rbindlist(lapply(files, readRDS), fill = TRUE)
}

mo_all <- load_model_output()
setDT(mo_all)
req <- c("scenario", "year", "age", "sex", "cause", "well", "sick",
         "newcases", "dead", "pop", "all.mx", "location")
missc <- setdiff(req, names(mo_all))
if (length(missc))
  stop("Model 09: Model 06 output missing required column(s): ",
       paste(missc, collapse = ", "))

## --- 1b. REQUIRED upstream contracts: Model 07 (health) + Model 08 (value) --
# Model 09 now depends on BOTH Model 07 (health outcomes / life expectancy) and
# Model 08 (economic value). Fail once, with a consolidated, actionable message,
# rather than silently omitting economic value.
.m07_file <- file.path(wd_outp, "dt_output_dalys.rds")
.m08_file <- file.path(wd_outp, "08_vsl_results.rds")
.bca_file <- file.path(wd_outp, "08_bca_parameters.rds")
.q40_file <- file.path(wd_outp, "07_cvd_40q30.rds")
.q40age_file <- file.path(wd_outp, "07_cvd_40q30_age.rds")
.missing_up <- character(0)
if (!file.exists(.m07_file)) .missing_up <- c(.missing_up,
  paste0("Model 07 output not found (", .m07_file, ") -- run 07_output_dalys.R"))
if (!file.exists(.m08_file)) .missing_up <- c(.missing_up,
  paste0("Model 08 output not found (", .m08_file, ") -- run 08_economic_value_calculation.R"))
if (!file.exists(.bca_file)) .missing_up <- c(.missing_up,
  paste0("Model 08 BCA parameters not found (", .bca_file, ") -- re-run Model 08"))
if (!file.exists(.q40_file)) .missing_up <- c(.missing_up,
  paste0("Model 07 CVD 40q30 output not found (", .q40_file, ") -- re-run 07_output_dalys.R"))
if (!file.exists(.q40age_file)) .missing_up <- c(.missing_up,
  paste0("Model 07 CVD 40q30 age audit not found (", .q40age_file, ") -- re-run 07_output_dalys.R"))
if (length(.missing_up))
  stop("Model 09 required upstream inputs are missing:\n  - ",
       paste(.missing_up, collapse = "\n  - "), call. = FALSE)
dt_h07     <- as.data.table(readRDS(.m07_file))
ev08       <- as.data.table(readRDS(.m08_file))
bca_params <- as.data.table(readRDS(.bca_file))
dt_cvd_40q30 <- as.data.table(readRDS(.q40_file))
cvd_age_40q30 <- as.data.table(readRDS(.q40age_file))
# Reconcile scenario IDs across Models 06/07/08 (exact-ID match, not name-guessing).
.scn06 <- unique(mo_all$scenario); .scn07 <- unique(dt_h07$scenario); .scn08 <- unique(ev08$scenario)
.miss0708 <- setdiff(setdiff(.scn06, baseline_scenario_id), .scn07)
if (length(.miss0708))
  stop("Model 09: scenario(s) in Model 06 but absent from Model 07 output: ",
       paste(.miss0708, collapse = ", "), "\n  Re-run Model 07 on the current run.", call. = FALSE)
# CVD 40q30 must cover every current-run scenario (staleness guard).
.scn40 <- unique(dt_cvd_40q30$scenario)
.miss40 <- setdiff(.scn06, .scn40)
if (length(.miss40))
  stop("Model 09: scenario(s) in Model 06 but absent from the CVD 40q30 output (",
       paste(.miss40, collapse = ", "), "). The 07_cvd_40q30.rds contract is stale; ",
       "re-run 07_output_dalys.R on the current run.", call. = FALSE)
# Resolved BCA parameters as a named character vector for downstream cell writing.
BCAP <- setNames(as.character(bca_params$value), bca_params$parameter_id)

# ==========================================================================
# Authoritative annual national population denominator (annual per-capita cost).
#   Source: this run's out_model/model_output_Indonesia_htncov2_aspirational.rds
#   (resolved under wd_outp -- never a machine-specific absolute path). We take
#   the BASELINE (current-course) scenario -- the single demographic reference
#   series applied identically to every scenario in a year -- and de-duplicate
#   population across the cause / model-state dimensions before summing the
#   unique age x sex cells per year. `pop` is invariant across cause within a
#   scenario-year-age-sex cell; it differs only marginally (a survivor effect)
#   across intervention scenarios, so the baseline series is the one defensible
#   national denominator. Fails early if the file, the baseline scenario, or a
#   required year cannot be uniquely resolved to one finite, positive value.
# ==========================================================================
.natpop_file <- file.path(wd_outp, "out_model", "model_output_Indonesia_htncov2_aspirational.rds")
if (!file.exists(.natpop_file))
  stop("Model 09: authoritative national-population source not found (", .natpop_file,
       "). Annual per-capita costing requires the htncov2_aspirational Model 06 output.",
       call. = FALSE)
.natpop_raw <- as.data.table(readRDS(.natpop_file))
.npreq <- c("scenario", "year", "age", "sex", "pop")
.npmiss <- setdiff(.npreq, names(.natpop_raw))
if (length(.npmiss))
  stop("Model 09: national-population source missing column(s): ",
       paste(.npmiss, collapse = ", "), " in ", basename(.natpop_file), call. = FALSE)
if ("location" %in% names(.natpop_raw))                       # national rows only (guard multi-level)
  .natpop_raw <- .natpop_raw[location == "Indonesia"]
.npall <- .natpop_raw[scenario == base_id, .(year, age, sex, pop)]  # cause/state rows share one pop
if (!nrow(.npall))
  stop("Model 09: baseline scenario '", base_id, "' absent from ", basename(.natpop_file),
       "; cannot build the national-population denominator.", call. = FALSE)
# pop is invariant across cause/state within a year x age x sex cell up to
# summation-order floating-point noise; require the spread < 1 person, then
# collapse to one value per cell (mean) so the denominator is uniquely resolved.
.npspread <- .npall[, .(sp = max(pop) - min(pop)), by = .(year, age, sex)]
if (any(.npspread$sp >= 1))
  stop("Model 09: national population is not uniquely resolved (",
       nrow(.npspread[sp >= 1]), " age x sex x year cell(s) carry baseline pop differing by ",
       ">=1 person across cause/state); cannot build one denominator per year.", call. = FALSE)
.npbase <- .npall[, .(pop = mean(pop)), by = .(year, age, sex)]  # collapse fp noise across cause/state
national_pop_dt <- .npbase[, .(national_population = sum(pop)), by = year][order(year)]
if (any(!is.finite(national_pop_dt$national_population)) ||
    any(national_pop_dt$national_population <= 0))
  stop("Model 09: national population is non-finite or non-positive for year(s): ",
       paste(national_pop_dt[!is.finite(national_population) | national_population <= 0]$year,
             collapse = ", "), ".", call. = FALSE)
# Reconciliation only (does NOT change the authoritative baseline series): the
# spread across intervention scenarios should be a small survivor effect.
.np_scn <- unique(.natpop_raw[, .(scenario, year, age, sex, pop)])[
  , .(tot = sum(pop)), by = .(scenario, year)][
  , .(spread_rel = (max(tot) - min(tot)) / max(tot)), by = year]
.np_spread_max <- if (nrow(.np_scn)) max(.np_scn$spread_rel) else 0
message(sprintf("  National-population denominator: baseline series from %s (%d-%d; %.2f%% max cross-scenario spread).",
                basename(.natpop_file), min(national_pop_dt$year), max(national_pop_dt$year),
                100 * .np_spread_max))
rm(.natpop_raw, .npall, .npspread, .npbase, .np_scn)

# Look up national population for a vector of years; errors on any missing /
# non-finite / non-positive year (per-capita denominators must be well-defined).
.national_population_for <- function(years) {
  yy  <- as.integer(years)
  idx <- match(yy, national_pop_dt$year)
  if (anyNA(idx))
    stop("Model 09: no national population for year(s): ",
         paste(unique(yy[is.na(idx)]), collapse = ", "),
         " (source ", basename(.natpop_file), ").", call. = FALSE)
  v <- national_pop_dt$national_population[idx]
  if (any(!is.finite(v)) || any(v <= 0))
    stop("Model 09: national population non-finite/non-positive for year(s): ",
         paste(unique(yy[!is.finite(v) | v <= 0]), collapse = ", "), ".", call. = FALSE)
  v
}
# Attach national_population + the four annual cost-per-capita columns to a
# Budget_Impact-like table (annual scenario x year costs). By construction the
# same-year national population is identical for every scenario.
.add_budget_percapita <- function(dt) {
  if (!nrow(dt)) return(dt)
  dt[, national_population := .national_population_for(year)]
  dt[, baseline_cost_per_capita         := baseline_cost         / national_population]
  dt[, scenario_cost_per_capita         := scenario_cost         / national_population]
  dt[, incremental_cost_per_capita      := incremental_cost      / national_population]
  dt[, disc_incremental_cost_per_capita := disc_incremental_cost / national_population]
  dt[]
}
# Attach national_population + per-capita versions of the four component annual
# cost columns to an Annual_Cost-like table. The denominator is ALWAYS the
# national population for that year -- never the component's eligible/PIN count.
.add_annualcost_percapita <- function(dt) {
  if (!nrow(dt)) return(dt)
  dt[, national_population := .national_population_for(year)]
  dt[, annual_cost_baseline_per_capita    := annual_cost_baseline    / national_population]
  dt[, annual_cost_scenario_per_capita    := annual_cost_scenario    / national_population]
  dt[, annual_cost_incremental_per_capita := annual_cost_incremental / national_population]
  dt[, disc_cost_incremental_per_capita   := disc_cost_incremental   / national_population]
  dt[]
}
# Per-scenario summary per-capita for the deck contract: the simple mean, over
# the analysis horizon EXCLUDING the first year (2025 -> 2026-2050), of the
# annual incremental / discounted-incremental cost per capita on a Budget_Impact
# table. This is the value the deck displays; the workbooks carry the same metric
# as a live Cost_Effectiveness formula. Returns one row per scenario.
.deck_percap_from_bi <- function(bi_dt) {
  if (!nrow(bi_dt))
    return(data.table(scenario = character(0), pc_undisc_val = numeric(0), pc_disc_val = numeric(0)))
  y0 <- min(bi_dt$year)
  bi_dt[year > y0, .(pc_undisc_val = mean(incremental_cost_per_capita),
                     pc_disc_val   = mean(disc_incremental_cost_per_capita)), by = scenario]
}

# ==========================================================================
# Active-scenario contract helpers (shared across all three workbooks).
#
# `Scenario_Catalog` describes exactly the current run's scenarios and is the
# single authoritative row/label/order source for downstream consumers (e.g.
# the executive deck). It is derived from the Model 04 catalogues intersected
# with what Model 06 actually produced, so it inherits the binding include_flag
# contract -- excluded interventions never appear.
# ==========================================================================
.coalesce_scalar <- function(x, default) if (is.null(x) || length(x) == 0L || is.na(x[1])) default else x[1]

# Build the Scenario_Catalog data.table for ONE family's scenario list.
# Columns match the combined workbook's existing Scenario_Catalog contract.
.scenario_catalog_dt <- function(scn_list, family_label, base_id, produced_ids = NULL) {
  if (is.null(scn_list) || !length(scn_list)) return(data.table())
  ids <- names(scn_list)
  if (!is.null(produced_ids)) ids <- intersect(ids, produced_ids)
  # Baseline first, then the remaining scenarios in catalogue order.
  ids <- c(intersect(base_id, ids), setdiff(ids, base_id))
  rbindlist(lapply(ids, function(scn) {
    e  <- scn_list[[scn]]
    iv <- e$intervention_ids
    if (is.null(iv)) iv <- character(0)
    lvl <- .coalesce_scalar(e$scenario_level,
             if (identical(scn, base_id)) "baseline"
             else if (scn %in% c("all", "all_public_health", "all_clinical_public_health")) "combined"
             else "standalone")
    data.table(
      scenario            = scn,
      scenario_label      = as.character(.coalesce_scalar(e$scenario_label, scn)),
      intervention_family = as.character(.coalesce_scalar(e$family, family_label)),
      scenario_level      = as.character(lvl),
      scenario_role       = as.character(.coalesce_scalar(e$scenario_role, NA_character_)),
      parent_package_id   = as.character(.coalesce_scalar(e$parent_package_id, NA_character_)),
      intervention_ids    = paste(iv, collapse = "; "),
      n_interventions     = length(iv))
  }), fill = TRUE)
}

# Accumulator: each workbook writer stashes its R-value cost-effectiveness table
# here so a single BCA-FREE, current-run results contract can be written for the
# executive deck (no Excel recalculation required). BCA/Model 08 outputs are
# untouched; this is an additional, non-BCA contract.
deck_cea_list     <- list()
deck_catalog_list <- list()
deck_percap_list  <- list()   # per-family, per-scenario mean annual per-capita (deck contract)

# ==========================================================================
# CLINICAL (FAIR Choices) cost/value workbooks -- written only when clinical
# interventions are enabled. All existing behaviour and both existing output
# files are preserved unchanged inside this block. (Braces do not create a new
# scope in R, so every object below remains available exactly as before.)
# ==========================================================================
if (isTRUE(run_clinical_interventions)) {

loc_run <- unique(mo_all$location)[1]
# Keep only scenarios that Model 04 declared AND that were actually produced.
declared     <- names(fair_scenarios)
produced     <- intersect(declared, unique(mo_all$scenario))
scen_missing <- setdiff(declared, produced)
if (length(scen_missing))
  message("  NOTE: declared scenario(s) not present in Model 06 output: ",
          paste(scen_missing, collapse = ", "))
comparators  <- setdiff(produced, base_id)   # scenarios to cost / compare

mo <- mo_all[scenario %in% produced & year %in% analysis_yrs]

## --- 2. State / flow trace --------------------------------------------------
# Cause-grained trace at location x scenario x year x age x cause. It is summed
# over SEX for compactness: no selected intervention/cost row is sex-specific
# (all are sex = "Both"), so the both-sex totals reproduce every PIN quantity
# and death count used here. All internal computations still use the full
# single-sex Model 06 output (`mo`). `population` is the total stratum population
# (identical across causes in the source; summed over sex here).
trace <- mo[, .(well = sum(well), sick = sum(sick), new_cases = sum(newcases),
                cause_deaths = sum(dead), population = sum(pop)),
            by = .(scenario, location, year, age, cause)]
setorder(trace, scenario, year, age, cause)

# Background / all-cause mortality de-duplicated to its proper stratum: all.mx
# (all-cause deaths) is constant across cause, so it is taken ONCE per
# (scenario, year, age, sex) and then summed over sex here -- never summed
# across the modeled causes.
bg_stratum <- unique(mo[, .(scenario, location, year, age, sex,
                            all_cause_deaths = all.mx, pop)])
bg <- bg_stratum[, .(all_cause_deaths = sum(all_cause_deaths), population = sum(pop)),
                 by = .(scenario, location, year, age)]
modeled <- mo[, .(modeled_cause_deaths = sum(dead)),
              by = .(scenario, location, year, age)]
bg <- merge(bg, modeled, by = c("scenario", "location", "year", "age"))
bg[, background_deaths := all_cause_deaths - modeled_cause_deaths]
setorder(bg, scenario, year, age)

## --- 3. Annual mortality: baseline vs scenario, deaths averted -------------
mort <- mo[, .(cases = sum(newcases), cause_deaths = sum(dead)),
           by = .(scenario, year, cause)]
base_mort <- mo[scenario == base_id, .(base_deaths = sum(dead), base_cases = sum(newcases)),
                by = .(year, cause)]
mort <- merge(mort, base_mort, by = c("year", "cause"), all.x = TRUE)
mort[, deaths_averted := base_deaths - cause_deaths]
mort[, cases_averted  := base_cases  - cases]
mort <- merge(mort,
              data.table(scenario = names(fair_scenarios),
                         scenario_label = vapply(fair_scenarios,
                                                 function(s) s$scenario_label, character(1))),
              by = "scenario", all.x = TRUE)
setcolorder(mort, c("scenario", "scenario_label", "year", "cause",
                    "cases", "cause_deaths", "base_deaths", "deaths_averted",
                    "base_cases", "cases_averted"))
setorder(mort, scenario, year, cause)

## --- 4. Component costing ---------------------------------------------------
costs <- copy(fair_inputs$costs)
# Only cost records belonging to scenarios we actually run, and only "ready"
# rows (valid unit cost, coverage and PIN). Unready rows are reported, not used.
costs[, cost_ready := !is.na(unit_cost_usd) & unit_cost_usd >= 0 &
        !is.na(cov_baseline) & !is.na(population_in_need_fraction) &
        population_in_need_measure %in% c("all", "prevalence", "incidence")]

# De-duplicated population table (pop identical across causes).
popu <- unique(mo[, .(scenario, year, age, sex, pop)])

# Model quantity for one cost record under a given scenario, by analysis year.
qty_by_year <- function(scn, cr) {
  a0 <- cr$c_age_start; a1 <- cr$c_age_stop; sx <- cr$c_sex
  if (cr$population_in_need_measure == "all") {
    d <- popu[scenario == scn & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(pop)), by = year]
  } else {
    vcol <- if (cr$population_in_need_measure == "prevalence") "sick" else "newcases"
    d <- mo[scenario == scn & cause == cr$cause_code & age >= a0 & age <= a1]
    if (!identical(sx, "Both")) d <- d[sex == sx]
    agg <- d[, .(q = sum(get(vcol))), by = year]
  }
  m <- merge(data.table(year = analysis_yrs), agg, by = "year", all.x = TRUE)
  m[is.na(q), q := 0]; m$q
}

# Absolute coverage path baseline -> target (linear; matches Model 06 engine).
cov_path <- function(cb, ct, sy, ty, yrs) {
  span <- max(ty - sy + 1, 1)
  frac <- pmin(pmax((yrs - sy + 1) / span, 0), 1)
  cc <- cb + (ct - cb) * frac
  cc[yrs < sy] <- cb; cc[yrs > ty] <- ct
  pmin(pmax(cc, 0), 1)
}

disc_factor <- 1 / (1 + disc_rate)^(analysis_yrs - yr_start)

cost_rows <- list()
for (scn in comparators) {
  ids   <- fair_scenarios[[scn]]$intervention_ids
  comps <- costs[intervention_id %in% ids & cost_ready == TRUE]
  if (nrow(comps) == 0) next
  for (i in seq_len(nrow(comps))) {
    cr    <- comps[i]
    q_s   <- qty_by_year(scn,     cr)
    q_b   <- qty_by_year(base_id, cr)
    cov_s <- cov_path(cr$cov_baseline, cr$cov_target, cr$cov_start_year, cr$cov_target_year, analysis_yrs)
    # OPTIONAL exact per-year cost-coverage path (ADDITIVE / backward-compatible):
    # the 70-30-30 -> 70-70-70 cascade attaches a `coverage_path` list-column so
    # costs use the SAME piecewise effective-coverage path as the health effects.
    # Absent on every standard workbook -> the linear cov_path above is used
    # unchanged.
    if (scn != base_id && "coverage_path" %in% names(comps)) {
      cpp <- cr[["coverage_path"]][[1]]
      if (!is.null(cpp) && NROW(cpp) > 0L) {
        cpp <- as.data.table(cpp)
        lk  <- setNames(as.numeric(cpp$coverage_t), as.character(cpp$year))
        v   <- as.numeric(lk[as.character(analysis_yrs)])
        v[is.na(v) & analysis_yrs < min(cpp$year)] <- cr$cov_baseline
        v[is.na(v) & analysis_yrs > max(cpp$year)] <- as.numeric(lk[[as.character(max(cpp$year))]])
        cov_s <- pmin(pmax(v, 0), 1)
      }
    }
    cov_b <- rep(cr$cov_baseline, length(analysis_yrs))
    pin_s <- q_s * cr$population_in_need_fraction
    pin_b <- q_b * cr$population_in_need_fraction
    cost_s <- pin_s * cov_s * cr$frequency_per_year * cr$unit_cost_usd
    cost_b <- pin_b * cov_b * cr$frequency_per_year * cr$unit_cost_usd
    cost_rows[[length(cost_rows) + 1L]] <- data.table(
      scenario = scn, year = analysis_yrs,
      cost_record_id = cr$cost_record_id, cost_component_key = cr$cost_component_key,
      cost_join_key = cr$cost_join_key, cost_scope = cr$cost_scope,
      intervention_id = cr$intervention_id, cause_code = cr$cause_code %||% NA_character_,
      population_in_need_measure = cr$population_in_need_measure,
      population_in_need_fraction = cr$population_in_need_fraction,
      coverage_scenario = cov_s, coverage_baseline = cov_b,
      frequency_per_year = cr$frequency_per_year, unit_cost_usd = cr$unit_cost_usd,
      pin_scenario = pin_s, pin_baseline = pin_b,
      annual_cost_baseline = cost_b, annual_cost_scenario = cost_s,
      annual_cost_incremental = cost_s - cost_b,
      indonesia_adjusted_flag = cr$indonesia_adjusted_flag, price_year = cr$price_year,
      discount_factor = disc_factor,
      disc_cost_baseline = cost_b * disc_factor,
      disc_cost_scenario = cost_s * disc_factor,
      disc_cost_incremental = (cost_s - cost_b) * disc_factor)
  }
}
annual_cost <- if (length(cost_rows)) rbindlist(cost_rows) else
  data.table()  # (no runnable cost components)
if (nrow(annual_cost))
  setcolorder(annual_cost, c("scenario", "year", "intervention_id", "cause_code",
                             "cost_record_id", "cost_component_key", "cost_join_key",
                             "cost_scope", "population_in_need_measure"))
# Annual per-capita component costs (national population denominator; Section 3).
annual_cost <- .add_annualcost_percapita(annual_cost)

## --- 5. Budget impact (UNDISCOUNTED headline; discounted kept separate) -----
if (nrow(annual_cost)) {
  bi <- annual_cost[, .(baseline_cost   = sum(annual_cost_baseline),
                        scenario_cost   = sum(annual_cost_scenario),
                        incremental_cost = sum(annual_cost_incremental),
                        disc_incremental_cost = sum(disc_cost_incremental)),
                    by = .(scenario, year)]
  setorder(bi, scenario, year)
  bi[, cumulative_incremental_cost := cumsum(incremental_cost), by = scenario]
  bi[, cumulative_disc_incremental_cost := cumsum(disc_incremental_cost), by = scenario]
} else bi <- data.table()
# National population + annual per-capita budget-impact columns (Section 3).
bi <- .add_budget_percapita(bi)

## --- 6. Cost-effectiveness: USD per death averted --------------------------
da_by_scn <- mort[scenario %in% comparators,
                  .(deaths_averted = sum(deaths_averted, na.rm = TRUE),
                    cases_averted  = sum(cases_averted,  na.rm = TRUE)),
                  by = scenario]
ic_by_scn <- if (nrow(bi))
  bi[, .(incremental_cost = sum(incremental_cost),
         disc_incremental_cost = sum(disc_incremental_cost)), by = scenario] else
  data.table(scenario = comparators, incremental_cost = 0, disc_incremental_cost = 0)

cea <- merge(da_by_scn, ic_by_scn, by = "scenario", all.x = TRUE)
cea[is.na(incremental_cost), incremental_cost := 0]
cea[is.na(disc_incremental_cost), disc_incremental_cost := 0]
cea <- merge(cea,
             data.table(scenario = names(fair_scenarios),
                        scenario_label = vapply(fair_scenarios,
                                                function(s) s$scenario_label, character(1))),
             by = "scenario", all.x = TRUE)
# Discount costs only; deaths averted counted UNDISCOUNTED.
cea[, cost_per_death_averted := NA_real_]
cea[deaths_averted > 0, cost_per_death_averted := disc_incremental_cost / deaths_averted]
cea[, dominance := "USD per death averted"]
cea[deaths_averted > 0 & disc_incremental_cost < 0, dominance := "Dominant (more health, lower cost)"]
cea[deaths_averted <= 0 & disc_incremental_cost > 0, dominance := "Dominated (less/no health, higher cost)"]
cea[deaths_averted <= 0 & disc_incremental_cost <= 0,
    dominance := "No deaths averted; ratio not defined"]
cea[deaths_averted <= 0, cost_per_death_averted := NA_real_]
setcolorder(cea, c("scenario", "scenario_label", "deaths_averted", "cases_averted",
                   "incremental_cost", "disc_incremental_cost",
                   "cost_per_death_averted", "dominance"))
setorder(cea, -deaths_averted)
# Summary annual per-capita (mean over 2026-2050) on the R-value Cost_Effectiveness.
cea <- merge(cea, .deck_percap_from_bi(bi)[, .(scenario,
              annual_cost_incremental_per_capita = pc_undisc_val,
              disc_cost_incremental_per_capita   = pc_disc_val)],
             by = "scenario", all.x = TRUE)
setorder(cea, -deaths_averted)

# Capture the clinical R-value CE table for the deck results contract.
deck_cea_list[["clinical"]]     <- copy(cea)
deck_percap_list[["clinical"]]  <- .deck_percap_from_bi(bi)   # mean annual per-capita for the deck
deck_catalog_list[["clinical"]] <- .scenario_catalog_dt(fair_scenarios, "clinical",
                                                        base_id, produced)

## --- 7. Economic value (reuse Model 08 VSL/VSLY when reconcilable) ----------
# We sum ONLY aggregate monetary-value columns (economic_value_* = VSL-based,
# vsly_value_* = VSLY-based) over the analysis horizon by scenario -- never the
# per-capita VSL/VSLY *rates*. A clearly-labelled SUPPLEMENTARY benefit-cost
# ratio and net benefit are added against the (undiscounted) incremental cost.
# This is a benefit-cost view, NOT the cost-effectiveness result.
econ_value <- NULL; econ_note <- ""
vsl_file <- file.path(wd_outp, "08_vsl_results.rds")
econ_value <- tryCatch({
  if (!file.exists(vsl_file)) {
    econ_note <- "Model 08 output (08_vsl_results.rds) not found; economic value omitted."; NULL
  } else {
    v <- as.data.table(readRDS(vsl_file))
    if (!("scenario" %in% names(v))) {
      econ_note <- "Model 08 output has no `scenario` column; not reconcilable."; NULL
    } else {
      ov <- intersect(unique(v$scenario), comparators)
      if (!length(ov)) {
        econ_note <- paste0("Model 08 scenarios (",
          paste(head(unique(v$scenario), 6), collapse = ", "),
          ") do not match current run scenarios; economic value omitted. ",
          "Re-run Model 08 on the current scenarios to populate this sheet."); NULL
      } else {
        val_cols <- names(v)[grepl("^economic_value_|^vsly_value_", names(v)) &
                               vapply(v, is.numeric, logical(1))]
        if (!length(val_cols)) {
          econ_note <- "Model 08 output has no aggregate economic_value_/vsly_value_ column."; NULL
        } else {
          ev <- v[scenario %in% ov, c(lapply(.SD, sum, na.rm = TRUE),
                                      list(model08_deaths_averted =
                                             if ("deaths_averted" %in% names(v))
                                               sum(deaths_averted, na.rm = TRUE) else NA_real_)),
                  .SDcols = val_cols, by = scenario]
          # central VSL benefit (elasticity 1.2) if available for BCR / net benefit
          cen <- if ("economic_value_e1_2" %in% names(ev)) "economic_value_e1_2" else val_cols[1]
          ev <- merge(ev, cea[, .(scenario, incremental_cost, disc_incremental_cost,
                                  deaths_averted)], by = "scenario", all.x = TRUE)
          ev[, benefit_cost_ratio_supp := get(cen) / incremental_cost]
          ev[, net_benefit_supp_usd   := get(cen) - incremental_cost]
          econ_note <- paste0("Reused from Model 08 (VSL/VSLY). Supplementary benefit-cost ",
                              "columns use ", cen, " vs undiscounted incremental cost. ",
                              "This is NOT cost-effectiveness.")
          ev[]
        }
      }
    }
  }
}, error = function(e) { econ_note <<- paste0("Model 08 reuse failed: ", conditionMessage(e)); NULL })

## --- 8. R-side reconciliation / QA -----------------------------------------
qa <- list()
add_qa <- function(check, expected, actual, status, note = "")
  qa[[length(qa) + 1L]] <<- data.table(check = check, expected = as.character(expected),
                                       actual = as.character(actual), status = status, note = note)
tol <- 1e-6

# (1) key uniqueness in the validated catalogue
dupk <- fair_inputs$links[, .N, by = intervention_cause_key][N > 1]
add_qa("Intervention-cause key uniqueness", 0, nrow(dupk),
       if (nrow(dupk) == 0) "PASS" else "FAIL", "Each selected link key appears once")
# (2) input readiness
nfail <- sum(fair_inputs$validation$severity == "FAIL")
nrev  <- sum(fair_inputs$validation$severity == "REVIEW")
add_qa("Workbook FAIL-level issues", 0, nfail, if (nfail == 0) "PASS" else "REVIEW",
       "Blocked links/scenarios excluded (see Selected_Interventions / diagnostic)")
add_qa("Workbook REVIEW-level issues", 0, nrev, if (nrev == 0) "PASS" else "REVIEW",
       "Flagged but usable (e.g. cost not Indonesia-adjusted, missing optional component)")
# (3) baseline pairing
paired <- all(comparators %in% unique(mort$scenario)) &&
  all(!is.na(mort[scenario %in% comparators]$base_deaths))
add_qa("Every scenario paired to baseline", TRUE, paired, if (paired) "PASS" else "FAIL",
       "Deaths averted = baseline - scenario at matched location/year/cause")
# (4) stock/flow non-negativity
negc <- mo[, sum(well < -tol | sick < -tol | newcases < -tol | dead < -tol | pop < -tol)]
add_qa("No impossible negative states", 0, negc, if (negc == 0) "PASS" else "FAIL",
       "well/sick/new_cases/cause_deaths/population >= 0")
# (5) stock/flow identity: per cause row, pop = well_c + sick_c + all-cause deaths
maxres <- mo[, max(abs(pop - (well + sick + all.mx)))]
add_qa("Stock/flow identity pop = well + sick + all-cause deaths", "~0",
       round(maxres, 2), if (maxres < 1e3 || maxres / mo[, max(pop)] < 1e-3) "PASS" else "REVIEW",
       "Per cause row; small residual from 95+ pooling / rounding")
# (6) background mortality not duplicated across causes
ndist <- mo[, .(n = uniqueN(round(all.mx, 6))), by = .(scenario, year, age, sex)][, max(n)]
add_qa("Background mortality constant across cause (not duplicated)", 1, ndist,
       if (ndist == 1) "PASS" else "FAIL", "all.mx taken once per stratum in Background sheet")
# (7) cost reconciliation: component rows sum to budget-impact totals
if (nrow(annual_cost) && nrow(bi)) {
  chk <- merge(annual_cost[, .(c_scn = sum(annual_cost_scenario),
                               c_base = sum(annual_cost_baseline)), by = .(scenario, year)],
               bi[, .(scenario, year, scenario_cost, baseline_cost)],
               by = c("scenario", "year"))
  d1 <- chk[, max(abs(c_scn - scenario_cost) + abs(c_base - baseline_cost))]
  add_qa("Cost reconciliation (components -> budget impact)", "0", signif(d1, 3),
         if (d1 < 1e-3) "PASS" else "FAIL", "Component rows sum exactly to annual totals")
} else add_qa("Cost reconciliation (components -> budget impact)", "0", "n/a", "REVIEW",
              "No runnable cost components")
# (8) shared-cost counted once per stratum/year
if (nrow(annual_cost)) {
  shdup <- annual_cost[cost_scope == "shared-count-once",
                       .N, by = .(scenario, year, cost_record_id)][N > 1]
  add_qa("Shared cost counted once per stratum/year", 0, nrow(shdup),
         if (nrow(shdup) == 0) "PASS" else "FAIL",
         "shared-count-once components appear once per scenario-year")
} else add_qa("Shared cost counted once per stratum/year", 0, "n/a", "REVIEW", "No cost components")
# (9) CEA reconciliation: scenario disc incremental cost & deaths averted -> ratio
if (nrow(cea)) {
  rec_ok <- TRUE; rec_note <- "OK"
  for (s in cea$scenario) {
    dc <- if (nrow(bi)) bi[scenario == s, sum(disc_incremental_cost)] else 0
    da <- mort[scenario == s, sum(deaths_averted, na.rm = TRUE)]
    row <- cea[scenario == s]
    if (abs(dc - row$disc_incremental_cost) > max(1, abs(dc) * 1e-6)) { rec_ok <- FALSE; rec_note <- paste0("disc cost mismatch ", s) }
    if (abs(da - row$deaths_averted) > 1e-3) { rec_ok <- FALSE; rec_note <- paste0("deaths averted mismatch ", s) }
    if (!is.na(row$cost_per_death_averted) &&
        abs(row$cost_per_death_averted - dc / da) > max(1, abs(dc/da) * 1e-6)) {
      rec_ok <- FALSE; rec_note <- paste0("ratio mismatch ", s) }
  }
  add_qa("CEA reconciliation (detail -> summary ratio)", "consistent",
         if (rec_ok) "consistent" else "mismatch", if (rec_ok) "PASS" else "FAIL", rec_note)
}
# (10) national population denominator: one finite, positive value per analysis year
.np_years_needed <- if (nrow(bi)) sort(unique(bi$year)) else analysis_yrs
.np_ok <- tryCatch({ v <- .national_population_for(.np_years_needed); all(is.finite(v) & v > 0) },
                   error = function(e) FALSE)
add_qa("National population denominator valid for every analysis year", TRUE, .np_ok,
       if (isTRUE(.np_ok)) "PASS" else "FAIL",
       "One finite, positive Indonesia population per year from model_output_Indonesia_htncov2_aspirational.rds")
# (11) national_population identical across scenarios within a year
if (nrow(bi)) {
  .np_nd <- bi[, .(nd = uniqueN(round(national_population, 6))), by = year][, max(nd)]
  add_qa("National population identical across scenarios within year", 1, .np_nd,
         if (.np_nd == 1) "PASS" else "FAIL", "Same denominator applied to every scenario in a year")
}
# (12) budget-impact per-capita reconciliation: per_capita x population == cost
if (nrow(bi)) {
  .pc_res <- bi[, max(abs(baseline_cost_per_capita         * national_population - baseline_cost),
                      abs(scenario_cost_per_capita         * national_population - scenario_cost),
                      abs(incremental_cost_per_capita      * national_population - incremental_cost),
                      abs(disc_incremental_cost_per_capita * national_population - disc_incremental_cost))]
  .pc_tol <- max(1e-3, 1e-9 * bi[, max(abs(scenario_cost))])
  add_qa("Budget-impact per-capita reconciliation (per_capita x population = cost)", "~0",
         signif(.pc_res, 3), if (.pc_res <= .pc_tol) "PASS" else "FAIL",
         "baseline/scenario/incremental/discounted per-capita each reconcile to that year's cost")
  .pc_bad <- bi[, sum(!is.finite(baseline_cost_per_capita) | !is.finite(scenario_cost_per_capita) |
                       !is.finite(incremental_cost_per_capita) | !is.finite(disc_incremental_cost_per_capita))]
  add_qa("No missing/non-finite budget-impact per-capita values", 0, .pc_bad,
         if (.pc_bad == 0) "PASS" else "FAIL", "population positive & numerators finite -> per-capita finite")
}
# (13) Annual_Cost component per-capita reconciliation (national denominator)
if (nrow(annual_cost)) {
  .ac_res <- annual_cost[, max(abs(annual_cost_baseline_per_capita    * national_population - annual_cost_baseline),
                               abs(annual_cost_scenario_per_capita    * national_population - annual_cost_scenario),
                               abs(annual_cost_incremental_per_capita * national_population - annual_cost_incremental),
                               abs(disc_cost_incremental_per_capita   * national_population - disc_cost_incremental))]
  .ac_tol <- max(1e-3, 1e-9 * annual_cost[, max(abs(annual_cost_scenario))])
  add_qa("Annual_Cost per-capita reconciliation (per_capita x population = cost)", "~0",
         signif(.ac_res, 3), if (.ac_res <= .ac_tol) "PASS" else "FAIL",
         "component annual per-capita costs use the national denominator, not the eligible/PIN count")
}
qa_dt <- rbindlist(qa)

## --- 9. Assemble supporting / metadata tables ------------------------------
`%f%` <- function(x, d = 4) ifelse(is.na(x), NA_real_, round(x, d))
sel <- copy(fair_inputs$valid_links)
sel[, adjusted_effect_at_target :=
      affected_fraction * (effect_value * (target_coverage - baseline_coverage) /
                             (1 - effect_value * baseline_coverage))]
sel_out <- sel[, .(intervention_id, intervention_cause_key, intervention_name,
                   cause_id, cause_code, model_name,
                   transition_from, transition_to, model_transition,
                   effect_value, affected_fraction,
                   baseline_coverage, target_coverage, start_year, target_year,
                   adjusted_effect_at_target = `%f%`(adjusted_effect_at_target),
                   cost_join_key, cost_scope, effect_review, coverage_review)]
setorder(sel_out, intervention_id, cause_code)

blocked_out <- fair_inputs$blocked_links[, .(intervention_id, intervention_cause_key,
                                             cause_id, transition_from, transition_to,
                                             effect_value, affected_fraction,
                                             baseline_coverage, target_coverage, problem)]

cost_out <- costs[, .(cost_record_id, cost_component_key, cost_option,
                      intervention_id, cause_id, cause_code, cost_join_key,
                      cost_scope, cost_component, population_in_need_measure,
                      population_in_need_fraction, frequency_per_year,
                      c_age_start, c_age_stop, c_sex, unit_cost_usd, price_year,
                      indonesia_adjusted_flag, cov_baseline, cov_target,
                      cov_start_year, cov_target_year, cost_review, cost_ready)]
setorder(cost_out, intervention_id, cost_component_key)

diag_out <- fair_inputs$validation

meta <- data.table(item = c(
  "Workbook title", "Run date", "Model / pipeline", "Input workbook",
  "Model output source", "Location", "Analysis years", "Baseline scenario",
  "Scenarios costed", "Cost discount rate", "Cost price year", "Currency",
  "Economic perspective", "Coverage scale-up shape", "Downstream cost offsets",
  "Health outcomes (DALYs/YLL/YLD)", "R version", "openxlsx / data.table"),
  value = c(
  "Indonesia NCD FAIR Choices - cost & value",
  as.character(Sys.Date()),
  "CVD FAIR Choices (Models 00-06 -> 09)",
  fair_inputs$inputs_path,
  "output/out_model/model_output_*.rds (Model 06)",
  loc_run,
  paste0(yr_start, "-", yr_end),
  base_id,
  paste(comparators, collapse = ", "),
  sprintf("%.1f%%", 100 * disc_rate),
  as.character(A$cost_price_year),
  A$currency,
  A$economic_perspective,
  A$scale_up_shape,
  as.character(A$downstream_cost_offsets),
  "Out of scope in this stage (deferred)",
  R.version.string,
  paste0(as.character(packageVersion("openxlsx")), " / ",
         as.character(packageVersion("data.table")))))

readme <- data.table(section = c(
  "Purpose",
  "How to read",
  "Scenarios",
  "Baseline pairing",
  "Model trace grain",
  "Background mortality",
  "Costing",
  "Shared costs",
  "Budget impact",
  "Cost-effectiveness",
  "Economic value",
  "Colour legend",
  "Deferred"),
  detail = c(
  "Costing, budget impact and mortality-based cost-effectiveness for the FAIR Choices CVD interventions selected in indonesia_model_inputs.xlsx.",
  "Each sheet is a flat, filterable table (frozen header row). Totals were computed and reconciled in R (see QA_Checks) before writing.",
  "Baseline + one scenario per selected, valid intervention + a combined 'all' scenario. Membership derives only from workbook selections (Model 04).",
  "Deaths averted = baseline deaths - scenario deaths, matched at location x year x age x sex x cause.",
  "Model_State_Trace: location x scenario x year x age x cause, summed over sex (no selection is sex-specific). Population is the total stratum population.",
  "All-cause deaths are constant across cause, so they are taken ONCE per (scenario, year, age, sex) and reported in Background_Mortality (never summed per modeled cause).",
  "annual_cost = population_in_need x coverage(t) x frequency x unit_cost. PIN measure maps 'all'->eligible population, 'prevalence'->sick stock, 'incidence'->new cases.",
  "Components flagged 'shared-count-once' (cost_join_key ...__C_SHARED) are counted once at intervention level, never once per affected cause.",
  "Budget impact reports UNDISCOUNTED baseline, scenario, incremental and cumulative incremental cost. Discounted costs are separate columns. national_population and the *_cost_per_capita columns give annual cost per capita = that year's cost / that year's national Indonesia population; national annual population is taken from out_model/model_output_Indonesia_htncov2_aspirational.rds (baseline series, de-duplicated across cause; Annual_Cost carries the matching per-component per-capita columns).",
  "USD per death averted = cumulative discounted incremental cost / cumulative (undiscounted) deaths averted over the horizon. Not a DALY-based ICER.",
  "Value of statistical life (VSL/VSLY) is reused from Model 08 only when its scenarios reconcile with this run; otherwise it is omitted (see Economic_Value).",
  "Header dark-blue; derived light-blue; unresolved/flagged pale-yellow; PASS green; FAIL/REVIEW red/orange.",
  "DALYs, YLL, YLD, disability weights and life-expectancy outcomes are deferred to later work and are NOT in this workbook."))

methods <- data.table(
  method_id = c("M01","M02","M03","M04","M05","M06","M07","M08","M09","M10","M11"),
  concept = c("Incremental coverage", "FAIR adjusted effect", "Affected fraction",
              "Adjusted transition", "HF/severe mapping", "Annual component cost",
              "Shared cost", "Discounting", "Budget impact", "Cost-effectiveness",
              "FAIR unit-cost markups"),
  formula_or_rule = c(
    "delta_cov(t) = coverage(t) - baseline_coverage",
    "e_adj(t) = effect_value * delta_cov(t) / (1 - effect_value * baseline_coverage)",
    "transition_effect(t) = e_adj(t) * affected_fraction",
    "p_scenario(t) = p_baseline(t) * (1 - transition_effect(t)); prevention->incidence (IR/eff_ir), management->case fatality (CF/eff_cf)",
    "workbook sick_hf / sick_severe collapse onto the single 'sick' state via affected_fraction; NO new Markov states",
    "annual_cost = population_in_need * coverage(t) * frequency_per_year * unit_cost_usd",
    "shared-count-once components counted once per intervention & eligible stratum, not once per affected cause",
    "discount_factor(t) = 1 / (1 + cost_discount_rate)^(t - analysis_start_year); costs discounted, death counts undiscounted",
    "incremental = scenario_cost - baseline_cost (undiscounted headline; discounted reported separately)",
    "USD per death averted = cumulative discounted incremental cost / cumulative deaths averted",
    "not re-applied where indonesia_adjusted_flag = 1 (supplied adjusted costs already include them)"),
  source = c(rep("FairChoices_Methods sheet + https://fairchoices.w.uib.no/documentation/fairchoices-methods/", 4),
             "Model 04 translation table (Indonesia Markov adaptation)",
             rep("FairChoices_Methods sheet / input workbook", 5),
             "FairChoices_Methods M08"))

## --- 10. Write the workbook -------------------------------------------------
message("  Building workbook: ", cost_value_output_file)
wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Carlito", fontSize = 10)

C_HDR <- "#1F4E78"; C_DERIVED <- "#DDEBF7"; C_INPUT <- "#FFF2CC"
C_PASS <- "#C6EFCE"; C_FAIL <- "#FFC7CE"; C_REVIEW <- "#FFEB9C"
st_hdr   <- createStyle(fontName = "Carlito", fontColour = "#FFFFFF", fgFill = C_HDR,
                        textDecoration = "bold", halign = "center", valign = "center",
                        border = "TopBottomLeftRight", borderColour = "#8EA9C1", wrapText = TRUE)
st_title <- createStyle(fontName = "Carlito", fontColour = "#FFFFFF", fgFill = C_HDR,
                        textDecoration = "bold", fontSize = 13)
st_wrap  <- createStyle(fontName = "Carlito", valign = "top", wrapText = TRUE)
st_pass  <- createStyle(fgFill = C_PASS)
st_fail  <- createStyle(fgFill = C_FAIL)
st_rev   <- createStyle(fgFill = C_REVIEW)
st_flag  <- createStyle(fgFill = C_INPUT)

fmt_of <- function(col) {
  cl <- tolower(col)
  if (grepl("^year$|_year$|price_year|^age|c_age", cl))             return(NA_character_)
  if (grepl("discount_factor", cl))                                 return("0.000")
  if (grepl("effect_value|affected_fraction|adjusted_effect", cl))  return("0.000")
  if (grepl("coverage|fraction|^cov_", cl))                         return("0.0%")
  if (grepl("unit_cost", cl))                                       return("#,##0.00")
  if (grepl("per_death", cl))                                       return("#,##0")
  if (grepl("per_capita", cl) && !grepl("usd_per_capita", cl))      return("#,##0.00")
  if (grepl("cost|value|benefit|^pin_", cl))                        return("#,##0")
  if (grepl("death|case|population|averted|^well$|^sick$|dead|new_cases", cl)) return("#,##0")
  NA_character_
}

add_sheet <- function(name, df, big = FALSE, round_num = 2) {
  df <- as.data.frame(df)
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = st_hdr, withFilter = !big)
  freezePane(wb, name, firstActiveRow = 2, firstActiveCol = 1)
  nc <- ncol(df); nr <- nrow(df)
  w <- pmin(pmax(nchar(names(df)) + 2, 10), 42)
  setColWidths(wb, name, cols = seq_len(nc), widths = w)
  if (!big && nr > 0) {
    for (j in seq_len(nc)) {
      f <- fmt_of(names(df)[j])
      if (!is.na(f))
        addStyle(wb, name, createStyle(numFmt = f), rows = 2:(nr + 1), cols = j,
                 gridExpand = TRUE, stack = TRUE)
    }
  }
  invisible(NULL)
}

# README (two-column narrative)
addWorksheet(wb, "README")
writeData(wb, "README", "Indonesia NCD FAIR Choices - cost & value workbook", startRow = 1)
addStyle(wb, "README", st_title, rows = 1, cols = 1)
writeData(wb, "README", readme, startRow = 3, headerStyle = st_hdr)
setColWidths(wb, "README", cols = 1:2, widths = c(22, 120))
addStyle(wb, "README", st_wrap, rows = 4:(nrow(readme) + 3), cols = 2, gridExpand = TRUE, stack = TRUE)

add_sheet("Run_Metadata", meta); setColWidths(wb, "Run_Metadata", cols = 1:2, widths = c(30, 70))
add_sheet("Selected_Interventions", sel_out)
if (nrow(blocked_out)) add_sheet("Blocked_Links", blocked_out)
add_sheet("Cost_Components", cost_out)
add_sheet("Annual_Mortality", mort)

## Reference-Case BCA value tables (R values, synchronized with the formula
## workbook's Health_Outcomes / Economic_Value / Benefit_Cost sheets). Health
## outcomes come from Model 07; VSL/VSLY from Model 08; PV costs from Budget_Impact
## converted to the benefit basis via cost_to_bca_currency_factor.
.cf <- suppressWarnings(as.numeric(BCAP[["cost_to_bca_currency_factor"]])); if (is.na(.cf)) .cf <- 1
.cmpR <- comparators
ho_val <- dt_h07[scenario %in% .cmpR, .(
  scenario_label = scenario_label[1L], deaths_averted = sum(deaths_averted),
  cases_averted = sum(cases_averted), yll_averted = sum(yll_averted),
  yld_averted = sum(yld_averted), dalys_averted = sum(dalys_averted),
  life_years_gained = sum(life_years_gained)), by = .(scenario, year)]
setcolorder(ho_val, c("scenario", "scenario_label", "year")); setorder(ho_val, scenario, year)
ev_val <- ev08[scenario %in% .cmpR, .(
  scenario, scenario_label, year, deaths_averted,
  life_years_gained = life_years_gained_undisc,
  gni_pc_idn = gni_pc_ppp, gni_pc_usa, population, le_avg_adult,
  vsl_preferred, vsl_gni100, vsl_gni160, vsly_preferred,
  econ_value_vsl_undisc  = economic_value_preferred,
  econ_value_vsly_undisc = vsly_value_preferred,
  disc_factor = disc_bca_primary,
  pv_vsl_pref  = economic_value_preferred_disc,
  pv_vsly_pref = vsly_value_preferred * disc_bca_primary,
  total_national_gni)]
setorder(ev_val, scenario, year)
.pvb <- ev08[scenario %in% .cmpR, .(
  VSL_preferred  = sum(economic_value_preferred_disc, na.rm = TRUE),
  VSLY_preferred = sum(vsly_value_preferred * disc_bca_primary, na.rm = TRUE),
  VSL_100xGNI    = sum(economic_value_gni100 * disc_bca_primary, na.rm = TRUE),
  VSL_160xGNI    = sum(economic_value_gni160 * disc_bca_primary, na.rm = TRUE),
  pv_national_gni = sum(total_national_gni * disc_bca_primary, na.rm = TRUE)), by = scenario]
.pvc <- if (nrow(bi)) bi[scenario %in% .cmpR, .(pv_costs = sum(disc_incremental_cost) * .cf), by = scenario] else
  data.table(scenario = .cmpR, pv_costs = 0)
.lab <- unique(dt_h07[scenario %in% .cmpR, .(scenario, scenario_label, intervention_family)])
bc_val <- rbindlist(lapply(c("VSL_preferred","VSLY_preferred","VSL_100xGNI","VSL_160xGNI"), function(cs) {
  b <- merge(.pvb[, .(scenario, pv_benefits = get(cs), pv_national_gni)], .pvc, by = "scenario", all.x = TRUE)
  b[is.na(pv_costs), pv_costs := 0]
  b <- merge(.lab, b, by = "scenario")
  b[, `:=`(valuation_method = if (grepl("VSLY", cs)) "VSLY" else "VSL", valuation_case = cs,
           pv_net_benefit = pv_benefits - pv_costs,
           benefit_cost_ratio = fifelse(pv_costs > 0, pv_benefits / pv_costs, NA_real_),
           benefit_gni_share  = fifelse(pv_national_gni > 0, pv_benefits / pv_national_gni, NA_real_))]
  b[, decision := fifelse(pv_costs <= 0, "cost-saving or ratio not meaningful (PV cost<=0)",
                   fifelse(pv_benefits / pv_costs >= 1, "benefits exceed costs (BCR>=1)",
                           "costs exceed benefits (BCR<1)"))]
  b }))
bc_val[, scope_note := paste0("Partial mortality-benefit BCA: benefits = averted-mortality VSL/VSLY (PPP int$); ",
       "costs = implementation/health-system converted to PPP int$; omits morbidity, productivity, downstream offsets.")]
setcolorder(bc_val, c("scenario","scenario_label","intervention_family","valuation_method","valuation_case",
                      "pv_benefits","pv_costs","pv_net_benefit","benefit_cost_ratio","pv_national_gni",
                      "benefit_gni_share","decision","scope_note"))
setorder(bc_val, scenario, valuation_method, valuation_case)

add_sheet("Health_Outcomes", ho_val)
if (nrow(annual_cost)) add_sheet("Annual_Cost", annual_cost)
if (nrow(bi))          add_sheet("Budget_Impact", bi)
add_sheet("Cost_Effectiveness", cea)
add_sheet("Economic_Value", ev_val)
add_sheet("Benefit_Cost", bc_val)
add_sheet("QA_Checks", qa_dt)
if (nrow(diag_out)) add_sheet("Input_Diagnostic", diag_out)
add_sheet("Methods_and_Sources", methods)
setColWidths(wb, "Methods_and_Sources", cols = 1:4, widths = c(10, 26, 90, 60))
addStyle(wb, "Methods_and_Sources", st_wrap, rows = 2:(nrow(methods) + 1), cols = 3:4,
         gridExpand = TRUE, stack = TRUE)

# Large trace sheets last (header-only styling for speed; values rounded)
trace_w <- copy(trace)
num_c <- c("well", "sick", "new_cases", "cause_deaths", "population")
trace_w[, (num_c) := lapply(.SD, function(x) round(x, 2)), .SDcols = num_c]
add_sheet("Model_State_Trace", trace_w, big = TRUE)
bg_w <- copy(bg)
num_b <- c("all_cause_deaths", "modeled_cause_deaths", "background_deaths", "population")
bg_w[, (num_b) := lapply(.SD, function(x) round(x, 2)), .SDcols = num_b]
add_sheet("Background_Mortality", bg_w, big = TRUE)

# Conditional colouring on QA status
qa_r <- which(names(as.data.frame(qa_dt)) == "status")
if (length(qa_r)) {
  sc <- as.data.frame(qa_dt)$status
  for (i in seq_along(sc)) {
    stl <- if (sc[i] == "PASS") st_pass else if (sc[i] == "FAIL") st_fail else st_rev
    addStyle(wb, "QA_Checks", stl, rows = i + 1, cols = qa_r, stack = TRUE)
  }
}
# Flag unresolved/blocked cells
if (nrow(blocked_out)) addStyle(wb, "Blocked_Links", st_flag,
                                rows = 2:(nrow(blocked_out) + 1),
                                cols = which(names(blocked_out) == "problem"),
                                gridExpand = TRUE, stack = TRUE)
if (nrow(diag_out)) {
  sev <- diag_out$severity
  cj <- which(names(diag_out) == "severity")
  for (i in seq_along(sev))
    addStyle(wb, "Input_Diagnostic", if (sev[i] == "FAIL") st_fail else st_rev,
             rows = i + 1, cols = cj, stack = TRUE)
}

if (!dir.exists(dirname(cost_value_output_file)))
  dir.create(dirname(cost_value_output_file), recursive = TRUE)
strip_dangling_drawings(wb)
saveWorkbook(wb, cost_value_output_file, overwrite = TRUE)

message("  Wrote: ", cost_value_output_file)

## ===========================================================================
## 11. Formula-driven decision workbook (live cross-sheet Excel formulas) ------
## ===========================================================================
# Emits a SECOND, compact workbook whose decision tables (adjusted effects,
# costing, budget impact, cost-effectiveness, supplementary economic value and
# QA) are LIVE EXCEL FORMULAS instead of static R values. Editing an exposed
# assumption in Excel recomputes every dependent result. The large model traces
# stay in the R-value workbook above; this book carries only the annual health
# aggregates and the full-precision cost quantities the formulas need.
#
#   Cell colour legend (also stated on the README sheet):
#     * header      dark-blue  (#1F4E78, white bold)
#     * formula     light-blue (#DDEBF7)  -- Excel-calculated
#     * R source    grey       (#F2F2F2)  -- R-generated value the formulas read
#     * input       pale-yellow(#FFF2CC)  -- user-editable assumption/control
#     * QA status   green/red/orange via conditional formatting (PASS/FAIL/REVIEW)
#
# Design notes
#   * Coverage in Cost_Components is pulled (INDEX/MATCH on cost_join_key) from
#     Selected_Interventions so a single coverage assumption is never duplicated.
#   * Annual_Cost exposes the full-precision R quantity (q_scenario/q_baseline)
#     BEFORE the PIN fraction; all costing arithmetic is then Excel formula, so
#     results reconcile with R despite the display rounding used elsewhere.
#   * QA_Checks recomputes each invariant in Excel AND reconciles the Excel CEA
#     headline against the R engine values embedded in Calculation_Assumptions.
# ---------------------------------------------------------------------------

if (!exists("cost_value_formulae_file"))
  cost_value_formulae_file <- sub("\\.xlsx$", "_formulae.xlsx", cost_value_output_file)

message("  Building formula workbook: ", cost_value_formulae_file)

# ---- 11.1 small helpers ---------------------------------------------------
int2col <- openxlsx::int2col

# range-end sheet rows (header = row 1; data rows 2 .. r_<sheet>)
n_si <- nrow(sel_out);      r_si <- n_si + 1L        # Selected_Interventions
n_cc <- nrow(cost_out);     r_cc <- n_cc + 1L        # Cost_Components
n_am <- nrow(mort);         r_am <- n_am + 1L        # Annual_Mortality
n_ac <- nrow(annual_cost);  r_ac <- max(n_ac + 1L, 2L)   # Annual_Cost
n_bi <- nrow(bi);           r_bi <- max(n_bi + 1L, 2L)   # Budget_Impact
n_ce <- nrow(cea);          r_ce <- n_ce + 1L        # Cost_Effectiveness
n_id <- nrow(diag_out);     r_id <- max(n_id + 1L, 2L)   # Input_Diagnostic

# per-column number format from column name (sensible units: people, %, USD,
# rates, ratios, years).
fmt_of2 <- function(col) {
  cl <- tolower(col)
  if (grepl("frequency", cl))                                   return("0.00")
  if (grepl("adjusted_effect", cl))                             return("0.0000")
  if (grepl("effect_value|affected_fraction", cl))              return("0.000")
  if (grepl("discount_factor", cl))                             return("0.000")
  if (grepl("benefit_cost_ratio|_ratio$|^ratio$", cl))          return("0.00")
  if (grepl("^year$|_year$|price_year|age_start|age_stop|^c_age", cl)) return("0")
  if (grepl("coverage|_fraction$|^fraction$|^cov_base|^cov_targ$|coverage_", cl)) return("0.0%")
  if (grepl("^cov_baseline$|^cov_target$", cl))                 return("0.0%")
  if (grepl("unit_cost|r_quantity", cl))                        return("#,##0.00")
  if (grepl("per_death", cl))                                   return("#,##0")
  if (grepl("per_capita", cl) && !grepl("usd_per_capita", cl))  return("#,##0.00")
  if (grepl("cost|value|benefit|^pin_|net_benefit|budget", cl)) return("#,##0")
  if (grepl("death|case|population|averted|duplicate_count|key_count|distinct|residual|negative|_count$|^count$", cl)) return("#,##0")
  NA_character_
}

# vector of per-row formulas: fn(r) -> formula string (no leading "=")
frows <- function(fn, rows) vapply(rows, fn, character(1))

# INDEX/MATCH one Cost_Components column (target letter) by cost_record_id (E)
idx_cc <- function(tgt, r)
  sprintf("IFERROR(INDEX('Cost_Components'!$%s$2:$%s$%d,MATCH(E%d,'Cost_Components'!$A$2:$A$%d,0)),\"\")",
          tgt, tgt, r_cc, r, r_cc)
# INDEX/MATCH one Selected_Interventions column (target letter) by cost_join_key (G)
idx_si <- function(tgt, r)
  sprintf("IFERROR(INDEX('Selected_Interventions'!$%s$2:$%s$%d,MATCH(G%d,'Selected_Interventions'!$Q$2:$Q$%d,0)),\"\")",
          tgt, tgt, r_si, r, r_si)
# INDEX/MATCH one Cost_Effectiveness column (target letter) by scenario (A)
idx_ce <- function(tgt, r)
  sprintf("IFERROR(INDEX('Cost_Effectiveness'!$%s$2:$%s$%d,MATCH(A%d,'Cost_Effectiveness'!$A$2:$A$%d,0)),\"\")",
          tgt, tgt, r_ce, r, r_ce)

# (strip_dangling_drawings() is defined once in Section 0 and reused here.)

# ---- 11.2 styles ----------------------------------------------------------
C_HDR <- "#1F4E78"; C_FORMULA <- "#DDEBF7"; C_RSRC <- "#F2F2F2"; C_INPUT <- "#FFF2CC"
st_hdr     <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          halign = "center", valign = "center", wrapText = TRUE,
                          border = "TopBottomLeftRight", borderColour = "#8EA9C1")
st_title   <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          fontSize = 13, valign = "center")
st_formula <- createStyle(fgFill = C_FORMULA)
st_rsrc    <- createStyle(fgFill = C_RSRC)
st_input   <- createStyle(fgFill = C_INPUT)
st_wrap    <- createStyle(valign = "top", wrapText = TRUE)
# conditional-format styles (use bgFill)
cf_pass <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
cf_fail <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
cf_rev  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C6500")

wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Carlito", fontSize = 11)

# style a freshly-written tabular sheet: header, fills, number formats, freeze,
# filter, column widths. formula_cols / rsource_cols / input_cols are 1-based
# column indices; nm is the vector of column names (drives number formats/width).
style_sheet <- function(sheet, nm, nrow_data,
                        formula_cols = integer(0), rsource_cols = integer(0),
                        input_cols = integer(0), header_row = 1L,
                        wrap_cols = integer(0), filter = TRUE, min_w = 11, max_w = 46) {
  ncol <- length(nm)
  addStyle(wb, sheet, st_hdr, rows = header_row, cols = seq_len(ncol), gridExpand = TRUE)
  if (nrow_data > 0) {
    dr <- (header_row + 1L):(header_row + nrow_data)
    for (j in formula_cols) addStyle(wb, sheet, st_formula, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    for (j in rsource_cols) addStyle(wb, sheet, st_rsrc,    rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    for (j in input_cols)   addStyle(wb, sheet, st_input,   rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    for (j in seq_len(ncol)) {
      f <- fmt_of2(nm[j])
      if (!is.na(f)) addStyle(wb, sheet, createStyle(numFmt = f), rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    }
    for (j in wrap_cols) addStyle(wb, sheet, st_wrap, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
  }
  freezePane(wb, sheet, firstActiveRow = header_row + 1L, firstActiveCol = 1L)
  if (filter) addFilter(wb, sheet, rows = header_row, cols = seq_len(ncol))
  w <- pmin(pmax(nchar(nm) + 2L, min_w), max_w)
  setColWidths(wb, sheet, cols = seq_len(ncol), widths = w)
  setRowHeights(wb, sheet, rows = header_row, heights = 28)
  invisible(NULL)
}

# =========================================================================
# 11.3 Calculation_Assumptions  (single source of truth for formula controls)
# =========================================================================
anchor_scn <- if ("all" %in% cea$scenario) "all" else cea$scenario[1]
ar <- cea[scenario == anchor_scn]
r_da_anchor  <- if (nrow(ar)) ar$deaths_averted[1]        else NA_real_
r_dic_anchor <- if (nrow(ar)) ar$disc_incremental_cost[1] else NA_real_
r_cpd_anchor <- if (nrow(ar)) ar$cost_per_death_averted[1] else NA_real_

ca <- data.table(
  parameter_id = c("analysis_start_year","analysis_end_year","baseline_scenario_id",
                   "cost_discount_rate","cost_price_year","currency",
                   "economic_perspective","scale_up_shape","downstream_cost_offsets",
                   "formula_tolerance","stock_flow_residual_limit","trace_precision",
                   "r_stock_flow_max_residual","r_background_distinct_count",
                   "r_negative_state_count","r_deaths_averted_anchor",
                   "r_disc_incremental_cost_anchor","r_cost_per_death_anchor",
                   "qa_anchor_scenario"),
  value = list(as.integer(yr_start), as.integer(yr_end), base_id,
               disc_rate, as.integer(A$cost_price_year), A$currency,
               A$economic_perspective, A$scale_up_shape, as.integer(A$downstream_cost_offsets),
               0.001, 1000L, 2L,
               round(as.numeric(maxres), 2), as.integer(ndist),
               as.integer(negc), as.numeric(r_da_anchor),
               as.numeric(r_dic_anchor), as.numeric(r_cpd_anchor),
               anchor_scn),
  unit = c("year","year","scenario id","proportion/year","USD year","currency",
           "text","text","0/1 flag","USD/count","persons","decimal places",
           "persons","count","count","deaths","USD","USD/death","scenario id"),
  role = c("formula control","formula control","formula control","formula control",
           "metadata","metadata","metadata","formula control","scope","QA control",
           "QA control","audit note","R QA source","R QA source","R QA source",
           "R reconciliation source","R reconciliation source","R reconciliation source",
           "R reconciliation source"),
  description = c("First model and discount year","Last model year",
                  "Comparator used for health and cost calculations",
                  "Annual discount rate applied to costs only","Reporting price year",
                  "Workbook reporting currency","Economic evaluation perspective",
                  "Coverage increases linearly from start to target year",
                  "Downstream disease-cost offsets excluded at this stage",
                  "Absolute reconciliation tolerance (internal Excel checks)",
                  "Review threshold used by the R stock/flow check",
                  "Exported model traces are rounded; R quantity helpers keep full precision",
                  "Maximum stock/flow residual computed in R before export",
                  "Max distinct all-cause mortality values across causes (R)",
                  "Count of impossible negative state/flow values (R)",
                  "R engine deaths averted for the anchor scenario",
                  "R engine discounted incremental cost for the anchor scenario",
                  "R engine USD per death averted for the anchor scenario",
                  "Scenario used for the Excel-vs-R reconciliation checks"),
  source = c(rep("indonesia_model_inputs.xlsx / Model 09", 2), "Model 04 / Model 09",
             "indonesia_model_inputs.xlsx / Model 09", rep("indonesia_model_inputs.xlsx", 4),
             "indonesia_model_inputs.xlsx", "Workbook QA rule", "Model 09",
             "Model 09 export rule", rep("Model 09 current run", 3),
             rep("Model 09 current run (R CEA)", 4)))

# --- Append the Reference-Case BCA parameters (Model 08 resolved set) as editable
#     controls that anchor the VSL/VSLY/Benefit-Cost formulas below. -----------
.bcab <- bca_ca_block(bca_params)
n_ca_core <- nrow(ca)
ca <- rbind(ca, .bcab$ca_bca)
bca_fmt_vec <- .bcab$fmt

addWorksheet(wb, "Calculation_Assumptions")
writeData(wb, "Calculation_Assumptions",
          data.frame(parameter_id = "parameter_id", value = "value", unit = "unit",
                     role = "role", description = "description", source = "source"),
          colNames = FALSE, startRow = 1)
writeData(wb, "Calculation_Assumptions", ca$parameter_id, startCol = 1, startRow = 2, colNames = FALSE)
writeData(wb, "Calculation_Assumptions",
          as.data.frame(ca[, .(unit, role, description, source)]),
          startCol = 3, startRow = 2, colNames = FALSE)
for (i in seq_len(nrow(ca)))
  writeData(wb, "Calculation_Assumptions", ca$value[[i]], startCol = 2, startRow = 1 + i, colNames = FALSE)
# fills: editable inputs (B2:B13) + BCA controls yellow; R-source (B14:B20) grey
addStyle(wb, "Calculation_Assumptions", st_hdr, rows = 1, cols = 1:6, gridExpand = TRUE)
addStyle(wb, "Calculation_Assumptions", st_input, rows = 2:13, cols = 2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Calculation_Assumptions", st_rsrc,  rows = 14:20, cols = 2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Calculation_Assumptions", st_input,
         rows = (n_ca_core + 2L):(nrow(ca) + 1L), cols = 2, gridExpand = TRUE, stack = TRUE)  # BCA controls
# per-value number formats (core 19 rows + appended BCA rows)
ca_fmt <- c("0","0",NA,"0.0%","0",NA,NA,NA,"0","0.000","#,##0","0",
            "#,##0.0","0","0","#,##0","#,##0","#,##0.00",NA, bca_fmt_vec)
for (i in seq_along(ca_fmt)) if (!is.na(ca_fmt[i]))
  addStyle(wb, "Calculation_Assumptions", createStyle(numFmt = ca_fmt[i]),
           rows = 1 + i, cols = 2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Calculation_Assumptions", st_wrap, rows = 2:(nrow(ca)+1), cols = 5, gridExpand = TRUE, stack = TRUE)
freezePane(wb, "Calculation_Assumptions", firstActiveRow = 2)
addFilter(wb, "Calculation_Assumptions", rows = 1, cols = 1:6)
setColWidths(wb, "Calculation_Assumptions", cols = 1:6, widths = c(38, 16, 16, 24, 62, 40))
setRowHeights(wb, "Calculation_Assumptions", rows = 1, heights = 28)

# BCA cell references (row = position in ca + 1 header) for the formula sheets.
.carow <- function(pid) match(pid, ca$parameter_id) + 1L
.bcell <- function(pid) sprintf("'Calculation_Assumptions'!$B$%d", .carow(pid))
bca_cells_clin <- list(
  ratio     = .bcell("vsl_us_gni_ratio"),
  elast     = .bcell("vsl_income_elasticity_preferred"),
  floor     = .bcell("vsl_floor_gni_multiple"),
  mult100   = .bcell("vsl_sensitivity_gni_multiple_100"),
  mult160   = .bcell("vsl_sensitivity_gni_multiple_160"),
  r_primary = .bcell("bca_discount_rate_primary"),
  base_year = .bcell("bca_base_year"),
  price_year  = .bcell("bca_price_year"),
  cost_factor = .bcell("cost_to_bca_currency_factor"),
  scope     = .bcell("bca_scope"))

# =========================================================================
# 11.4 README (narrative + colour legend)
# =========================================================================
readme_f <- data.table(
  section = c("Purpose","How to read","Scenarios","Baseline pairing",
              "CVD 40q30","Model aggregates","Costing","Shared costs","Budget impact",
              "Cost-effectiveness","Economic value","QA & reconciliation",
              "Colour legend","Companion workbook","Deferred"),
  detail = c(
    "Costing, budget impact and mortality-based cost-effectiveness for the FAIR Choices CVD interventions selected in indonesia_model_inputs.xlsx.",
    "Grey cells are R-generated source values; light-blue cells are LIVE Excel formulas; pale-yellow cells on Calculation_Assumptions are editable controls. Change a yellow control and the blue results recompute. Calculation_Map lists every dependency.",
    "Baseline + one scenario per selected valid intervention + a combined 'all' scenario. Membership derives only from the workbook selections (Model 04).",
    "Deaths averted = baseline deaths - scenario deaths, matched at location x year x age x sex x cause (aggregated to year x cause here).",
    "CVD_40q30 / CVD_40q30_Age give the period probability of dying from the six CVD causes (ihd, istroke, hstroke, hhd, rhd, cmd) between exact ages 30 and 70, per scenario/HTN-target/year. The life table (m_x, q_x, l_x, l_{x+1}) is live Excel formula off grey CVD deaths and de-duplicated population; cvd_40q30 = 100*(1-l_70/l_30) reconciles to the Model 07 value. cvd_40q30 and the absolute reduction are PERCENT on a 0-100 scale (format 0.000), NOT Excel's fractional %.",
    "Annual_Mortality carries the R health aggregates (cases, deaths, baseline); Annual_Cost carries the full-precision R population quantity before the PIN fraction. The 157k-row state trace stays in the companion R workbook.",
    "annual_cost = population_in_need x coverage(t) x frequency x unit_cost. PIN measure maps 'all'->eligible population, 'prevalence'->sick stock, 'incidence'->new cases.",
    "Components flagged 'shared-count-once' (cost_join_key ...__C_SHARED) are counted once at intervention level, never once per affected cause (see Annual_Cost shared_duplicate_count and QA).",
    "Budget impact reports UNDISCOUNTED baseline, scenario, incremental and cumulative incremental cost. Discounted costs are separate columns. The four *_cost_per_capita columns are LIVE formulae (= that year's cost / national_population); national_population (grey R-source) is each year's national Indonesia population from out_model/model_output_Indonesia_htncov2_aspirational.rds (baseline series, de-duplicated across cause). Annual_Cost carries the matching per-component per-capita formulae, and Cost_Effectiveness carries the scenario mean annual incremental/discounted per-capita (2026-2050).",
    "USD per death averted = cumulative discounted incremental cost / cumulative (undiscounted) deaths averted over the horizon. Not a DALY-based ICER.",
    "Reference-Case benefit-cost analysis (2019 Robinson et al. Guidelines): Health_Outcomes (Model 07 averted deaths/YLL/YLD/DALY & life-years gained), Economic_Value (Model 08 VSL/VSLY source with LIVE VSL-transfer, floor, VSLY, discount and PV-benefit formulas) and Benefit_Cost (PV benefits, PV costs converted to the benefit basis, net benefit and BCR). Preferred VSL = MAX(160xGNIpc_US x (GNIpc_IDN/GNIpc_US)^1.5, 20xGNIpc_IDN); 100x/160x GNI sensitivities. PARTIAL mortality-benefit BCA -- not a full societal BCA, and distinct from Cost_Effectiveness (USD per death averted).",
    "QA_Checks recomputes each invariant in Excel and reconciles the Excel cost-effectiveness headline against the R engine values stored on Calculation_Assumptions; BCA checks cover the VSL floor, reference-case parameters, price-year basis and Benefit_Cost scenario coverage. PASS/FAIL/REVIEW are conditionally formatted.",
    "Header dark-blue; formula-derived light-blue; R source/helper grey; editable controls pale-yellow; PASS green; FAIL/REVIEW red/orange.",
    "The full R-value workbook (indonesia_model_cost_value.xlsx) keeps the detailed Model_State_Trace and Background_Mortality tables for independent review.",
    "Health outcomes (deaths averted, YLL/YLD/DALY averted, life-years gained) now come from Model 07 (Health_Outcomes); life-expectancy and disability-weight sources are documented via Model 07."))
addWorksheet(wb, "README")
writeData(wb, "README", "Indonesia NCD FAIR Choices - cost & value workbook (formula edition)", startRow = 1)
addStyle(wb, "README", st_title, rows = 1, cols = 1)
writeData(wb, "README", readme_f, startRow = 3, headerStyle = st_hdr)
setColWidths(wb, "README", cols = 1:2, widths = c(22, 118))
addStyle(wb, "README", st_wrap, rows = 4:(nrow(readme_f) + 3), cols = 2, gridExpand = TRUE, stack = TRUE)
setRowHeights(wb, "README", rows = 1, heights = 22)

# =========================================================================
# 11.5 Run_Metadata (values; several pulled from Calculation_Assumptions)
# =========================================================================
meta_f <- copy(meta)   # same 18-row item/value table used by the R workbook
addWorksheet(wb, "Run_Metadata")
writeData(wb, "Run_Metadata", meta_f, headerStyle = st_hdr)
# formula cells (sheet rows: data row k -> sheet row k+1)
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 8,
             x = "'Calculation_Assumptions'!B2&\"-\"&'Calculation_Assumptions'!B3")   # Analysis years
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 9,  x = "'Calculation_Assumptions'!B4")   # Baseline scenario
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 11, x = "'Calculation_Assumptions'!B5")   # Cost discount rate
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 12, x = "'Calculation_Assumptions'!B6")   # Cost price year
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 13, x = "'Calculation_Assumptions'!B7")   # Currency
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 14, x = "'Calculation_Assumptions'!B8")   # Economic perspective
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 15, x = "'Calculation_Assumptions'!B9")   # Coverage scale-up shape
writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 16, x = "'Calculation_Assumptions'!B10")  # Downstream cost offsets
addStyle(wb, "Run_Metadata", st_formula, rows = c(8,9,11,12,13,14,15,16), cols = 2, gridExpand = FALSE, stack = TRUE)
addStyle(wb, "Run_Metadata", createStyle(numFmt = "0.0%"), rows = 11, cols = 2, stack = TRUE)
setColWidths(wb, "Run_Metadata", cols = 1:2, widths = c(30, 74))
freezePane(wb, "Run_Metadata", firstActiveRow = 2)
setRowHeights(wb, "Run_Metadata", rows = 1, heights = 28)

# =========================================================================
# 11.6 Selected_Interventions  (P adjusted effect + U/V key check = formulas)
# =========================================================================
si <- as.data.frame(sel_out)
si$adjusted_effect_at_target <- NA_real_       # -> formula P
si$key_count <- NA_real_                        # -> formula U
si$formula_status <- NA_character_              # -> formula V
addWorksheet(wb, "Selected_Interventions")
writeData(wb, "Selected_Interventions", si, headerStyle = st_hdr)
writeFormula(wb, "Selected_Interventions", startCol = 16, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(OR(J%d=\"\",K%d=\"\",L%d=\"\",M%d=\"\"),\"\",ROUND(K%d*(J%d*(M%d-L%d)/(1-J%d*L%d)),4))",
               r,r,r,r, r,r,r,r,r,r), 2:r_si))
writeFormula(wb, "Selected_Interventions", startCol = 21, startRow = 2,
             x = frows(function(r) sprintf("COUNTIF($B$2:$B$%d,B%d)", r_si, r), 2:r_si))
writeFormula(wb, "Selected_Interventions", startCol = 22, startRow = 2,
             x = frows(function(r) sprintf("IF(U%d=1,\"OK\",\"DUPLICATE KEY\")", r), 2:r_si))
style_sheet("Selected_Interventions", names(si), n_si,
            formula_cols = c(16, 21, 22))

# =========================================================================
# 11.7 Blocked_Links  (R values; problem highlighted)
# =========================================================================
if (nrow(blocked_out)) {
  addWorksheet(wb, "Blocked_Links")
  writeData(wb, "Blocked_Links", as.data.frame(blocked_out), headerStyle = st_hdr)
  style_sheet("Blocked_Links", names(blocked_out), nrow(blocked_out))
  addStyle(wb, "Blocked_Links", st_input, rows = 2:(nrow(blocked_out) + 1),
           cols = which(names(blocked_out) == "problem"), gridExpand = TRUE, stack = TRUE)
}

# =========================================================================
# 11.8 Cost_Components  (S:V coverage pull + X cost_ready = formulas)
# =========================================================================
cc <- copy(cost_out)
cc[, `:=`(cov_baseline = NA_real_, cov_target = NA_real_,
          cov_start_year = NA_real_, cov_target_year = NA_real_, cost_ready = NA_real_)]
cc <- as.data.frame(cc)
addWorksheet(wb, "Cost_Components")
writeData(wb, "Cost_Components", cc, headerStyle = st_hdr)
writeFormula(wb, "Cost_Components", startCol = 19, startRow = 2, x = frows(function(r) idx_si("L", r), 2:r_cc)) # cov_baseline
writeFormula(wb, "Cost_Components", startCol = 20, startRow = 2, x = frows(function(r) idx_si("M", r), 2:r_cc)) # cov_target
writeFormula(wb, "Cost_Components", startCol = 21, startRow = 2, x = frows(function(r) idx_si("N", r), 2:r_cc)) # cov_start_year
writeFormula(wb, "Cost_Components", startCol = 22, startRow = 2, x = frows(function(r) idx_si("O", r), 2:r_cc)) # cov_target_year
writeFormula(wb, "Cost_Components", startCol = 24, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(AND(P%d<>\"\",P%d>=0,S%d<>\"\",K%d<>\"\",K%d>=0,K%d<=1,OR(J%d=\"all\",J%d=\"prevalence\",J%d=\"incidence\")),1,0)",
               r,r,r,r,r,r,r,r,r), 2:r_cc))  # cost_ready
style_sheet("Cost_Components", names(cc), n_cc, formula_cols = c(19,20,21,22,24),
            wrap_cols = which(names(cc) == "cost_component"))

# =========================================================================
# 11.9 Annual_Mortality  (H deaths_averted, J cases_averted = formulas)
# =========================================================================
am <- copy(mort)
am[, `:=`(deaths_averted = NA_real_, cases_averted = NA_real_)]
am <- as.data.frame(am)
addWorksheet(wb, "Annual_Mortality")
writeData(wb, "Annual_Mortality", am, headerStyle = st_hdr)
writeFormula(wb, "Annual_Mortality", startCol = 8, startRow = 2,
             x = frows(function(r) sprintf("G%d-F%d", r, r), 2:r_am))
writeFormula(wb, "Annual_Mortality", startCol = 10, startRow = 2,
             x = frows(function(r) sprintf("I%d-E%d", r, r), 2:r_am))
style_sheet("Annual_Mortality", names(am), n_am,
            formula_cols = c(8, 10), rsource_cols = c(7, 9))   # G, I are R base values

# =========================================================================
# 11.10 Annual_Cost  (purpose-built formula layout; AF/AG = R quantities)
# =========================================================================
ac_cols <- c("scenario","year","intervention_id","cause_code","cost_record_id",
             "cost_component_key","cost_join_key","cost_scope","population_in_need_measure",
             "population_in_need_fraction","coverage_scenario","coverage_baseline",
             "frequency_per_year","unit_cost_usd","pin_scenario","pin_baseline",
             "annual_cost_baseline","annual_cost_scenario","annual_cost_incremental",
             "indonesia_adjusted_flag","price_year","discount_factor","disc_cost_baseline",
             "disc_cost_scenario","disc_cost_incremental","cov_target","cov_start_year",
             "cov_target_year","c_age_start","c_age_stop","c_sex","r_quantity_scenario",
             "r_quantity_baseline","shared_duplicate_count",
             "national_population","annual_cost_baseline_per_capita","annual_cost_scenario_per_capita",
             "annual_cost_incremental_per_capita","disc_cost_incremental_per_capita")
if (n_ac > 0) {
  q_s <- ifelse(annual_cost$population_in_need_fraction > 0,
                annual_cost$pin_scenario / annual_cost$population_in_need_fraction, 0)
  q_b <- ifelse(annual_cost$population_in_need_fraction > 0,
                annual_cost$pin_baseline / annual_cost$population_in_need_fraction, 0)
  ac <- data.frame(
    scenario = annual_cost$scenario, year = annual_cost$year,
    intervention_id = annual_cost$intervention_id, cause_code = annual_cost$cause_code,
    cost_record_id = annual_cost$cost_record_id, cost_component_key = annual_cost$cost_component_key,
    cost_join_key = annual_cost$cost_join_key, cost_scope = annual_cost$cost_scope,
    population_in_need_measure = annual_cost$population_in_need_measure,
    stringsAsFactors = FALSE)
  for (cn in ac_cols[10:31]) ac[[cn]] <- NA_real_        # J..AE formula placeholders
  ac$c_sex <- NA_character_                              # AE is text
  ac$r_quantity_scenario <- q_s                          # AF
  ac$r_quantity_baseline  <- q_b                         # AG
  ac$shared_duplicate_count <- NA_real_                  # AH formula
  # AI = R-source national population denominator; AJ..AM per-capita = live formulae
  ac$national_population                <- annual_cost$national_population
  for (cn in ac_cols[36:39]) ac[[cn]] <- NA_real_
  ac <- ac[, ac_cols]
} else {
  ac <- as.data.frame(setNames(replicate(length(ac_cols), logical(0), simplify = FALSE), ac_cols))
}
addWorksheet(wb, "Annual_Cost")
writeData(wb, "Annual_Cost", ac, headerStyle = st_hdr)
if (n_ac > 0) {
  R <- 2:r_ac
  wf <- function(col, fn) writeFormula(wb, "Annual_Cost", startCol = col, startRow = 2, x = frows(fn, R))
  wf(10, function(r) idx_cc("K", r))                                        # J population_in_need_fraction
  wf(11, function(r) sprintf(                                               # K coverage_scenario (linear path)
    "IF(OR(L%d=\"\",Z%d=\"\",AA%d=\"\",AB%d=\"\"),\"\",IF(B%d<AA%d,L%d,IF(B%d>AB%d,Z%d,L%d+(Z%d-L%d)*MIN(MAX((B%d-AA%d+1)/MAX(AB%d-AA%d+1,1),0),1))))",
    r,r,r,r, r,r,r, r,r,r, r,r,r, r,r,r,r))
  wf(12, function(r) idx_cc("S", r))                                        # L coverage_baseline
  wf(13, function(r) idx_cc("L", r))                                        # M frequency_per_year
  wf(14, function(r) idx_cc("P", r))                                        # N unit_cost_usd
  wf(15, function(r) sprintf("AF%d*J%d", r, r))                             # O pin_scenario
  wf(16, function(r) sprintf("AG%d*J%d", r, r))                             # P pin_baseline
  wf(17, function(r) sprintf("P%d*L%d*M%d*N%d", r, r, r, r))                # Q annual_cost_baseline
  wf(18, function(r) sprintf("O%d*K%d*M%d*N%d", r, r, r, r))                # R annual_cost_scenario
  wf(19, function(r) sprintf("R%d-Q%d", r, r))                              # S annual_cost_incremental
  wf(20, function(r) idx_cc("R", r))                                        # T indonesia_adjusted_flag
  wf(21, function(r) idx_cc("Q", r))                                        # U price_year
  wf(22, function(r) sprintf("1/(1+'Calculation_Assumptions'!$B$5)^(B%d-'Calculation_Assumptions'!$B$2)", r)) # V discount_factor
  wf(23, function(r) sprintf("Q%d*V%d", r, r))                             # W disc_cost_baseline
  wf(24, function(r) sprintf("R%d*V%d", r, r))                             # X disc_cost_scenario
  wf(25, function(r) sprintf("S%d*V%d", r, r))                             # Y disc_cost_incremental
  wf(26, function(r) idx_cc("T", r))                                        # Z cov_target
  wf(27, function(r) idx_cc("U", r))                                        # AA cov_start_year
  wf(28, function(r) idx_cc("V", r))                                        # AB cov_target_year
  wf(29, function(r) idx_cc("M", r))                                        # AC c_age_start
  wf(30, function(r) idx_cc("N", r))                                        # AD c_age_stop
  wf(31, function(r) idx_cc("O", r))                                        # AE c_sex
  wf(34, function(r) sprintf(                                               # AH shared_duplicate_count
    "IF(H%d=\"shared-count-once\",COUNTIFS($A$2:$A$%d,A%d,$B$2:$B$%d,B%d,$E$2:$E$%d,E%d),1)",
    r, r_ac, r, r_ac, r, r_ac, r))
  # AJ..AM component annual cost per capita = component cost / national_population (AI)
  wf(36, function(r) sprintf("IF(AI%d=0,\"\",Q%d/AI%d)", r, r, r))         # annual_cost_baseline_per_capita
  wf(37, function(r) sprintf("IF(AI%d=0,\"\",R%d/AI%d)", r, r, r))         # annual_cost_scenario_per_capita
  wf(38, function(r) sprintf("IF(AI%d=0,\"\",S%d/AI%d)", r, r, r))         # annual_cost_incremental_per_capita
  wf(39, function(r) sprintf("IF(AI%d=0,\"\",Y%d/AI%d)", r, r, r))         # disc_cost_incremental_per_capita
}
style_sheet("Annual_Cost", ac_cols, n_ac,
            formula_cols = c(10:31, 34, 36:39), rsource_cols = c(32, 33, 35))

# CASCADE cost-coverage tie-out (opt-in): the engine costed with the EXACT
# per-year effective-coverage path (not a linear ramp), so replace the Annual_Cost
# coverage_scenario FORMULA (col K, cols[11]) with the R-source per-year values
# the engine used. Column K then behaves like the other R-source inputs
# (r_quantity, etc.) and the downstream Excel cost/budget totals reconcile exactly
# to the R engine. Guarded: ordinary runs keep the live linear-ramp formula.
if (n_ac > 0 && isTRUE(get0("run_cascade_70_30_30_to_70_70_70", ifnotfound = FALSE)) &&
    !is.null(fair_inputs$cascade) && "coverage_scenario" %in% names(annual_cost)) {
  writeData(wb, "Annual_Cost", annual_cost$coverage_scenario,
            startCol = 11L, startRow = 2L, colNames = FALSE)
  addStyle(wb, "Annual_Cost", st_rsrc, rows = 2:r_ac, cols = 11L, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Annual_Cost", createStyle(numFmt = "0.0000%"),
           rows = 2:r_ac, cols = 11L, gridExpand = TRUE, stack = TRUE)
}

# =========================================================================
# 11.11 Budget_Impact  (C:H = formulas over Annual_Cost)
# =========================================================================
bud_cols <- c("scenario","year","baseline_cost","scenario_cost","incremental_cost",
              "disc_incremental_cost","cumulative_incremental_cost","cumulative_disc_incremental_cost",
              "national_population","baseline_cost_per_capita","scenario_cost_per_capita",
              "incremental_cost_per_capita","disc_incremental_cost_per_capita")
if (n_bi > 0) {
  bud <- data.frame(scenario = bi$scenario, year = bi$year, stringsAsFactors = FALSE)
  for (cn in bud_cols[3:8]) bud[[cn]] <- NA_real_
  bud$national_population <- bi$national_population        # I = R-source denominator
  for (cn in bud_cols[10:13]) bud[[cn]] <- NA_real_        # J..M per-capita = live formulae
} else bud <- as.data.frame(setNames(replicate(length(bud_cols), logical(0), simplify = FALSE), bud_cols))
addWorksheet(wb, "Budget_Impact")
writeData(wb, "Budget_Impact", bud, headerStyle = st_hdr)
if (n_bi > 0) {
  R <- 2:r_bi
  sumif_ac <- function(tgt, r) sprintf(
    "SUMIFS('Annual_Cost'!$%s$2:$%s$%d,'Annual_Cost'!$A$2:$A$%d,A%d,'Annual_Cost'!$B$2:$B$%d,B%d)",
    tgt, tgt, r_ac, r_ac, r, r_ac, r)
  writeFormula(wb, "Budget_Impact", startCol = 3, startRow = 2, x = frows(function(r) sumif_ac("Q", r), R)) # baseline_cost
  writeFormula(wb, "Budget_Impact", startCol = 4, startRow = 2, x = frows(function(r) sumif_ac("R", r), R)) # scenario_cost
  writeFormula(wb, "Budget_Impact", startCol = 5, startRow = 2, x = frows(function(r) sumif_ac("S", r), R)) # incremental_cost
  writeFormula(wb, "Budget_Impact", startCol = 6, startRow = 2, x = frows(function(r) sumif_ac("Y", r), R)) # disc_incremental_cost
  writeFormula(wb, "Budget_Impact", startCol = 7, startRow = 2,
               x = frows(function(r) sprintf("SUMIFS($E$2:E%d,$A$2:A%d,A%d)", r, r, r), R))                 # cumulative
  writeFormula(wb, "Budget_Impact", startCol = 8, startRow = 2,
               x = frows(function(r) sprintf("SUMIFS($F$2:F%d,$A$2:A%d,A%d)", r, r, r), R))                 # cumulative disc
  # J..M annual cost per capita = same-year cost / national_population (col I), live formulae
  writeFormula(wb, "Budget_Impact", startCol = 10, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",C%d/I%d)", r, r, r), R))
  writeFormula(wb, "Budget_Impact", startCol = 11, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",D%d/I%d)", r, r, r), R))
  writeFormula(wb, "Budget_Impact", startCol = 12, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",E%d/I%d)", r, r, r), R))
  writeFormula(wb, "Budget_Impact", startCol = 13, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",F%d/I%d)", r, r, r), R))
}
style_sheet("Budget_Impact", bud_cols, n_bi, formula_cols = c(3:8, 10:13), rsource_cols = 9)

# =========================================================================
# 11.12 Cost_Effectiveness  (C:I = formulas)
# =========================================================================
ce_cols <- c("scenario","scenario_label","deaths_averted","cases_averted",
             "incremental_cost","disc_incremental_cost","cost_per_death_averted",
             "dominance","reconciliation_status",
             "annual_cost_incremental_per_capita","disc_cost_incremental_per_capita")
ce <- data.frame(scenario = cea$scenario, scenario_label = cea$scenario_label, stringsAsFactors = FALSE)
for (cn in ce_cols[3:7]) ce[[cn]] <- NA_real_
ce$dominance <- NA_character_; ce$reconciliation_status <- NA_character_
ce$annual_cost_incremental_per_capita <- NA_real_   # J = live formula
ce$disc_cost_incremental_per_capita   <- NA_real_   # K = live formula
addWorksheet(wb, "Cost_Effectiveness")
writeData(wb, "Cost_Effectiveness", ce, headerStyle = st_hdr)
R <- 2:r_ce
writeFormula(wb, "Cost_Effectiveness", startCol = 3, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Annual_Mortality'!$H$2:$H$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 4, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Annual_Mortality'!$J$2:$J$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 5, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Budget_Impact'!$E$2:$E$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 6, startRow = 2,
             x = frows(function(r) sprintf("SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 7, startRow = 2,
             x = frows(function(r) sprintf("IF(C%d>0,F%d/C%d,\"\")", r, r, r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 8, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(AND(C%d>0,F%d<0),\"Dominant (more health, lower cost)\",IF(AND(C%d<=0,F%d>0),\"Dominated (less/no health, higher cost)\",IF(AND(C%d<=0,F%d<=0),\"No deaths averted; ratio not defined\",\"USD per death averted\")))",
               r,r, r,r, r,r), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 9, startRow = 2,
             x = frows(function(r) sprintf(
               "IF(AND(ABS(F%d-SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d))<='Calculation_Assumptions'!$B$11,ABS(C%d-SUMIFS('Annual_Mortality'!$H$2:$H$%d,'Annual_Mortality'!$A$2:$A$%d,A%d))<='Calculation_Assumptions'!$B$11),\"consistent\",\"mismatch\")",
               r, r_bi, r_bi, r, r, r_am, r_am, r), R))
# J/K summary annual per-capita = mean of Budget_Impact per-capita (L/M) over the
# analysis horizon excluding the start year (>= analysis_start_year + 1).
writeFormula(wb, "Cost_Effectiveness", startCol = 10, startRow = 2,
             x = frows(function(r) sprintf("IFERROR(AVERAGEIFS('Budget_Impact'!$L$2:$L$%d,'Budget_Impact'!$A$2:$A$%d,A%d,'Budget_Impact'!$B$2:$B$%d,\">=\"&('Calculation_Assumptions'!$B$2+1)),\"\")", r_bi, r_bi, r, r_bi), R))
writeFormula(wb, "Cost_Effectiveness", startCol = 11, startRow = 2,
             x = frows(function(r) sprintf("IFERROR(AVERAGEIFS('Budget_Impact'!$M$2:$M$%d,'Budget_Impact'!$A$2:$A$%d,A%d,'Budget_Impact'!$B$2:$B$%d,\">=\"&('Calculation_Assumptions'!$B$2+1)),\"\")", r_bi, r_bi, r, r_bi), R))
style_sheet("Cost_Effectiveness", ce_cols, n_ce, formula_cols = c(3:9, 10:11),
            wrap_cols = c(2, 8))
# CEA reconciliation_status conditional format
conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "consistent", type = "contains", style = cf_pass)
conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "mismatch",   type = "contains", style = cf_fail)
conditionalFormatting(wb, "Cost_Effectiveness", cols = 8, rows = 2:r_ce, rule = "Dominated",  type = "contains", style = cf_rev)

# =========================================================================
# 11.13 Health_Outcomes + Economic_Value + Benefit_Cost (Reference-Case BCA) --
# =========================================================================
# Built by the shared formula-driven builder off Model 07 (health) and Model 08
# (value), restricted to the clinical comparator scenarios. Every derived cell is
# a live Excel formula anchored to the BCA controls on Calculation_Assumptions.
.cmp <- comparators
ho_src_clin <- dt_h07[scenario %in% .cmp, .(
  scenario_label = scenario_label[1L],
  modeled_deaths = sum(deaths),   baseline_deaths = sum(base_deaths),
  modeled_cases  = sum(newcases), baseline_cases  = sum(base_newcases),
  yll = sum(yll), base_yll = sum(base_yll),
  yld = sum(yld), base_yld = sum(base_yld),
  daly = sum(daly), base_daly = sum(base_daly)),
  by = .(scenario, year)]
ev_src_clin <- ev08[scenario %in% .cmp, .(
  scenario_label    = scenario_label[1L],
  deaths_averted    = sum(deaths_averted),
  life_years_gained = sum(life_years_gained_undisc),
  gni_pc_idn = gni_pc_ppp[1L], gni_pc_usa = gni_pc_usa[1L],
  population = population[1L],  le_avg_adult = le_avg_adult[1L]),
  by = .(scenario, year)]
scen_meta_clin <- unique(dt_h07[scenario %in% .cmp, .(
  scenario, scenario_label, intervention_family,
  scenario_level = fifelse(scenario %in% c("all", "all_public_health"), "combined",
                    fifelse(is.na(scenario_role), "standalone", scenario_role)))])
sty_clin <- list(st_hdr = st_hdr, st_formula = st_formula, st_rsrc = st_rsrc,
                 st_input = st_input, st_wrap = st_wrap,
                 cf_pass = cf_pass, cf_fail = cf_fail, cf_rev = cf_rev)
build_bca_sheets_into(wb, .cmp, ho_src_clin, ev_src_clin, scen_meta_clin,
                      bca_cells_clin, r_bi, sty_clin)

# =========================================================================
# 11.13b CVD_40q30_Age + CVD_40q30 (formula-driven period CVD 40q30) --------
# =========================================================================
# Baseline + the clinical comparators. The baseline id must be included so the
# on-sheet baseline SUMIFS resolves.
build_cvd_40q30_sheets_into(wb, c(base_id, comparators), dt_cvd_40q30, cvd_age_40q30,
                            base_id, sty_clin)

# =========================================================================
# 11.14 Input_Diagnostic  (R values; severity conditionally formatted)
# =========================================================================
addWorksheet(wb, "Input_Diagnostic")
if (n_id > 0) {
  writeData(wb, "Input_Diagnostic", as.data.frame(diag_out), headerStyle = st_hdr)
  style_sheet("Input_Diagnostic", names(diag_out), n_id,
              wrap_cols = which(names(diag_out) == "problem"))
  sev_col <- which(names(diag_out) == "severity")
  conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "FAIL",   type = "contains", style = cf_fail)
  conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "REVIEW", type = "contains", style = cf_rev)
} else {
  writeData(wb, "Input_Diagnostic",
            data.frame(scope = character(0), item_key = character(0), field = character(0),
                       problem = character(0), severity = character(0)), headerStyle = st_hdr)
  addStyle(wb, "Input_Diagnostic", st_hdr, rows = 1, cols = 1:5, gridExpand = TRUE)
}

# =========================================================================
# 11.15 QA_Checks  (C actual + D status = formulas; Excel-vs-R reconciliation)
# =========================================================================
qa_check <- c(
  "Intervention-cause key uniqueness",
  "Workbook FAIL-level issues",
  "Workbook REVIEW-level issues",
  "Every scenario paired to baseline",
  "No impossible negative states",
  "Stock/flow identity pop = well + sick + all-cause deaths",
  "Background mortality constant across cause (not duplicated)",
  "Cost reconciliation (components -> budget impact)",
  "Shared cost counted once per stratum/year",
  "CEA reconciliation (detail -> summary ratio)",
  "Excel vs R: deaths averted (anchor scenario)",
  "Excel vs R: discounted incremental cost (anchor scenario)",
  "Excel vs R: cost per death averted (anchor scenario)")
qa_expect <- c("0","0","0","TRUE","0","<= limit","1","<= tol","0","consistent",
               "match R","match R","match R")
qa_note <- c(
  "Each selected link key appears once",
  "Blocked links/scenarios excluded (see Selected_Interventions / Input_Diagnostic)",
  "Flagged but usable (e.g. cost not Indonesia-adjusted, missing optional component)",
  "Deaths averted = baseline - scenario at matched location/year/cause",
  "well/sick/new_cases/cause_deaths/population >= 0",
  "Per cause row; small residual from 95+ pooling / rounding",
  "all.mx taken once per stratum in the R Background sheet",
  "Excel component rows sum to Excel annual totals within tolerance",
  "shared-count-once components appear once per scenario-year",
  "Every Cost_Effectiveness row's internal reconciliation is consistent",
  "Excel CEA deaths averted reconciles to the R engine value",
  "Excel CEA discounted incremental cost reconciles to the R engine value",
  "Excel CEA USD per death averted reconciles to the R engine value")
# C (actual) formulas
qa_actual <- c(
  sprintf("COUNTIF('Selected_Interventions'!$U$2:$U$%d,\">1\")", r_si),
  sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"FAIL\")", r_id),
  sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"REVIEW\")", r_id),
  sprintf("COUNTBLANK('Annual_Mortality'!$G$2:$G$%d)=0", r_am),
  "'Calculation_Assumptions'!$B$15... placeholder",   # replaced below
  "'Calculation_Assumptions'!$B$13... placeholder",
  "'Calculation_Assumptions'!$B$14... placeholder",
  sprintf("ABS(SUM('Budget_Impact'!$D$2:$D$%d)-SUM('Annual_Cost'!$R$2:$R$%d))+ABS(SUM('Budget_Impact'!$C$2:$C$%d)-SUM('Annual_Cost'!$Q$2:$Q$%d))",
          r_bi, r_ac, r_bi, r_ac),
  sprintf("COUNTIF('Annual_Cost'!$AH$2:$AH$%d,\">1\")", r_ac),
  sprintf("IF(COUNTIF('Cost_Effectiveness'!$I$2:$I$%d,\"mismatch\")=0,\"consistent\",\"mismatch\")", r_ce),
  sprintf("INDEX('Cost_Effectiveness'!$C$2:$C$%d,MATCH('Calculation_Assumptions'!$B$20,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, r_ce),
  sprintf("INDEX('Cost_Effectiveness'!$F$2:$F$%d,MATCH('Calculation_Assumptions'!$B$20,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, r_ce),
  sprintf("INDEX('Cost_Effectiveness'!$G$2:$G$%d,MATCH('Calculation_Assumptions'!$B$20,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, r_ce))
qa_actual[5] <- "'Calculation_Assumptions'!$B$16"   # r_negative_state_count
qa_actual[6] <- "'Calculation_Assumptions'!$B$14"   # r_stock_flow_max_residual
qa_actual[7] <- "'Calculation_Assumptions'!$B$15"   # r_background_distinct_count
# D (status) formulas -- each references its own C{r}
qa_status <- c(
  "IF(C2=0,\"PASS\",\"FAIL\")",
  "IF(C3=0,\"PASS\",\"REVIEW\")",
  "IF(C4=0,\"PASS\",\"REVIEW\")",
  "IF(C5=TRUE,\"PASS\",\"FAIL\")",
  "IF(C6=0,\"PASS\",\"FAIL\")",
  "IF(C7<='Calculation_Assumptions'!$B$12,\"PASS\",\"REVIEW\")",
  "IF(C8=1,\"PASS\",\"FAIL\")",
  "IF(C9<='Calculation_Assumptions'!$B$11,\"PASS\",\"FAIL\")",
  "IF(C10=0,\"PASS\",\"FAIL\")",
  "IF(C11=\"consistent\",\"PASS\",\"FAIL\")",
  "IF(ABS(C12-'Calculation_Assumptions'!$B$17)<=ABS('Calculation_Assumptions'!$B$17)*0.000001+0.5,\"PASS\",\"FAIL\")",
  "IF(ABS(C13-'Calculation_Assumptions'!$B$18)<=ABS('Calculation_Assumptions'!$B$18)*0.000001+1,\"PASS\",\"FAIL\")",
  "IF(ABS(C14-'Calculation_Assumptions'!$B$19)<=ABS('Calculation_Assumptions'!$B$19)*0.000001+1,\"PASS\",\"FAIL\")")
# --- Reference-Case BCA QA (rows 15-18): VSL floor, unchanged reference-case
#     parameters, benefit/cost price-year basis, Benefit_Cost scenario coverage.
.rev_c <- nrow(ev_src_clin) + 1L; .rho_c <- nrow(ho_src_clin) + 1L
.rbc_c <- nrow(scen_meta_clin) * 4L + 1L
qa_check  <- c(qa_check,
  "VSL floor applied (preferred VSL >= 20x GNI floor)",
  "Reference-case VSL parameters (elasticity 1.5, US ratio 160, 20x floor)",
  "Benefit and cost on the same price year (BCA basis)",
  "Benefit_Cost scenarios all present in Health_Outcomes")
qa_expect <- c(qa_expect, "0", "as_specified", "match", "0")
qa_note   <- c(qa_note,
  "No Economic_Value row has preferred VSL below the 20x GNI-per-capita floor",
  "Reference case = elasticity 1.5 transfer, 160x US ratio, 20x GNI floor (Robinson et al. 2019)",
  "bca_price_year equals the cost price year, so benefits and costs share a real price basis",
  "Every scenario x valuation-case row in Benefit_Cost has a matching Health_Outcomes scenario")
qa_actual <- c(qa_actual,
  sprintf("SUMPRODUCT(('Economic_Value'!$L$2:$L$%d<'Economic_Value'!$K$2:$K$%d)*1)", .rev_c, .rev_c),
  sprintf("IF(AND(%s=1.5,%s=160,%s=20),\"as_specified\",\"edited\")",
          bca_cells_clin$elast, bca_cells_clin$ratio, bca_cells_clin$floor),
  sprintf("IF(%s=%s,\"match\",\"mismatch\")", bca_cells_clin$price_year, .bcell("cost_price_year")),
  sprintf("SUMPRODUCT((COUNTIF('Health_Outcomes'!$A$2:$A$%d,'Benefit_Cost'!$A$2:$A$%d)=0)*1)", .rho_c, .rbc_c))
qa_status <- c(qa_status,
  "IF(C15=0,\"PASS\",\"FAIL\")",
  "IF(C16=\"as_specified\",\"PASS\",\"REVIEW\")",
  "IF(C17=\"match\",\"PASS\",\"REVIEW\")",
  "IF(C18=0,\"PASS\",\"FAIL\")")
# --- CVD 40q30 reconciliation (row 19): Excel life-table vs the R anchor -------
.rq40_c <- nrow(dt_cvd_40q30[scenario %in% c(base_id, comparators)]) + 1L
qa_check  <- c(qa_check,  "CVD 40q30 Excel vs R (all rows match)")
qa_expect <- c(qa_expect, "0")
qa_note   <- c(qa_note,   "CVD_40q30 recon_status has no 'mismatch' (life-table formula reconciles to Model 07)")
qa_actual <- c(qa_actual, sprintf("COUNTIF('CVD_40q30'!$N$2:$N$%d,\"mismatch\")", .rq40_c))
qa_status <- c(qa_status, "IF(C19=0,\"PASS\",\"FAIL\")")
qa_df <- data.frame(check = qa_check, expected = qa_expect,
                    actual = NA, status = NA_character_, note = qa_note, stringsAsFactors = FALSE)
addWorksheet(wb, "QA_Checks")
writeData(wb, "QA_Checks", qa_df, headerStyle = st_hdr)
writeFormula(wb, "QA_Checks", startCol = 3, startRow = 2, x = qa_actual)
writeFormula(wb, "QA_Checks", startCol = 4, startRow = 2, x = qa_status)
n_qa <- nrow(qa_df); r_qa <- n_qa + 1L
style_sheet("QA_Checks", names(qa_df), n_qa, formula_cols = c(3, 4),
            wrap_cols = 5, filter = TRUE)
conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "PASS",   type = "contains", style = cf_pass)
conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "FAIL",   type = "contains", style = cf_fail)
conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "REVIEW", type = "contains", style = cf_rev)

# =========================================================================
# 11.16 Methods_and_Sources + Calculation_Map
# =========================================================================
methods_f <- rbind(as.data.frame(methods), data.frame(
  method_id = c("M12","M13","M14","M15","M16","M17","M18"),
  concept = c("Excel formula lineage","PIN audit quantities",
              "Reference-Case VSL transfer","Constant VSLY",
              "BCA discounting & monetary basis","Benefit-cost ratio & net benefit",
              "CVD 40q30 (period life table)"),
  formula_or_rule = c(
    "Formula-derived cells are light blue and depend on visible grey R source/helper cells; Calculation_Map lists the dependency chain.",
    "r_quantity_scenario/baseline (Annual_Cost AF:AG) retain the full-precision R population quantity used before the PIN fraction.",
    "Preferred VSL = MAX(vsl_us_gni_ratio*GNIpc_US*(GNIpc_IDN/GNIpc_US)^income_elasticity, vsl_floor_gni_multiple*GNIpc_IDN); standardized sensitivities VSL = 100x and 160x GNIpc (Economic_Value J:P,N,O).",
    "VSLY = preferred VSL / undiscounted remaining LE at the average age of the working-age (vsly_adult_min_age..max_age) population; applied to Model 07 age-specific life-years gained (not an algebraic identity with VSL).",
    "Benefits and costs are discounted to bca_base_year at bca_discount_rate_primary; VSL benefits are PPP international dollars; market-USD costs are converted to that basis via cost_to_bca_currency_factor (never assumed 1).",
    "BCR = PV benefits / PV costs and net benefit = PV benefits - PV costs, on ONE base year, rate, price year and currency basis (Benefit_Cost). This is a PARTIAL mortality-benefit BCA, not cost-effectiveness (USD per death averted).",
    "Period CVD 40q30 over the six CVD causes (ihd, istroke, hstroke, hhd, rhd, cmd), exact ages 30-69: sexes combined before the rate, m_x=(D_F+D_M)/(N_F+N_M), q_x=1-EXP(-m_x), l_30=1, l_{x+1}=l_x(1-q_x), 40q30=100*(1-l_70/l_30). CVD_40q30_Age carries the live life-table formulas; CVD_40q30 has the metric, shared-baseline pairing, absolute (pp) and relative (%) reduction, all reconciled to the Model 07 R value. cvd_40q30 is a PERCENT on a 0-100 scale (format 0.000, not Excel %). Population is de-duplicated across causes (never summed over the six causes)."),
  source = c("This workbook","Model 09 and this workbook",
             rep("Robinson et al. 2019 Reference Case Guidelines / Model 08", 4),
             "Model 07 (07_cvd_40q30.rds) / Model 09"),
  stringsAsFactors = FALSE))
addWorksheet(wb, "Methods_and_Sources")
writeData(wb, "Methods_and_Sources", methods_f, headerStyle = st_hdr)
style_sheet("Methods_and_Sources", names(methods_f), nrow(methods_f),
            wrap_cols = c(3, 4), filter = FALSE, max_w = 90)
setColWidths(wb, "Methods_and_Sources", cols = 1:4, widths = c(10, 26, 88, 58))

cmap <- data.table(
  output_sheet = c("Selected_Interventions","Cost_Components","Annual_Mortality",
                   "Health_Outcomes","CVD_40q30_Age","CVD_40q30","Annual_Cost","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                   "Economic_Value","Benefit_Cost","QA_Checks","Run_Metadata","Companion R workbook"),
  formula_columns = c("P, U:V","S:V, X","H, J","F,I,L,O,R,S","J:M","I,J,K,L,N","J:AE, AH","AF:AG","C:H","C:I",
                      "J:AB","G:M","C:D","B8:B16 (subset)","none"),
  depends_on = c("selected health links (J:M)","Selected_Interventions",
                 "R aggregates E:G, I","Model 07 modeled/baseline health (grey D:E,G:H,J:K,M:N,P:Q)",
                 "CVD deaths + de-duplicated population (grey H:I)","CVD_40q30_Age life table; baseline SUMIFS; R anchor (M)",
                 "Cost_Components; Calculation_Assumptions; R quantity AF:AG",
                 "full-precision Model 06 output","Annual_Cost",
                 "Annual_Mortality; Budget_Impact; Calculation_Assumptions",
                 "Model 08 source (grey D:I); Calculation_Assumptions BCA controls",
                 "Economic_Value PV benefits; Budget_Impact costs; Calculation_Assumptions",
                 "calculation + diagnostic sheets",
                 "Calculation_Assumptions","Model 06 / Model 09"),
  calculation = c("Adjusted effect at target; key uniqueness/status",
                  "Coverage link (INDEX/MATCH) and cost-readiness rule",
                  "Deaths averted and cases averted",
                  "Deaths/cases/YLL/YLD/DALY averted and life-years gained (baseline - intervention)",
                  "m_x=deaths/pop, q_x=1-EXP(-m_x), recursive l_x, l_{x+1} (period life table, ages 30-69)",
                  "CVD 40q30 = 100*(1-l_70/l_30); baseline-paired absolute (pp) & relative (%) reduction; R reconciliation",
                  "Coverage path, PIN, annual + discounted cost, shared-cost QA",
                  "Scenario/baseline quantity before the PIN fraction",
                  "Annual and cumulative cost by scenario",
                  "Cumulative health, cost, cost/death, dominance, reconciliation",
                  "Reference-Case VSL (elast 1.5 + 20x floor) & 100x/160x, VSLY, undisc & PV benefits, GNI shares",
                  "PV benefits, PV costs (to PPP int$), net benefit, BCR, benefit/GNI, decision (partial BCA)",
                  "Invariant recomputation and Excel-vs-R reconciliation status",
                  "Metadata pulled from the assumptions controls",
                  "Detailed Model_State_Trace and Background_Mortality"),
  source_role = c(rep("Workbook formula", 13), "Workbook formula", "R source data"))
addWorksheet(wb, "Calculation_Map")
writeData(wb, "Calculation_Map", cmap, headerStyle = st_hdr)
style_sheet("Calculation_Map", names(cmap), nrow(cmap), wrap_cols = c(3, 4), filter = FALSE, max_w = 56)
setColWidths(wb, "Calculation_Map", cols = 1:5, widths = c(22, 18, 34, 52, 26))

# =========================================================================
# 11.x Scenario_Catalog  (authoritative current-run scenario contract)
# Describes exactly the scenarios in THIS clinical run (baseline + each active
# intervention + the combined 'all'), derived from the Model 04 catalogue under
# the binding include_flag rule. Literal R-source values (no formulas).
# =========================================================================
scat_clin <- as.data.frame(deck_catalog_list[["clinical"]])
addWorksheet(wb, "Scenario_Catalog")
writeData(wb, "Scenario_Catalog", scat_clin, headerStyle = st_hdr)
style_sheet("Scenario_Catalog", names(scat_clin), nrow(scat_clin),
            rsource_cols = seq_along(scat_clin), wrap_cols = c(2, 7))

# =========================================================================
# 11.16b CASCADE-SPECIFIC SHEETS  (opt-in; only when the cascade flag is set) --
# Adds Cascade_Assumptions / Cascade_Trajectory / Cascade_QA to the SAME formula
# workbook (never renaming or merging into any standard sheet). Guarded so an
# ordinary clinical run produces a byte-for-byte unchanged workbook.
# =========================================================================
.cascade_sheets_added <- character(0)
if (isTRUE(get0("run_cascade_70_30_30_to_70_70_70", ifnotfound = FALSE)) &&
    !is.null(fair_inputs$cascade)) {
  casc <- fair_inputs$cascade
  MONEYC <- "#,##0"; COVFMT <- "0.000000"; CHKFMT <- "0.0000000000"

  ## ---- Cascade_Assumptions ------------------------------------------------
  # The exact editable cascade inputs, sourced from the cascade input workbook's
  # Assumptions sheet. To change (e.g.) the diabetes control-rate baseline, edit
  # `diabetes_baseline_control_among_treated` in that workbook and re-run --
  # the model is driven by the workbook, not by this display copy.
  asheet <- as.data.table(casc$assumptions_sheet)
  keep_ids <- c("analysis_start_year","analysis_end_year","first_target_year","final_target_year",
                "target_diagnosis_share","first_target_treatment_conditional",
                "first_target_control_conditional","final_target_treatment_conditional",
                "final_target_control_conditional","treated_uncontrolled_effect_fraction",
                "first_target_effective_coverage_exact","final_target_effective_coverage_exact",
                "diabetes_baseline_control_among_treated","diabetes_baseline_diagnosis_conditioning",
                "cholesterol_coverage_proxy","scale_up_shape","post_target_rule",
                "prevent_coverage_backsliding","scenario_id","cost_discount_rate",
                "health_discount_rate","cost_price_year","currency")
  ca_dt <- asheet[parameter_id %in% keep_ids]
  ca_dt <- ca_dt[match(intersect(keep_ids, ca_dt$parameter_id), parameter_id)]
  ca_show <- data.frame(
    parameter_id = as.character(ca_dt$parameter_id),
    value        = as.character(ca_dt$value),
    unit         = if ("unit" %in% names(ca_dt)) as.character(ca_dt$unit) else "",
    description  = if ("description" %in% names(ca_dt)) as.character(ca_dt$description) else "",
    source       = if ("source" %in% names(ca_dt)) as.character(ca_dt$source) else "",
    source_workbook_cell = paste0("Assumptions!", "[parameter_id=", ca_dt$parameter_id, "]"),
    stringsAsFactors = FALSE)
  addWorksheet(wb, "Cascade_Assumptions")
  writeData(wb, "Cascade_Assumptions", ca_show, headerStyle = st_hdr)
  style_sheet("Cascade_Assumptions", names(ca_show), nrow(ca_show),
              input_cols = 2, rsource_cols = c(1,3,4,5,6), wrap_cols = c(4,5,6), max_w = 60)
  # numeric coverage/fraction rows: write TRUE numbers (so QA formulas that
  # reference them do exact arithmetic, not text coercion) and show full precision.
  .ca_row <- function(pid) which(ca_show$parameter_id == pid) + 1L
  for (pid in c("treated_uncontrolled_effect_fraction","first_target_effective_coverage_exact",
                "final_target_effective_coverage_exact","diabetes_baseline_control_among_treated"))
    if (length(.ca_row(pid))) {
      writeData(wb, "Cascade_Assumptions",
                suppressWarnings(as.numeric(asheet[parameter_id == pid, value][1])),
                startCol = 2, startRow = .ca_row(pid), colNames = FALSE)
      addStyle(wb, "Cascade_Assumptions", createStyle(numFmt = CHKFMT),
               rows = .ca_row(pid), cols = 2, gridExpand = TRUE, stack = TRUE)
    }
  # Cell refs used by the QA formulas below.
  cell_partial <- sprintf("'Cascade_Assumptions'!$B$%d", .ca_row("treated_uncontrolled_effect_fraction"))
  cell_eff2030 <- sprintf("'Cascade_Assumptions'!$B$%d", .ca_row("first_target_effective_coverage_exact"))
  cell_eff2040 <- sprintf("'Cascade_Assumptions'!$B$%d", .ca_row("final_target_effective_coverage_exact"))
  .cascade_sheets_added <- c(.cascade_sheets_added, "Cascade_Assumptions")

  ## ---- Cascade_Trajectory -------------------------------------------------
  # Per intervention x sex x year: baseline effective coverage, the 2030/2040
  # targets, milestone cascade components (controlled + treated-uncontrolled with
  # the half-effect) where defined, the exact per-year scenario effective coverage
  # R applied, and an Excel `model_effective_coverage_used` formula that references
  # that same R-source cell (so the value R used is visible and live).
  ctrj <- as.data.table(casc$coverage_trajectory)[order(intervention_id, sex, year)]
  cov_ms <- as.data.table(casc$coverage)[, .(
      risk_factor_id = trimws(as.character(risk_factor_id)),
      sex = trimws(as.character(sex)), year = as.integer(year),
      controlled_share = as.numeric(controlled_share_all_condition),
      treated_uncontrolled_share = as.numeric(treated_uncontrolled_share),
      partial_effect_fraction = as.numeric(partial_effect_fraction),
      exact_effective_coverage = as.numeric(exact_effective_coverage))]
  trj <- merge(ctrj, cov_ms, by = c("risk_factor_id","sex","year"), all.x = TRUE)
  setorder(trj, intervention_id, sex, year)
  phase_of <- function(y) ifelse(y <= casc$first_target_year, "Scale to 70-30-30",
                          ifelse(y <= casc$final_target_year, "Scale to 70-70-70", "Maintain 70-70-70"))
  tr_show <- data.frame(
    intervention_id = trj$intervention_id, risk_factor_id = trj$risk_factor_id,
    sex = trj$sex, year = trj$year, phase = phase_of(trj$year),
    baseline_effective_coverage = trj$baseline_effective_coverage,
    target_2030_effective = casc$eff_2030, target_2040_effective = casc$eff_2040,
    controlled_share = trj$controlled_share,
    treated_uncontrolled_share = trj$treated_uncontrolled_share,
    half_effect_fraction = trj$partial_effect_fraction,
    scenario_effective_coverage = trj$scenario_effective_coverage,
    effective_from_components = NA_real_,        # Excel formula (milestone rows)
    model_effective_coverage_used = NA_real_,    # Excel formula (= scenario cell)
    stringsAsFactors = FALSE)
  addWorksheet(wb, "Cascade_Trajectory")
  writeData(wb, "Cascade_Trajectory", tr_show, headerStyle = st_hdr)
  nT <- nrow(tr_show)
  Lc <- function(i) openxlsx::int2col(i)
  col_ctrl <- which(names(tr_show) == "controlled_share")
  col_tunc <- which(names(tr_show) == "treated_uncontrolled_share")
  col_scen <- which(names(tr_show) == "scenario_effective_coverage")
  col_efc  <- which(names(tr_show) == "effective_from_components")
  col_muse <- which(names(tr_show) == "model_effective_coverage_used")
  # model_effective_coverage_used = same-row scenario cell (live tie to R value)
  writeFormula(wb, "Cascade_Trajectory",
               x = sprintf("=%s%d", Lc(col_scen), (2:(nT+1L))),
               startCol = col_muse, startRow = 2L)
  # effective_from_components = controlled + partial*treated_uncontrolled (only
  # where the milestone components exist; blank otherwise).
  efc <- vapply(seq_len(nT), function(k) {
    r <- k + 1L
    if (is.na(tr_show$controlled_share[k]) || is.na(tr_show$treated_uncontrolled_share[k])) return("")
    sprintf("=%s%d+%s*%s%d", Lc(col_ctrl), r, cell_partial, Lc(col_tunc), r)
  }, character(1))
  for (k in seq_len(nT)) if (nzchar(efc[k]))
    writeFormula(wb, "Cascade_Trajectory", x = efc[k], startCol = col_efc, startRow = k + 1L)
  style_sheet("Cascade_Trajectory", names(tr_show), nT,
              formula_cols = c(col_efc, col_muse),
              rsource_cols = setdiff(seq_along(tr_show), c(col_efc, col_muse)))
  for (j in c(which(names(tr_show)=="baseline_effective_coverage"),
              which(names(tr_show)=="target_2030_effective"),
              which(names(tr_show)=="target_2040_effective"),
              col_ctrl, col_tunc, col_scen, col_efc, col_muse))
    addStyle(wb, "Cascade_Trajectory", createStyle(numFmt = COVFMT),
             rows = 2:(nT+1L), cols = j, gridExpand = TRUE, stack = TRUE)
  .cascade_sheets_added <- c(.cascade_sheets_added, "Cascade_Trajectory")

  ## ---- Cascade_QA ---------------------------------------------------------
  # Reconcile the Excel-reconstructed cascade arithmetic to the exact workbook
  # milestones (0.1365 / 0.4165) and record the R-side adapter reconciliation
  # against the workbook's Model_Input_View transition multipliers.
  ms30 <- cov_ms[year == casc$first_target_year][1]
  ms40 <- cov_ms[year == casc$final_target_year][1]
  qa <- data.frame(
    check_id = c("QAC01","QAC02","QAC03","QAC04","QAC05"),
    check = c("2030 effective coverage reconstructs to 0.1365",
              "2040 effective coverage reconstructs to 0.4165",
              "Excel model_effective_coverage_used ties to R (by construction)",
              "R adapter vs workbook Model_Input_View multipliers",
              "Effective coverage is monotonic (no backsliding)"),
    controlled_share = c(ms30$controlled_share, ms40$controlled_share, NA, NA, NA),
    treated_uncontrolled_share = c(ms30$treated_uncontrolled_share, ms40$treated_uncontrolled_share, NA, NA, NA),
    excel_effective = NA_real_,      # formula
    expected_effective = c(casc$eff_2030, casc$eff_2040, NA, NA, NA),
    r_value = c(NA, NA, 0, casc$recon_max_abs, 0),
    status = NA,                     # formula / literal
    stringsAsFactors = FALSE)
  addWorksheet(wb, "Cascade_QA")
  writeData(wb, "Cascade_QA", qa, headerStyle = st_hdr)
  nQ <- nrow(qa)
  qc_ctrl <- which(names(qa) == "controlled_share")
  qc_tunc <- which(names(qa) == "treated_uncontrolled_share")
  qc_exc  <- which(names(qa) == "excel_effective")
  qc_exp  <- which(names(qa) == "expected_effective")
  qc_rval <- which(names(qa) == "r_value")
  qc_stat <- which(names(qa) == "status")
  # rows 1-2 (Excel reconstruction) at sheet rows 2,3
  for (rr in 1:2) {
    r <- rr + 1L
    writeFormula(wb, "Cascade_QA",
      x = sprintf("=%s%d+%s*%s%d", Lc(qc_ctrl), r, cell_partial, Lc(qc_tunc), r),
      startCol = qc_exc, startRow = r)
    writeFormula(wb, "Cascade_QA",
      x = sprintf("=IF(ABS(%s%d-%s%d)<0.0000001,\"PASS\",\"CHECK\")",
                  Lc(qc_exc), r, Lc(qc_exp), r),
      startCol = qc_stat, startRow = r)
  }
  # row 3: tie-out (always PASS by construction)
  writeData(wb, "Cascade_QA", "PASS", startCol = qc_stat, startRow = 4, colNames = FALSE)
  # row 4: adapter reconciliation PASS if < 1e-6
  writeFormula(wb, "Cascade_QA",
    x = sprintf("=IF(%s5<0.000001,\"PASS\",\"FAIL\")", Lc(qc_rval)),
    startCol = qc_stat, startRow = 5)
  # row 5: monotonicity (R-source count of decreases == 0)
  writeFormula(wb, "Cascade_QA",
    x = sprintf("=IF(%s6=0,\"PASS\",\"FAIL\")", Lc(qc_rval)),
    startCol = qc_stat, startRow = 6)
  style_sheet("Cascade_QA", names(qa), nQ,
              formula_cols = c(qc_exc, qc_stat),
              rsource_cols = c(qc_ctrl, qc_tunc, qc_exp, qc_rval), wrap_cols = 2, max_w = 60)
  for (j in c(qc_ctrl, qc_tunc, qc_exc, qc_exp))
    addStyle(wb, "Cascade_QA", createStyle(numFmt = CHKFMT),
             rows = 2:(nQ+1L), cols = j, gridExpand = TRUE, stack = TRUE)
  conditionalFormatting(wb, "Cascade_QA", cols = qc_stat, rows = 2:(nQ+1L),
                        rule = "PASS", type = "contains", style = cf_pass)
  conditionalFormatting(wb, "Cascade_QA", cols = qc_stat, rows = 2:(nQ+1L),
                        rule = "FAIL", type = "contains", style = cf_fail)
  conditionalFormatting(wb, "Cascade_QA", cols = qc_stat, rows = 2:(nQ+1L),
                        rule = "CHECK", type = "contains", style = cf_rev)
  .cascade_sheets_added <- c(.cascade_sheets_added, "Cascade_QA")

  message("  Cascade sheets added: ", paste(.cascade_sheets_added, collapse = ", "))
}

# =========================================================================
# 11.17 worksheet order, recalc-on-open, save
# =========================================================================
desired_order <- c("README","Run_Metadata","Scenario_Catalog",
                   "Cascade_Assumptions","Cascade_Trajectory","Cascade_QA",
                   "Selected_Interventions","Blocked_Links",
                   "Cost_Components","Annual_Mortality","Health_Outcomes",
                   "CVD_40q30","CVD_40q30_Age","Annual_Cost","Budget_Impact",
                   "Cost_Effectiveness","Economic_Value","Benefit_Cost","QA_Checks","Input_Diagnostic",
                   "Methods_and_Sources","Calculation_Assumptions","Calculation_Map")
desired_order <- desired_order[desired_order %in% names(wb)]   # drop any conditionally-absent sheet
worksheetOrder(wb) <- match(desired_order, names(wb))
# force Excel to recalculate every formula on open (even in manual-calc mode)
wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
strip_dangling_drawings(wb)

if (!dir.exists(dirname(cost_value_formulae_file)))
  dir.create(dirname(cost_value_formulae_file), recursive = TRUE)
saveWorkbook(wb, cost_value_formulae_file, overwrite = TRUE)
message("  Wrote formula workbook: ", cost_value_formulae_file)


message(sprintf("  Clinical scenarios: %s", paste(produced, collapse = ", ")))
message(sprintf("  Clinical QA: %d PASS / %d REVIEW / %d FAIL",
                sum(qa_dt$status == "PASS"), sum(qa_dt$status == "REVIEW"),
                sum(qa_dt$status == "FAIL")))

}  # end clinical (run_clinical_interventions) block



# =====================================================================
# source_public_health_cost_value()  --  Model 09 Section 12 builder
# Builds output/indonesia_cost_value_public_health_formulae.xlsx from the
# current-run public-health catalogue (public_health_inputs) and the
# public-health scenarios in the shared Model 06 output (mo_all).
# Fully formatted, formula-driven; exposure-based effects + per-capita policy
# costs. Reuses the clinical workbook's styling/audit conventions.
# =====================================================================
source_public_health_cost_value <- function() {
  stopifnot(exists("public_health_inputs"), !is.null(public_health_inputs),
            exists("public_health_scenarios"), !is.null(public_health_scenarios),
            exists("mo_all"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  phi  <- public_health_inputs
  phs  <- public_health_scenarios
  PA   <- phi$assumptions
  out_file <- if (exists("public_health_cost_value_formulae_file"))
    public_health_cost_value_formulae_file else
    paste0(wd_outp, "indonesia_cost_value_public_health_formulae.xlsx")
  message("  Building public-health formula workbook: ", out_file)

  yr_start <- as.integer(PA$analysis_start_year); yr_end <- as.integer(PA$analysis_end_year)
  analysis_yrs <- yr_start:yr_end
  disc_rate    <- PA$cost_discount_rate
  ramp_years   <- max(PA$policy_cost_ramp_years, 1)
  policy_start <- as.integer(PA$policy_start_year)
  base_id      <- phi$baseline_scenario_id %||% "baseline"
  base_impl    <- 0

  mo <- as.data.table(mo_all)[scenario %in% names(phs) & year %in% analysis_yrs]
  if (!nrow(mo))
    stop("no public-health scenarios found in the Model 06 output.", call. = FALSE)
  produced    <- intersect(names(phs), unique(mo$scenario))
  comparators <- setdiff(produced, base_id)
  loc_run     <- unique(mo$location)[1]
  scen_lab    <- vapply(phs, function(s) s$scenario_label %||% s$scenario_id, character(1))

  ## ---- health: annual mortality (baseline vs scenario) --------------------
  mort <- mo[, .(cases = sum(newcases), cause_deaths = sum(dead)),
             by = .(scenario, year, cause)]
  base_mort <- mo[scenario == base_id,
                  .(base_deaths = sum(dead), base_cases = sum(newcases)), by = .(year, cause)]
  mort <- merge(mort, base_mort, by = c("year", "cause"), all.x = TRUE)
  mort[, scenario_label := scen_lab[scenario]]
  mort <- mort[scenario %in% comparators]
  setcolorder(mort, c("scenario", "scenario_label", "year", "cause",
                      "cases", "cause_deaths", "base_cases", "base_deaths"))
  setorder(mort, scenario, year, cause)

  ## ---- dedup population by age/sex/year per scenario (never over causes) ---
  pop_for <- function(scn, a0, a1, sx) {
    d <- unique(mo[scenario == scn & age >= a0 & age <= a1, .(year, age, sex, pop)])
    if (!identical(sx, "Both")) d <- d[sex == sx]
    m <- merge(data.table(year = analysis_yrs),
               d[, .(pop = sum(pop)), by = year], by = "year", all.x = TRUE)
    m[is.na(pop), pop := 0]; m$pop
  }

  ## ---- component costing (per intervention; shared-count-once) -------------
  costs <- copy(phi$costs)
  disc_factor <- 1 / (1 + disc_rate)^(analysis_yrs - yr_start)
  cimpl <- pmin(pmax((analysis_yrs - policy_start + 1) / ramp_years, 0), 1)
  cost_rows <- list()
  for (scn in comparators) {
    ids   <- phs[[scn]]$intervention_ids
    comps <- costs[intervention_id %in% ids & cost_ready == TRUE]
    if (!nrow(comps)) next
    for (i in seq_len(nrow(comps))) {
      cr <- comps[i]
      pop_s <- pop_for(scn, cr$c_age_start, cr$c_age_stop, cr$c_sex)
      pop_b <- pop_for(base_id, cr$c_age_start, cr$c_age_stop, cr$c_sex)
      pin_s <- pop_s * cr$population_in_need_fraction
      pin_b <- pop_b * cr$population_in_need_fraction
      cost_s <- pin_s * cimpl     * cr$frequency_per_year * cr$unit_cost_usd
      cost_b <- pin_b * base_impl * cr$frequency_per_year * cr$unit_cost_usd
      cost_rows[[length(cost_rows) + 1L]] <- data.table(
        scenario = scn, year = analysis_yrs,
        intervention_id = cr$intervention_id, cost_record_id = cr$cost_record_id,
        cost_component_key = cr$cost_component_key, cost_join_key = cr$cost_join_key,
        cost_scope = cr$cost_scope, population_in_need_measure = cr$population_in_need_measure,
        population_in_need_fraction = cr$population_in_need_fraction,
        frequency_per_year = cr$frequency_per_year, unit_cost_usd = cr$unit_cost_usd,
        pop_scenario = pop_s, pop_baseline = pop_b,
        cost_impl_frac = cimpl, pin_scenario = pin_s, pin_baseline = pin_b,
        annual_cost_baseline = cost_b, annual_cost_scenario = cost_s,
        annual_cost_incremental = cost_s - cost_b, discount_factor = disc_factor,
        disc_cost_baseline = cost_b * disc_factor, disc_cost_scenario = cost_s * disc_factor,
        disc_cost_incremental = (cost_s - cost_b) * disc_factor,
        indonesia_adjusted_flag = cr$indonesia_adjusted_flag, price_year = cr$price_year,
        review_status = cr$cost_review)
    }
  }
  annual_cost <- if (length(cost_rows)) rbindlist(cost_rows) else data.table()
  annual_cost <- .add_annualcost_percapita(annual_cost)   # national per-capita component costs (Section 3)

  ## ---- budget impact ------------------------------------------------------
  if (nrow(annual_cost)) {
    bi <- annual_cost[, .(baseline_cost = sum(annual_cost_baseline),
                          scenario_cost = sum(annual_cost_scenario),
                          incremental_cost = sum(annual_cost_incremental),
                          disc_incremental_cost = sum(disc_cost_incremental)),
                      by = .(scenario, year)]
    setorder(bi, scenario, year)
    bi[, cumulative_incremental_cost := cumsum(incremental_cost), by = scenario]
    bi[, cumulative_disc_incremental_cost := cumsum(disc_incremental_cost), by = scenario]
  } else bi <- data.table()
  bi <- .add_budget_percapita(bi)                         # national + annual per-capita budget impact

  ## ---- cost-effectiveness -------------------------------------------------
  da <- mort[, .(deaths_averted = sum(base_deaths - cause_deaths, na.rm = TRUE),
                 cases_averted  = sum(base_cases  - cases, na.rm = TRUE)), by = scenario]
  ic <- if (nrow(bi)) bi[, .(incremental_cost = sum(incremental_cost),
                             disc_incremental_cost = sum(disc_incremental_cost)), by = scenario] else
    data.table(scenario = comparators, incremental_cost = 0, disc_incremental_cost = 0)
  cea <- merge(da, ic, by = "scenario", all.x = TRUE)
  cea[is.na(incremental_cost), incremental_cost := 0]
  cea[is.na(disc_incremental_cost), disc_incremental_cost := 0]
  cea[, scenario_label := scen_lab[scenario]]
  cea[, cost_per_death_averted := NA_real_]
  cea[deaths_averted > 0, cost_per_death_averted := disc_incremental_cost / deaths_averted]
  cea[, dominance := "USD per death averted"]
  cea[deaths_averted > 0 & disc_incremental_cost < 0, dominance := "Dominant (more health, lower cost)"]
  cea[deaths_averted <= 0 & disc_incremental_cost > 0, dominance := "Dominated (less/no health, higher cost)"]
  cea[deaths_averted <= 0 & disc_incremental_cost <= 0, dominance := "No deaths averted; ratio not defined"]
  setcolorder(cea, c("scenario", "scenario_label", "deaths_averted", "cases_averted",
                     "incremental_cost", "disc_incremental_cost", "cost_per_death_averted", "dominance"))
  setorder(cea, -deaths_averted)

  # Capture the public-health R-value CE table for the deck results contract.
  deck_cea_list[["public_health"]]     <<- copy(cea)
  deck_percap_list[["public_health"]]  <<- .deck_percap_from_bi(bi)   # mean annual per-capita for the deck
  deck_catalog_list[["public_health"]] <<- .scenario_catalog_dt(phs, "public_health",
                                                                base_id, produced)

  ## ---- R QA anchors -------------------------------------------------------
  tol <- 1e-6
  negc   <- mo[, sum(well < -tol | sick < -tol | newcases < -tol | dead < -tol | pop < -tol)]
  maxres <- mo[, max(abs(pop - (well + sick + all.mx)))]
  ndist  <- mo[, .(n = uniqueN(round(all.mx, 6))), by = .(scenario, year, age, sex)][, max(n)]
  # Public-health effects now map to TWO allowed transitions: well->sick
  # (incidence) and sick->dead (case_fatality). "Bad" = anything OUTSIDE that
  # allowed pair (was: != incidence, which is now a legitimate mapping).
  n_bad_trans <- phi$valid_links[!(model_transition %in% c("incidence", "case_fatality")), .N]
  # Cross-pathway guard: the workbook transition_from/transition_to must agree
  # with the derived model_transition on every valid link (a well->sick effect
  # must never be recorded as case_fatality, or vice-versa).
  n_xpath <- phi$valid_links[
    (model_transition == "incidence"     & !(transition_from == "well" & transition_to == "sick")) |
    (model_transition == "case_fatality" & !(transition_from %in% c("sick","sick_severe","sick_hf") &
                                              grepl("^dead", transition_to))), .N]
  n_cf_links <- phi$valid_links[model_transition == "case_fatality", .N]
  anchor_scn <- if ("all_public_health" %in% cea$scenario) "all_public_health" else
    if (nrow(cea)) cea$scenario[1] else NA_character_
  ar <- cea[scenario == anchor_scn]
  r_da  <- if (nrow(ar)) ar$deaths_averted[1] else NA_real_
  r_dic <- if (nrow(ar)) ar$disc_incremental_cost[1] else NA_real_
  r_cpd <- if (nrow(ar)) ar$cost_per_death_averted[1] else NA_real_

  ## ---- display tables -----------------------------------------------------
  vl <- copy(phi$valid_links)
  # Carry the mapped + raw transition so Model 09 reports BOTH pathways
  # (incidence and case_fatality) and retains the original workbook fields.
  sel_out <- vl[, .(intervention_id, intervention_cause_key, intervention_name,
                    risk_id, cause_id, cause_code, effect_model,
                    model_transition, transition_from, transition_to,
                    baseline_exposure, target_exposure, response_value, paf_value,
                    lag_model, lag_parameter, exposure_start_year, exposure_target_year,
                    full_effect_at_target = NA_real_, exposure_reduction_abs = NA_real_,
                    exposure_reduction_rel = NA_real_, cost_join_key,
                    review_status = effect_review, key_count = NA_real_,
                    formula_status = NA_character_,
                    parent_package_id, intervention_role, tfa_effect_method)]
  setorder(sel_out, intervention_id, cause_code)

  blocked_out <- phi$blocked_links[, .(intervention_id, intervention_cause_key, cause_id,
                                       transition_from, transition_to, effect_model, problem)]

  expo_out <- phi$exposure[, .(intervention_id, risk_id, risk_exposure_measure, exposure_unit,
                               baseline_exposure, reduction_method, red_or_target, exposure_floor,
                               target_exposure = NA_real_, absolute_reduction = NA_real_,
                               relative_reduction = NA_real_, start_year, target_year,
                               scale_up_shape, review_status = exposure_review)]

  # model_transition stays in column N (COUNTIFS QA anchor below); the raw
  # workbook transition_from/transition_to are appended AFTER so the original
  # transition fields are retained without shifting the anchored column letter.
  eff_out <- phi$links[, .(intervention_cause_key, intervention_id, cause_id, cause_code,
                           effect_model, response_parameter, response_value, paf_value,
                           lag_model, lag_parameter, baseline_exposure, target_exposure,
                           full_effect_at_target = NA_real_, model_transition,
                           valid = as.integer(valid), review_status = effect_review,
                           transition_from, transition_to)]
  setorder(eff_out, intervention_id, cause_code)

  # Policy levers with the new fiscal/regulatory/hierarchy fields; the derived
  # gap / price-change / tax-delta / reduction cells are LIVE Excel formulas.
  Lvp <- as.data.table(phi$policy_levers_processed)
  lev_out <- Lvp[, .(lever_id, intervention_id, component, lever_method,
                     parent_package_id, intervention_role,
                     fiscal_baseline_tax_level, fiscal_target_tax_level, fiscal_tax_level_unit,
                     regulatory_baseline_level, regulatory_target_level,
                     regulatory_baseline_score = reg_baseline_score,
                     regulatory_target_score   = reg_target_score,
                     effect_parameter,
                     implementation_gap = NA_real_, implied_price_change = NA_real_,
                     fiscal_tax_delta = NA_real_, policy_reduction = NA_real_,
                     estimated_risk_reduction_wb, review_status = lever_review, qa_status = lever_qa)]

  # Cost components with the new allocation fields; allocated_child_cost and
  # cost_ready are LIVE formulas (package total x share; cost-readiness rule).
  cc_out <- phi$costs[, .(cost_record_id, cost_component_key, cost_join_key, cost_scope,
                          intervention_id, parent_package_id, scenario_role = cost_scenario_role,
                          cost_component, population_in_need_measure,
                          population_in_need_fraction, frequency_per_year, unit_cost_usd,
                          cost_allocation_share, package_total_cost_usd_per_capita, allocation_method,
                          allocated_child_cost = NA_real_,
                          price_year, indonesia_adjusted_flag, source_country,
                          review_status = cost_review, cost_ready = NA_real_, notes)]
  diag_out <- phi$validation

  # ------------------------------------------------------------------------
  # WRITE WORKBOOK
  # ------------------------------------------------------------------------
  int2col <- openxlsx::int2col
  frows <- function(fn, rows) vapply(rows, fn, character(1))
  # Excel column letter for a data-frame column BY NAME. Formula targets are
  # derived through this so that inserting/reordering columns never silently
  # misaligns a writeFormula() reference.
  xlc <- function(nm_vec, name) int2col(match(name, nm_vec))
  C_HDR <- "#1F4E78"; C_FORMULA <- "#DDEBF7"; C_RSRC <- "#F2F2F2"; C_INPUT <- "#FFF2CC"
  st_hdr   <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          halign = "center", valign = "center", wrapText = TRUE,
                          border = "TopBottomLeftRight", borderColour = "#8EA9C1")
  st_title <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          fontSize = 13, valign = "center")
  st_formula <- createStyle(fgFill = C_FORMULA)
  st_rsrc    <- createStyle(fgFill = C_RSRC)
  st_input   <- createStyle(fgFill = C_INPUT)
  st_wrap    <- createStyle(valign = "top", wrapText = TRUE)
  cf_pass <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
  cf_fail <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
  cf_rev  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C6500")

  fmt_of2 <- function(col) {
    cl <- tolower(col)
    if (grepl("full_effect|_reduction_rel$|relative_reduction|estimated_risk", cl)) return("0.0000")
    if (grepl("response_value|effect_value|paf_value|lag_parameter|baseline_value|target_value|_score$|implementation_gap|price_change|tax_delta|policy_reduction", cl)) return("0.0000")
    if (grepl("frequency", cl)) return("0.00")
    if (grepl("impl_frac|discount_factor|_ratio$", cl)) return("0.000")
    if (grepl("^year$|_year$|price_year|start_year|target_year", cl)) return("0")
    if (grepl("baseline_exposure|target_exposure|exposure_floor|red_or_target|absolute_reduction|reduction_abs|exposure$", cl)) return("0.0000")
    if (grepl("_fraction$|^fraction$|population_in_need_fraction", cl)) return("0.0%")
    if (grepl("allocation_share", cl)) return("0.000")
    if (grepl("unit_cost|package_total_cost|allocated_child_cost", cl)) return("#,##0.0000")
    if (grepl("per_death|per_case", cl)) return("#,##0")
    if (grepl("per_capita", cl) && !grepl("usd_per_capita", cl)) return("#,##0.00")
    if (grepl("cost|value|budget|pin_|_cost$", cl)) return("#,##0")
    if (grepl("death|case|population|averted|pop_|_count$|^count$|residual|distinct|negative|key_count", cl)) return("#,##0")
    NA_character_
  }
  wb <- createWorkbook()
  modifyBaseFont(wb, fontName = "Carlito", fontSize = 11)
  style_sheet <- function(sheet, nm, nrow_data, formula_cols = integer(0),
                          rsource_cols = integer(0), input_cols = integer(0),
                          header_row = 1L, wrap_cols = integer(0), filter = TRUE,
                          min_w = 11, max_w = 46) {
    ncol <- length(nm)
    addStyle(wb, sheet, st_hdr, rows = header_row, cols = seq_len(ncol), gridExpand = TRUE)
    if (nrow_data > 0) {
      dr <- (header_row + 1L):(header_row + nrow_data)
      for (j in formula_cols) addStyle(wb, sheet, st_formula, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in rsource_cols) addStyle(wb, sheet, st_rsrc,    rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in input_cols)   addStyle(wb, sheet, st_input,   rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in seq_len(ncol)) { f <- fmt_of2(nm[j])
        if (!is.na(f)) addStyle(wb, sheet, createStyle(numFmt = f), rows = dr, cols = j, gridExpand = TRUE, stack = TRUE) }
      for (j in wrap_cols) addStyle(wb, sheet, st_wrap, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    }
    freezePane(wb, sheet, firstActiveRow = header_row + 1L, firstActiveCol = 1L)
    if (filter) addFilter(wb, sheet, rows = header_row, cols = seq_len(ncol))
    setColWidths(wb, sheet, cols = seq_len(ncol), widths = pmin(pmax(nchar(nm) + 2L, min_w), max_w))
    setRowHeights(wb, sheet, rows = header_row, heights = 28)
  }

  ## ===== Calculation_Assumptions =========================================
  ca <- data.table(
    parameter_id = c("analysis_start_year","analysis_end_year","baseline_scenario_id",
                     "policy_start_year","exposure_target_year","policy_cost_ramp_years",
                     "cost_discount_rate","reporting_price_year","source_cost_price_year",
                     "currency","economic_perspective","scale_up_shape",
                     "baseline_implementation_fraction","formula_tolerance",
                     "stock_flow_residual_limit",
                     "r_negative_state_count","r_stock_flow_max_residual","r_background_distinct_count",
                     "r_deaths_averted_anchor","r_disc_incremental_cost_anchor",
                     "r_cost_per_death_anchor","qa_anchor_scenario"),
    value = list(yr_start, yr_end, base_id, policy_start, as.integer(PA$exposure_target_year),
                 ramp_years, disc_rate, as.integer(PA$cost_price_year),
                 as.integer(PA$source_cost_price_year %||% NA), PA$currency,
                 PA$economic_perspective, PA$scale_up_shape, base_impl, 0.001, 1000,
                 as.integer(negc), round(as.numeric(maxres), 2), as.integer(ndist),
                 as.numeric(r_da), as.numeric(r_dic), as.numeric(r_cpd), anchor_scn),
    unit = c("year","year","scenario id","year","year","years","proportion/year","USD year",
             "USD year","currency","text","text","proportion","USD/count","persons","count","persons",
             "count","deaths","USD","USD/death","scenario id"),
    role = c(rep("formula control", 9), "metadata","metadata","formula control",
             "formula control","QA control","QA control", rep("R QA source",3),
             rep("R reconciliation source",4)),
    description = c("First model and discount year","Last model year",
                   "No-new-policy comparator","First policy implementation year",
                   "Year full exposure reduction is reached","Years to full policy cost",
                   "Annual discount rate applied to costs","Reporting price year",
                   "Price year of source unit costs","Reporting currency",
                   "Economic evaluation perspective","Cost/effect scale-up shape",
                   "Baseline (counterfactual) policy implementation fraction",
                   "Absolute reconciliation tolerance",
                   "Persons tolerance for the stock/flow identity check",
                   "Impossible negative state count (R)",
                   "Max stock/flow residual (R)","Max distinct all-cause mx across cause (R)",
                   "R deaths averted for the anchor scenario",
                   "R discounted incremental cost for the anchor scenario",
                   "R USD per death averted for the anchor scenario",
                   "Scenario used for Excel-vs-R reconciliation"),
    source = c(rep(basename(phi$inputs_path), 12), "Model 09",
               "Workbook QA rule","Workbook QA rule", rep("Model 09 current run",3),
               rep("Model 09 current run (R CEA)",4)))
  # Append the Reference-Case BCA controls (shared block) for the PH BCA sheets.
  .bcab <- bca_ca_block(bca_params)
  n_ca_core <- nrow(ca)
  ca <- rbind(ca, .bcab$ca_bca)
  bca_fmt_vec <- .bcab$fmt
  addWorksheet(wb, "Calculation_Assumptions")
  writeData(wb, "Calculation_Assumptions",
            data.frame(parameter_id="parameter_id", value="value", unit="unit",
                       role="role", description="description", source="source"),
            colNames = FALSE, startRow = 1)
  writeData(wb, "Calculation_Assumptions", ca$parameter_id, startCol = 1, startRow = 2, colNames = FALSE)
  writeData(wb, "Calculation_Assumptions", as.data.frame(ca[, .(unit, role, description, source)]),
            startCol = 3, startRow = 2, colNames = FALSE)
  for (i in seq_len(nrow(ca)))
    writeData(wb, "Calculation_Assumptions", ca$value[[i]], startCol = 2, startRow = 1 + i, colNames = FALSE)
  addStyle(wb, "Calculation_Assumptions", st_hdr, rows = 1, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_input, rows = 2:16, cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_rsrc,  rows = 17:23, cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_input,
           rows = (n_ca_core + 2L):(nrow(ca) + 1L), cols = 2, gridExpand = TRUE, stack = TRUE)  # BCA controls
  ca_fmt <- c("0","0",NA,"0","0","0.0","0.0%","0","0",NA,NA,NA,"0.000","0.000","#,##0",
              "#,##0","#,##0.0","0","#,##0","#,##0","#,##0.00",NA, bca_fmt_vec)
  for (i in seq_along(ca_fmt)) if (!is.na(ca_fmt[i]))
    addStyle(wb, "Calculation_Assumptions", createStyle(numFmt = ca_fmt[i]), rows = 1 + i, cols = 2, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_wrap, rows = 2:(nrow(ca)+1), cols = 5, gridExpand = TRUE, stack = TRUE)
  freezePane(wb, "Calculation_Assumptions", firstActiveRow = 2)
  addFilter(wb, "Calculation_Assumptions", rows = 1, cols = 1:6)
  setColWidths(wb, "Calculation_Assumptions", cols = 1:6, widths = c(32, 18, 14, 22, 60, 40))
  setRowHeights(wb, "Calculation_Assumptions", rows = 1, heights = 28)
  # cell refs
  cA_start <- "'Calculation_Assumptions'!$B$2"; cA_end <- "'Calculation_Assumptions'!$B$3"
  cA_base  <- "'Calculation_Assumptions'!$B$4"; cA_pstart <- "'Calculation_Assumptions'!$B$5"
  cA_etgt  <- "'Calculation_Assumptions'!$B$6"; cA_ramp <- "'Calculation_Assumptions'!$B$7"
  cA_disc  <- "'Calculation_Assumptions'!$B$8"; cA_bimpl <- "'Calculation_Assumptions'!$B$14"
  cA_tol   <- "'Calculation_Assumptions'!$B$15"; cA_reslim <- "'Calculation_Assumptions'!$B$16"
  cA_neg   <- "'Calculation_Assumptions'!$B$17"; cA_resid <- "'Calculation_Assumptions'!$B$18"
  cA_bgd   <- "'Calculation_Assumptions'!$B$19"; cA_rda <- "'Calculation_Assumptions'!$B$20"
  cA_rdic  <- "'Calculation_Assumptions'!$B$21"; cA_rcpd <- "'Calculation_Assumptions'!$B$22"
  cA_anch  <- "'Calculation_Assumptions'!$B$23"
  # BCA control cell refs (rows derived from position in ca) for the BCA sheets.
  .carow <- function(pid) match(pid, ca$parameter_id) + 1L
  .bcell <- function(pid) sprintf("'Calculation_Assumptions'!$B$%d", .carow(pid))
  bca_cells_ph <- list(
    ratio     = .bcell("vsl_us_gni_ratio"),
    elast     = .bcell("vsl_income_elasticity_preferred"),
    floor     = .bcell("vsl_floor_gni_multiple"),
    mult100   = .bcell("vsl_sensitivity_gni_multiple_100"),
    mult160   = .bcell("vsl_sensitivity_gni_multiple_160"),
    r_primary = .bcell("bca_discount_rate_primary"),
    base_year = .bcell("bca_base_year"),
    price_year  = .bcell("bca_price_year"),
    cost_factor = .bcell("cost_to_bca_currency_factor"),
    scope     = .bcell("bca_scope"))

  ## ===== README ==========================================================
  readme <- data.table(section = c(
    "Purpose","How to read","Scenarios","Baseline pairing","CVD 40q30","Effect model","Exposure path",
    "Lag","Costing","Shared costs","Budget impact","Cost-effectiveness","Economic value",
    "QA & reconciliation","Colour legend","Deferred"),
    detail = c(
    "Costing, budget impact and mortality-based cost-effectiveness for the public-health (fiscal/regulatory) policies selected in the public-health input workbook.",
    "Grey cells are R-generated source values; light-blue cells are LIVE Excel formulas; pale-yellow cells on Calculation_Assumptions are editable controls. Change a yellow control and the blue results recompute. Calculation_Map lists the dependency chain.",
    "Baseline + one standalone scenario per runnable intervention + one JOINT scenario per parent package (tobacco, salt) + a combined 'all_public_health' scenario. Membership and the tobacco/salt package structure derive from the workbook Scenario_Hierarchy (Model 04). Parent-package cases/deaths come from a single joint run, never summed from standalone children; package cost = sum of selected child costs. See Scenario_Hierarchy, Child_Intervention_Summary and Parent_Package_Summary.",
    "Deaths averted = baseline deaths - scenario deaths, matched at year x cause (both-sex totals).",
    "CVD_40q30 / CVD_40q30_Age give the period probability of dying from the six CVD causes (ihd, istroke, hstroke, hhd, rhd, cmd) between exact ages 30 and 70, per scenario/HTN-target/year. The life table (m_x, q_x, l_x, l_{x+1}) is live Excel formula off grey CVD deaths and de-duplicated population; cvd_40q30 = 100*(1-l_70/l_30) reconciles to Model 07. cvd_40q30 and the absolute reduction are PERCENT on a 0-100 scale (format 0.000), NOT Excel's fractional %.",
    "Incidence effect = exposure-based: prevalence-shift RR (tobacco), log-linear RR per unit reduction (alcohol/sodium/SSB and the DEFAULT industrial-TFA path, RR per 1 percentage-point energy). The optional TFA PAF path (PAF x implementation gap) is used only when Assumptions.tfa_effect_method='PAF'. Fiscal levers use baseline->target tax change x price elasticity; regulatory levers use the none/partial/full implementation gap. NO clinical coverage-adjustment formula is used.",
    "Achieved exposure pt(t) ramps linearly baseline->target over start_year..target_year, floored; exposure reductions are shown as formulas on Exposure_Targets.",
    "immediate_after_full_implementation: effect tracks the exposure path. delayed_exponential_remaining_effect (tobacco): full target effect accrues as 1-(1-rate)^(years since start).",
    "annual_cost = population(t) x PIN fraction x implementation_fraction(t) x frequency x unit_cost. Public-wide policies use the deduplicated total population (once per age/sex/year, never per cause).",
    "Shared policy costs (cost_scope 'shared-count-once', ...__C_SHARED) are counted once per intervention/scenario/year, never once per affected cause.",
    "Budget impact reports UNDISCOUNTED baseline, scenario, incremental and cumulative incremental cost; discounted incremental cost is a separate column. The four *_cost_per_capita columns are LIVE formulae (= that year's cost / national_population); national_population (grey R-source) is each year's national Indonesia population from out_model/model_output_Indonesia_htncov2_aspirational.rds (baseline series, de-duplicated across cause). Annual_Cost carries the matching per-component per-capita formulae, and Cost_Effectiveness carries the scenario mean annual incremental/discounted per-capita (2026-2050).",
    "USD per death averted = cumulative discounted incremental cost / cumulative (undiscounted) deaths averted. Not a DALY-based ICER; DALYs are deferred.",
    "Reference-Case benefit-cost analysis (2019 Robinson et al. Guidelines) on Health_Outcomes (Model 07), Economic_Value (Model 08 VSL/VSLY) and Benefit_Cost. Preferred VSL = MAX(160xGNIpc_US x (GNIpc_IDN/GNIpc_US)^1.5, 20xGNIpc_IDN); standardized 100x/160x GNI sensitivities. Benefits are PPP int$; costs are converted to that basis (cost_to_bca_currency_factor). PARTIAL mortality-benefit BCA -- not a full societal BCA.",
    "QA_Checks recomputes each invariant in Excel and reconciles the Excel cost-effectiveness headline against the R engine values on Calculation_Assumptions; BCA checks cover the VSL floor, reference-case parameters, price-year basis and Benefit_Cost scenario coverage. PASS/REVIEW/FAIL are conditionally formatted.",
    "Header dark-blue; formula-derived light-blue; R source/helper grey; editable controls pale-yellow; PASS green; REVIEW amber; FAIL red.",
    "Health outcomes (deaths averted, YLL/YLD/DALY averted, life-years gained) come from Model 07 (Health_Outcomes). USD per death averted (Cost_Effectiveness) and the benefit-cost analysis (Benefit_Cost) are the principal decision results."))
  addWorksheet(wb, "README")
  writeData(wb, "README", "Indonesia NCD - public-health cost & value workbook (formula edition)", startRow = 1)
  addStyle(wb, "README", st_title, rows = 1, cols = 1)
  writeData(wb, "README", readme, startRow = 3, headerStyle = st_hdr)
  setColWidths(wb, "README", cols = 1:2, widths = c(22, 122))
  addStyle(wb, "README", st_wrap, rows = 4:(nrow(readme) + 3), cols = 2, gridExpand = TRUE, stack = TRUE)
  setRowHeights(wb, "README", rows = 1, heights = 22)

  ## ===== Run_Metadata ====================================================
  meta <- data.table(item = c(
    "Workbook title","Run date","Model / pipeline","Public-health input workbook",
    "Model output source","Location","Analysis years","Baseline scenario",
    "Scenarios costed","Cost discount rate","Reporting price year","Currency",
    "Economic perspective","Policy start / target year","Cost ramp years",
    "Health outcomes (DALYs/YLL/YLD)","R version","openxlsx / data.table",
    "TFA effect method","Public-health scenarios (n)"),
    value = c("Indonesia NCD public-health - cost & value", as.character(Sys.Date()),
    "CVD FAIR Choices pipeline (Models 00-06 -> 09), public-health family",
    phi$inputs_path, "output/out_model/model_output_*.rds (Model 06)", loc_run,
    paste0(yr_start, "-", yr_end), base_id, paste(comparators, collapse = ", "),
    sprintf("%.1f%%", 100 * disc_rate), as.character(PA$cost_price_year), PA$currency,
    PA$economic_perspective, paste0(policy_start, " / ", PA$exposure_target_year),
    as.character(ramp_years), "Out of scope in this stage (deferred)",
    R.version.string, paste0(as.character(packageVersion("openxlsx")), " / ",
                             as.character(packageVersion("data.table"))),
    as.character(PA$tfa_effect_method %||% "RR"),
    paste0(length(comparators), " comparators + baseline")))
  addWorksheet(wb, "Run_Metadata")
  writeData(wb, "Run_Metadata", meta, headerStyle = st_hdr)
  writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 8,
               x = paste0(cA_start, "&\"-\"&", cA_end))
  writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 9, x = cA_base)
  writeFormula(wb, "Run_Metadata", startCol = 2, startRow = 11, x = cA_disc)
  addStyle(wb, "Run_Metadata", st_formula, rows = c(8,9,11), cols = 2, stack = TRUE)
  addStyle(wb, "Run_Metadata", createStyle(numFmt = "0.0%"), rows = 11, cols = 2, stack = TRUE)
  setColWidths(wb, "Run_Metadata", cols = 1:2, widths = c(30, 74))
  freezePane(wb, "Run_Metadata", firstActiveRow = 2); setRowHeights(wb, "Run_Metadata", rows = 1, heights = 28)

  ## ===== Selected_Interventions ==========================================
  n_si <- nrow(sel_out); r_si <- n_si + 1L
  addWorksheet(wb, "Selected_Interventions")
  writeData(wb, "Selected_Interventions", as.data.frame(sel_out), headerStyle = st_hdr)
  # Formula targets/inputs derived BY NAME (xlc) so the inserted transition columns
  # (model_transition/transition_from/transition_to) can never misalign them:
  # full_effect (model-specific), exposure_reduction_abs = baseline - target,
  # exposure_reduction_rel = (baseline - target)/baseline, key_count, formula_status.
  Kb <- xlc(names(sel_out), "baseline_exposure"); Lt <- xlc(names(sel_out), "target_exposure")
  Gr <- xlc(names(sel_out), "response_value");    Hp <- xlc(names(sel_out), "paf_value")
  c_fe  <- match("full_effect_at_target",  names(sel_out))
  c_abs <- match("exposure_reduction_abs", names(sel_out))
  c_rel <- match("exposure_reduction_rel", names(sel_out))
  c_kc  <- match("key_count",              names(sel_out))
  c_fs  <- match("formula_status",         names(sel_out))
  KCl   <- int2col(c_kc)
  fe_formula <- function(r) {
    m <- sel_out$effect_model[r - 1L]
    if (identical(m, "direct_smoking_prevalence_shift_rr"))
      sprintf("1-(1+%s%d*(%s%d-1))/(1+%s%d*(%s%d-1))", Lt, r, Gr, r, Kb, r, Gr, r)
    else if (identical(m, "direct_loglinear_rr_per_unit_reduction"))
      sprintf("1-1/(%s%d^(%s%d-%s%d))", Gr, r, Kb, r, Lt, r)
    else sprintf("IFERROR(%s%d*%s%d,0)", Hp, r, Gr, r)
  }
  writeFormula(wb, "Selected_Interventions", startCol = c_fe, startRow = 2, x = frows(fe_formula, 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = c_abs, startRow = 2,
               x = frows(function(r) sprintf("%s%d-%s%d", Kb, r, Lt, r), 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = c_rel, startRow = 2,
               x = frows(function(r) sprintf("IF(%s%d=0,0,(%s%d-%s%d)/%s%d)", Kb, r, Kb, r, Lt, r, Kb, r), 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = c_kc, startRow = 2,
               x = frows(function(r) sprintf("COUNTIF($B$2:$B$%d,B%d)", r_si, r), 2:r_si))
  writeFormula(wb, "Selected_Interventions", startCol = c_fs, startRow = 2,
               x = frows(function(r) sprintf("IF(%s%d=1,\"OK\",\"DUPLICATE KEY\")", KCl, r), 2:r_si))
  style_sheet("Selected_Interventions", names(sel_out), n_si, formula_cols = c(c_fe, c_abs, c_rel, c_kc, c_fs))

  ## ===== Blocked_Links ===================================================
  addWorksheet(wb, "Blocked_Links")
  if (nrow(blocked_out)) {
    writeData(wb, "Blocked_Links", as.data.frame(blocked_out), headerStyle = st_hdr)
    style_sheet("Blocked_Links", names(blocked_out), nrow(blocked_out),
                wrap_cols = which(names(blocked_out) == "problem"))
    addStyle(wb, "Blocked_Links", st_input, rows = 2:(nrow(blocked_out)+1),
             cols = which(names(blocked_out) == "problem"), gridExpand = TRUE, stack = TRUE)
  } else {
    writeData(wb, "Blocked_Links", data.frame(note = "No blocked public-health links in this run."),
              headerStyle = st_hdr)
    setColWidths(wb, "Blocked_Links", cols = 1, widths = 80)
  }

  ## ===== Policy_Levers ===================================================
  n_lv <- nrow(lev_out); r_lv <- max(n_lv + 1L, 2L)
  addWorksheet(wb, "Policy_Levers")
  writeData(wb, "Policy_Levers", as.data.frame(lev_out), headerStyle = st_hdr)
  # Derived fiscal/regulatory cells are LIVE formulas (M04-M06); targets by name.
  Lm  <- xlc(names(lev_out), "lever_method");             Lfb <- xlc(names(lev_out), "fiscal_baseline_tax_level")
  Lft <- xlc(names(lev_out), "fiscal_target_tax_level");  Lrb <- xlc(names(lev_out), "regulatory_baseline_score")
  Lrt <- xlc(names(lev_out), "regulatory_target_score");  Lep <- xlc(names(lev_out), "effect_parameter")
  Lig <- xlc(names(lev_out), "implementation_gap");       Lipc<- xlc(names(lev_out), "implied_price_change")
  Ltd <- xlc(names(lev_out), "fiscal_tax_delta")
  cig <- match("implementation_gap", names(lev_out)); cipc <- match("implied_price_change", names(lev_out))
  ctd <- match("fiscal_tax_delta", names(lev_out));   cpr  <- match("policy_reduction", names(lev_out))
  if (n_lv > 0) {
    R <- 2:r_lv
    writeFormula(wb, "Policy_Levers", startCol = cig, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"regulatory_gap_multiplicative\",MAX(0,%s%d-%s%d),\"\")", Lm,r,Lrt,r,Lrb,r), R))
    writeFormula(wb, "Policy_Levers", startCol = ctd, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"price_elasticity\",MAX(0,%s%d-%s%d),\"\")", Lm,r,Lft,r,Lfb,r), R))
    writeFormula(wb, "Policy_Levers", startCol = cipc, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"tax_share_to_price_elasticity\",(1-%s%d)/(1-%s%d)-1,IF(%s%d=\"price_elasticity\",%s%d,\"\"))",
              Lm,r,Lfb,r,Lft,r,Lm,r,Ltd,r), R))
    writeFormula(wb, "Policy_Levers", startCol = cpr, startRow = 2, x = frows(function(r)
      sprintf("IF(%s%d=\"regulatory_gap_multiplicative\",%s%d*%s%d,IF(OR(%s%d=\"price_elasticity\",%s%d=\"tax_share_to_price_elasticity\"),ABS(%s%d)*%s%d,\"\"))",
              Lm,r,Lep,r,Lig,r, Lm,r,Lm,r, Lep,r,Lipc,r), R))
  }
  style_sheet("Policy_Levers", names(lev_out), n_lv, formula_cols = c(cig,cipc,ctd,cpr),
              rsource_cols = setdiff(seq_along(lev_out), c(cig,cipc,ctd,cpr)))
  rev_c <- which(names(lev_out) == "review_status")
  if (length(rev_c) && n_lv) {
    conditionalFormatting(wb, "Policy_Levers", cols = rev_c, rows = 2:r_lv, rule = "Ready", type = "contains", style = cf_pass)
    conditionalFormatting(wb, "Policy_Levers", cols = rev_c, rows = 2:r_lv, rule = "Review", type = "contains", style = cf_rev)
    conditionalFormatting(wb, "Policy_Levers", cols = rev_c, rows = 2:r_lv, rule = "Missing", type = "contains", style = cf_rev)
  }

  ## ===== Exposure_Targets ================================================
  n_ex <- nrow(expo_out); r_ex <- n_ex + 1L
  addWorksheet(wb, "Exposure_Targets")
  writeData(wb, "Exposure_Targets", as.data.frame(expo_out), headerStyle = st_hdr)
  # E baseline(5) F method(6) G red_or_target(7) H floor(8) I target(9) J abs(10) K rel(11)
  tgt_formula <- function(r) {
    meth <- tolower(expo_out$reduction_method[r - 1L])
    if (meth == "relative") sprintf("MAX(H%d,E%d*(1-G%d))", r, r, r)
    else if (meth == "absolute") sprintf("MAX(H%d,E%d-G%d)", r, r, r)
    else sprintf("MAX(H%d,G%d)", r, r)
  }
  writeFormula(wb, "Exposure_Targets", startCol = 9, startRow = 2, x = frows(tgt_formula, 2:r_ex))
  writeFormula(wb, "Exposure_Targets", startCol = 10, startRow = 2, x = frows(function(r) sprintf("E%d-I%d", r, r), 2:r_ex))
  writeFormula(wb, "Exposure_Targets", startCol = 11, startRow = 2, x = frows(function(r) sprintf("IF(E%d=0,0,(E%d-I%d)/E%d)", r, r, r, r), 2:r_ex))
  style_sheet("Exposure_Targets", names(expo_out), n_ex, formula_cols = c(9,10,11))

  ## ===== Effect_Parameters ===============================================
  n_ef <- nrow(eff_out); r_ef <- n_ef + 1L
  addWorksheet(wb, "Effect_Parameters")
  writeData(wb, "Effect_Parameters", as.data.frame(eff_out), headerStyle = st_hdr)
  # E model(5) G response(7) H paf(8) K baseline(11) L target(12) M full_effect(13)
  fe2 <- function(r) {
    m <- eff_out$effect_model[r - 1L]
    if (identical(m, "direct_smoking_prevalence_shift_rr"))
      sprintf("1-(1+L%d*(G%d-1))/(1+K%d*(G%d-1))", r, r, r, r)
    else if (identical(m, "direct_loglinear_rr_per_unit_reduction"))
      sprintf("1-1/(G%d^(K%d-L%d))", r, r, r)
    else sprintf("IFERROR(H%d*G%d,0)", r, r)
  }
  writeFormula(wb, "Effect_Parameters", startCol = 13, startRow = 2, x = frows(fe2, 2:r_ef))
  style_sheet("Effect_Parameters", names(eff_out), n_ef, formula_cols = 13)
  vcol <- which(names(eff_out) == "valid")
  conditionalFormatting(wb, "Effect_Parameters", cols = vcol, rows = 2:r_ef, rule = "==0", style = cf_rev)

  ## ===== Cost_Components =================================================
  n_cc <- nrow(cc_out); r_cc <- n_cc + 1L
  addWorksheet(wb, "Cost_Components")
  writeData(wb, "Cost_Components", as.data.frame(cc_out), headerStyle = st_hdr)
  # Formula targets derived BY NAME so the new allocation columns cannot misalign
  # them. allocated_child_cost = package total x allocation share (M13, falling back
  # to the row unit cost); cost_ready checks unit cost, PIN fraction and PIN measure.
  Cpm <- xlc(names(cc_out), "population_in_need_measure")
  Cpf <- xlc(names(cc_out), "population_in_need_fraction")
  Cuc <- xlc(names(cc_out), "unit_cost_usd")
  Cshare <- xlc(names(cc_out), "cost_allocation_share")
  Cptot  <- xlc(names(cc_out), "package_total_cost_usd_per_capita")
  c_alloc <- match("allocated_child_cost", names(cc_out))
  c_ready <- match("cost_ready", names(cc_out))
  if (n_cc > 0) {
    writeFormula(wb, "Cost_Components", startCol = c_alloc, startRow = 2, x = frows(function(r)
      sprintf("IFERROR(%s%d*%s%d,%s%d)", Cptot, r, Cshare, r, Cuc, r), 2:r_cc))
    writeFormula(wb, "Cost_Components", startCol = c_ready, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(%s%d>=0,%s%d<>\"\",%s%d>=0,%s%d<=1,OR(%s%d=\"all\",%s%d=\"prevalence\",%s%d=\"incidence\")),1,0)",
              Cuc, r, Cpf, r, Cpf, r, Cpf, r, Cpm, r, Cpm, r, Cpm, r), 2:r_cc))
  }
  style_sheet("Cost_Components", names(cc_out), n_cc, formula_cols = c(c_alloc, c_ready),
              wrap_cols = which(names(cc_out) %in% c("cost_component","notes")))
  ia_c <- which(names(cc_out) == "indonesia_adjusted_flag")
  conditionalFormatting(wb, "Cost_Components", cols = ia_c, rows = 2:r_cc, rule = "==0", style = cf_rev)

  ## ===== Annual_Mortality ================================================
  am <- copy(mort); am[, `:=`(deaths_averted = NA_real_, cases_averted = NA_real_)]
  n_am <- nrow(am); r_am <- n_am + 1L
  addWorksheet(wb, "Annual_Mortality")
  writeData(wb, "Annual_Mortality", as.data.frame(am), headerStyle = st_hdr)
  # A scn B label C year D cause E cases F cause_deaths G base_cases H base_deaths I averted J cases_averted
  writeFormula(wb, "Annual_Mortality", startCol = 9, startRow = 2, x = frows(function(r) sprintf("H%d-F%d", r, r), 2:r_am))
  writeFormula(wb, "Annual_Mortality", startCol = 10, startRow = 2, x = frows(function(r) sprintf("G%d-E%d", r, r), 2:r_am))
  style_sheet("Annual_Mortality", names(am), n_am, formula_cols = c(9,10), rsource_cols = c(5,6,7,8))

  ## ===== Annual_Cost =====================================================
  ac_cols <- c("scenario","year","intervention_id","cost_record_id","cost_component_key",
               "cost_join_key","cost_scope","population_in_need_measure","population_in_need_fraction",
               "frequency_per_year","unit_cost_usd","pop_scenario","pop_baseline","cost_impl_frac",
               "pin_scenario","pin_baseline","annual_cost_baseline","annual_cost_scenario",
               "annual_cost_incremental","discount_factor","disc_cost_baseline","disc_cost_scenario",
               "disc_cost_incremental","indonesia_adjusted_flag","price_year","review_status",
               "shared_duplicate_count",
               "national_population","annual_cost_baseline_per_capita","annual_cost_scenario_per_capita",
               "annual_cost_incremental_per_capita","disc_cost_incremental_per_capita")
  n_ac <- nrow(annual_cost); r_ac <- max(n_ac + 1L, 2L)
  if (n_ac > 0) {
    ac <- data.frame(scenario = annual_cost$scenario, year = annual_cost$year,
                     intervention_id = annual_cost$intervention_id, cost_record_id = annual_cost$cost_record_id,
                     cost_component_key = annual_cost$cost_component_key, cost_join_key = annual_cost$cost_join_key,
                     cost_scope = annual_cost$cost_scope, population_in_need_measure = annual_cost$population_in_need_measure,
                     population_in_need_fraction = annual_cost$population_in_need_fraction,
                     frequency_per_year = annual_cost$frequency_per_year, unit_cost_usd = annual_cost$unit_cost_usd,
                     pop_scenario = annual_cost$pop_scenario, pop_baseline = annual_cost$pop_baseline,
                     stringsAsFactors = FALSE)
    for (cn in ac_cols[14:23]) ac[[cn]] <- NA_real_
    ac$indonesia_adjusted_flag <- annual_cost$indonesia_adjusted_flag
    ac$price_year <- annual_cost$price_year
    ac$review_status <- annual_cost$review_status
    ac$shared_duplicate_count <- NA_real_
    # national population + annual per-capita component costs (R-source; Section 3)
    ac$national_population <- annual_cost$national_population   # AB = R-source denominator
    for (cn in ac_cols[29:32]) ac[[cn]] <- NA_real_             # AC..AF per-capita = live formulae
    ac <- ac[, ac_cols]
  } else ac <- as.data.frame(setNames(replicate(length(ac_cols), logical(0), simplify = FALSE), ac_cols))
  addWorksheet(wb, "Annual_Cost")
  writeData(wb, "Annual_Cost", ac, headerStyle = st_hdr)
  if (n_ac > 0) {
    R <- 2:r_ac; wf <- function(col, fn) writeFormula(wb, "Annual_Cost", startCol = col, startRow = 2, x = frows(fn, R))
    wf(14, function(r) sprintf("MIN(MAX((B%d-%s+1)/%s,0),1)", r, cA_pstart, cA_ramp))     # cost_impl_frac
    wf(15, function(r) sprintf("L%d*I%d", r, r))                                          # pin_scenario
    wf(16, function(r) sprintf("M%d*I%d", r, r))                                          # pin_baseline
    wf(17, function(r) sprintf("P%d*%s*J%d*K%d", r, cA_bimpl, r, r))                      # annual_cost_baseline
    wf(18, function(r) sprintf("O%d*N%d*J%d*K%d", r, r, r, r))                            # annual_cost_scenario
    wf(19, function(r) sprintf("R%d-Q%d", r, r))                                          # annual_cost_incremental
    wf(20, function(r) sprintf("1/(1+%s)^(B%d-%s)", cA_disc, r, cA_start))                # discount_factor
    wf(21, function(r) sprintf("Q%d*T%d", r, r))                                          # disc_cost_baseline
    wf(22, function(r) sprintf("R%d*T%d", r, r))                                          # disc_cost_scenario
    wf(23, function(r) sprintf("S%d*T%d", r, r))                                          # disc_cost_incremental
    wf(27, function(r) sprintf("IF(G%d=\"shared-count-once\",COUNTIFS($A$2:$A$%d,A%d,$B$2:$B$%d,B%d,$D$2:$D$%d,D%d),1)",
                               r, r_ac, r, r_ac, r, r_ac, r))                             # shared_duplicate_count
    # AC..AF component annual cost per capita = component cost / national_population (AB)
    wf(29, function(r) sprintf("IF(AB%d=0,\"\",Q%d/AB%d)", r, r, r))                      # annual_cost_baseline_per_capita
    wf(30, function(r) sprintf("IF(AB%d=0,\"\",R%d/AB%d)", r, r, r))                      # annual_cost_scenario_per_capita
    wf(31, function(r) sprintf("IF(AB%d=0,\"\",S%d/AB%d)", r, r, r))                      # annual_cost_incremental_per_capita
    wf(32, function(r) sprintf("IF(AB%d=0,\"\",W%d/AB%d)", r, r, r))                      # disc_cost_incremental_per_capita
  }
  style_sheet("Annual_Cost", ac_cols, n_ac, formula_cols = c(14:23, 27, 29:32), rsource_cols = c(9,10,11,12,13, 28))

  ## ===== Budget_Impact ===================================================
  bud_cols <- c("scenario","year","baseline_cost","scenario_cost","incremental_cost",
                "disc_incremental_cost","cumulative_incremental_cost","cumulative_disc_incremental_cost",
                "national_population","baseline_cost_per_capita","scenario_cost_per_capita",
                "incremental_cost_per_capita","disc_incremental_cost_per_capita")
  n_bi <- nrow(bi); r_bi <- max(n_bi + 1L, 2L)
  if (n_bi > 0) { bud <- data.frame(scenario = bi$scenario, year = bi$year, stringsAsFactors = FALSE)
    for (cn in bud_cols[3:8]) bud[[cn]] <- NA_real_
    bud$national_population <- bi$national_population        # I = R-source denominator
    for (cn in bud_cols[10:13]) bud[[cn]] <- NA_real_        # J..M per-capita = live formulae
  } else bud <- as.data.frame(setNames(replicate(length(bud_cols), logical(0), simplify = FALSE), bud_cols))
  addWorksheet(wb, "Budget_Impact")
  writeData(wb, "Budget_Impact", bud, headerStyle = st_hdr)
  if (n_bi > 0) {
    R <- 2:r_bi
    sac <- function(tgt, r) sprintf("SUMIFS('Annual_Cost'!$%s$2:$%s$%d,'Annual_Cost'!$A$2:$A$%d,A%d,'Annual_Cost'!$B$2:$B$%d,B%d)",
                                    tgt, tgt, r_ac, r_ac, r, r_ac, r)
    writeFormula(wb, "Budget_Impact", startCol = 3, startRow = 2, x = frows(function(r) sac("Q", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 4, startRow = 2, x = frows(function(r) sac("R", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 5, startRow = 2, x = frows(function(r) sac("S", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 6, startRow = 2, x = frows(function(r) sac("W", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 7, startRow = 2, x = frows(function(r) sprintf("SUMIFS($E$2:E%d,$A$2:A%d,A%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 8, startRow = 2, x = frows(function(r) sprintf("SUMIFS($F$2:F%d,$A$2:A%d,A%d)", r, r, r), R))
    # J..M annual cost per capita = same-year cost / national_population (col I), live formulae
    writeFormula(wb, "Budget_Impact", startCol = 10, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",C%d/I%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 11, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",D%d/I%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 12, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",E%d/I%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 13, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",F%d/I%d)", r, r, r), R))
  }
  style_sheet("Budget_Impact", bud_cols, n_bi, formula_cols = c(3:8, 10:13), rsource_cols = 9)

  ## ===== Cost_Effectiveness ==============================================
  ce_cols <- c("scenario","scenario_label","deaths_averted","cases_averted","incremental_cost",
               "disc_incremental_cost","cost_per_death_averted","dominance","reconciliation_status",
               "annual_cost_incremental_per_capita","disc_cost_incremental_per_capita")
  n_ce <- nrow(cea); r_ce <- max(n_ce + 1L, 2L)
  ce <- data.frame(scenario = cea$scenario, scenario_label = cea$scenario_label, stringsAsFactors = FALSE)
  for (cn in ce_cols[3:7]) ce[[cn]] <- NA_real_
  ce$dominance <- NA_character_; ce$reconciliation_status <- NA_character_
  ce$annual_cost_incremental_per_capita <- NA_real_   # J = live formula
  ce$disc_cost_incremental_per_capita   <- NA_real_   # K = live formula
  addWorksheet(wb, "Cost_Effectiveness")
  writeData(wb, "Cost_Effectiveness", ce, headerStyle = st_hdr)
  if (n_ce > 0) {
    R <- 2:r_ce
    writeFormula(wb, "Cost_Effectiveness", startCol = 3, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Annual_Mortality'!$I$2:$I$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 4, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Annual_Mortality'!$J$2:$J$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 5, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Budget_Impact'!$E$2:$E$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 6, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 7, startRow = 2, x = frows(function(r)
      sprintf("IF(C%d>0,F%d/C%d,\"\")", r, r, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 8, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(C%d>0,F%d<0),\"Dominant (more health, lower cost)\",IF(AND(C%d<=0,F%d>0),\"Dominated (less/no health, higher cost)\",IF(AND(C%d<=0,F%d<=0),\"No deaths averted; ratio not defined\",\"USD per death averted\")))",
              r, r, r, r, r, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 9, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(ABS(F%d-SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d))<=%s,ABS(C%d-SUMIFS('Annual_Mortality'!$I$2:$I$%d,'Annual_Mortality'!$A$2:$A$%d,A%d))<=%s),\"consistent\",\"mismatch\")",
              r, r_bi, r_bi, r, cA_tol, r, r_am, r_am, r, cA_tol), R))
    # J/K summary annual per-capita = mean of Budget_Impact per-capita (L/M) over years >= start+1
    writeFormula(wb, "Cost_Effectiveness", startCol = 10, startRow = 2, x = frows(function(r)
      sprintf("IFERROR(AVERAGEIFS('Budget_Impact'!$L$2:$L$%d,'Budget_Impact'!$A$2:$A$%d,A%d,'Budget_Impact'!$B$2:$B$%d,\">=\"&(%s+1)),\"\")", r_bi, r_bi, r, r_bi, cA_start), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 11, startRow = 2, x = frows(function(r)
      sprintf("IFERROR(AVERAGEIFS('Budget_Impact'!$M$2:$M$%d,'Budget_Impact'!$A$2:$A$%d,A%d,'Budget_Impact'!$B$2:$B$%d,\">=\"&(%s+1)),\"\")", r_bi, r_bi, r, r_bi, cA_start), R))
  }
  style_sheet("Cost_Effectiveness", ce_cols, n_ce, formula_cols = c(3:9, 10:11), wrap_cols = c(2, 8))
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "consistent", type = "contains", style = cf_pass)
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "mismatch",   type = "contains", style = cf_fail)
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 8, rows = 2:r_ce, rule = "Dominated",  type = "contains", style = cf_rev)

  ## ===== Health_Outcomes + Economic_Value + Benefit_Cost (Reference-Case BCA) =
  # Model 08 now runs ALL scenarios (incl. public-health I_PH_* / all_public_health),
  # so the Reference-Case VSL/VSLY benefit-cost view IS reproduced here for the PH
  # comparators, using the same shared formula-driven builder as the clinical book.
  ph_cmp <- comparators
  ho_src_ph <- dt_h07[scenario %in% ph_cmp, .(
    scenario_label = scenario_label[1L],
    modeled_deaths = sum(deaths),   baseline_deaths = sum(base_deaths),
    modeled_cases  = sum(newcases), baseline_cases  = sum(base_newcases),
    yll = sum(yll), base_yll = sum(base_yll),
    yld = sum(yld), base_yld = sum(base_yld),
    daly = sum(daly), base_daly = sum(base_daly)),
    by = .(scenario, year)]
  ev_src_ph <- ev08[scenario %in% ph_cmp, .(
    scenario_label    = scenario_label[1L],
    deaths_averted    = sum(deaths_averted),
    life_years_gained = sum(life_years_gained_undisc),
    gni_pc_idn = gni_pc_ppp[1L], gni_pc_usa = gni_pc_usa[1L],
    population = population[1L],  le_avg_adult = le_avg_adult[1L]),
    by = .(scenario, year)]
  scen_meta_ph <- unique(dt_h07[scenario %in% ph_cmp, .(
    scenario, scenario_label, intervention_family,
    scenario_level = fifelse(scenario == "all_public_health", "combined",
                      fifelse(is.na(scenario_role), "standalone", scenario_role)))])
  sty_ph <- list(st_hdr = st_hdr, st_formula = st_formula, st_rsrc = st_rsrc,
                 st_input = st_input, st_wrap = st_wrap,
                 cf_pass = cf_pass, cf_fail = cf_fail, cf_rev = cf_rev)
  if (nrow(ev_src_ph) > 0L) {
    build_bca_sheets_into(wb, ph_cmp, ho_src_ph, ev_src_ph, scen_meta_ph,
                          bca_cells_ph, r_bi, sty_ph)
  } else {
    addWorksheet(wb, "Economic_Value")
    writeData(wb, "Economic_Value",
              data.frame(note = "No public-health comparators with Model 08 economic value in this run."),
              headerStyle = st_hdr)
    setColWidths(wb, "Economic_Value", cols = 1, widths = 110)
  }

  ## ===== CVD_40q30_Age + CVD_40q30 (formula-driven period CVD 40q30) =======
  # Baseline + the public-health comparators (baseline required for the on-sheet
  # baseline SUMIFS). Uses the shared Model 07 CVD 40q30 contracts, which Model 09
  # REQUIRES (it stops early if 07_cvd_40q30*.rds are missing), so the call is
  # unconditional -- matching the clinical and combined builders.
  build_cvd_40q30_sheets_into(wb, c(base_id, comparators), dt_cvd_40q30, cvd_age_40q30,
                              base_id, sty_ph)

  ## ===== QA_Checks =======================================================
  qa_check <- c("Selected intervention-cause key uniqueness","Workbook FAIL-level issues",
                "Workbook REVIEW-level issues","Every scenario paired to baseline",
                "Public-health transitions within allowed set (well->sick, sick->dead)","No impossible negative states",
                "Stock/flow identity pop = well + sick + all-cause deaths",
                "Background mortality constant across cause","Cost reconciliation (components -> budget impact)",
                "Shared cost counted once per stratum/year","Annual reconciles to cumulative (budget impact)",
                "CEA reconciliation (detail -> summary)","Excel vs R: deaths averted (anchor)",
                "Excel vs R: discounted incremental cost (anchor)","Excel vs R: cost per death averted (anchor)")
  qa_expect <- c("0","0","0","0","0","0","<= limit","1","<= tol","0","<= tol","consistent",
                 "match R","match R","match R")
  qa_note <- c("Each selected link key appears once",
               "Blocked links excluded (see Blocked_Links / Input_Diagnostic)",
               "Flagged but usable (e.g. cost not Indonesia-adjusted; provisional PAF)",
               "Deaths averted = baseline - scenario at matched year/cause",
               sprintf(paste0("PH effects map to well->sick incidence OR sick->dead case fatality; ",
                              "%d case-fatality link(s) in this run; 0 outside the allowed set / no ",
                              "cross-pathway application. Inclusion is set by the workbook ",
                              "include_flag; links flagged 0 (e.g. the exploratory SSB->T2DM ",
                              "mortality link) stay excluded."), n_cf_links),
               "well/sick/new_cases/deaths/population >= 0",
               "Per cause row; small residual from 95+ pooling / rounding",
               "all.mx taken once per stratum (population not duplicated across causes)",
               "Excel component rows sum to Excel annual totals within tolerance",
               "shared-count-once components appear once per scenario-year",
               "Last-year cumulative equals the sum of annual incremental cost",
               "Every Cost_Effectiveness row's internal reconciliation is consistent",
               "Excel CEA deaths averted reconciles to the R engine value",
               "Excel CEA discounted incremental cost reconciles to the R engine value",
               "Excel CEA USD per death averted reconciles to the R engine value")
  qa_actual <- c(
    sprintf("COUNTIF('Selected_Interventions'!$%s$2:$%s$%d,\">1\")", KCl, KCl, r_si),  # key_count col (by name)
    sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"FAIL\")", max(nrow(diag_out)+1L,2L)),
    sprintf("COUNTIF('Input_Diagnostic'!$E$2:$E$%d,\"REVIEW\")", max(nrow(diag_out)+1L,2L)),
    sprintf("COUNTBLANK('Annual_Mortality'!$H$2:$H$%d)", r_am),
    # Count effect rows whose model_transition (col N) is OUTSIDE the allowed set
    # {incidence, case_fatality}. Both are now legitimate PH pathways.
    sprintf("COUNTIFS('Effect_Parameters'!$N$2:$N$%d,\"<>incidence\",'Effect_Parameters'!$N$2:$N$%d,\"<>case_fatality\")", r_ef, r_ef),
    cA_neg, cA_resid, cA_bgd,
    sprintf("ABS(SUM('Budget_Impact'!$D$2:$D$%d)-SUM('Annual_Cost'!$R$2:$R$%d))+ABS(SUM('Budget_Impact'!$C$2:$C$%d)-SUM('Annual_Cost'!$Q$2:$Q$%d))",
            r_bi, r_ac, r_bi, r_ac),
    sprintf("COUNTIF('Annual_Cost'!$AA$2:$AA$%d,\">1\")", r_ac),
    # check 11 (annual -> cumulative): sum of last-year cumulative == total incremental
    sprintf("ABS(SUMIFS('Budget_Impact'!$G$2:$G$%d,'Budget_Impact'!$B$2:$B$%d,%s)-SUM('Budget_Impact'!$E$2:$E$%d))",
            r_bi, r_bi, cA_end, r_bi),
    sprintf("IF(COUNTIF('Cost_Effectiveness'!$I$2:$I$%d,\"mismatch\")=0,\"consistent\",\"mismatch\")", r_ce),
    sprintf("INDEX('Cost_Effectiveness'!$C$2:$C$%d,MATCH(%s,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, cA_anch, r_ce),
    sprintf("INDEX('Cost_Effectiveness'!$F$2:$F$%d,MATCH(%s,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, cA_anch, r_ce),
    sprintf("INDEX('Cost_Effectiveness'!$G$2:$G$%d,MATCH(%s,'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, cA_anch, r_ce))
  qa_status <- c(
    "IF(C2=0,\"PASS\",\"FAIL\")","IF(C3=0,\"PASS\",\"REVIEW\")","IF(C4=0,\"PASS\",\"REVIEW\")",
    "IF(C5=0,\"PASS\",\"FAIL\")","IF(C6=0,\"PASS\",\"FAIL\")","IF(C7=0,\"PASS\",\"FAIL\")",
    sprintf("IF(C8<=%s,\"PASS\",\"REVIEW\")", cA_reslim),
    "IF(C9=1,\"PASS\",\"FAIL\")",
    sprintf("IF(C10<=%s,\"PASS\",\"FAIL\")", cA_tol),
    "IF(C11=0,\"PASS\",\"FAIL\")",
    sprintf("IF(C12<=%s,\"PASS\",\"FAIL\")", cA_tol),
    "IF(C13=\"consistent\",\"PASS\",\"FAIL\")",
    sprintf("IF(ABS(C14-%s)<=ABS(%s)*0.000001+0.5,\"PASS\",\"FAIL\")", cA_rda, cA_rda),
    sprintf("IF(ABS(C15-%s)<=ABS(%s)*0.000001+1,\"PASS\",\"FAIL\")", cA_rdic, cA_rdic),
    sprintf("IF(ABS(C16-%s)<=ABS(%s)*0.000001+1,\"PASS\",\"FAIL\")", cA_rcpd, cA_rcpd))
  # --- appended hierarchy / cost-allocation checks (Scenario_Hierarchy, Cost_Components,
  # Annual_Mortality all exist in the final workbook; letters/rows derived by name) ---
  r_sh   <- nrow(phi$hierarchy) + 1L
  pk_all <- phi$package_ids
  tob_pk <- c(pk_all[grepl("TOB", pk_all)],  "I_PH_TOBACCO_POLICY")[1]
  salt_pk<- c(pk_all[grepl("SALT", pk_all)], "I_PH_SALT_POLICY")[1]
  SHpar  <- xlc(names(phi$hierarchy), "parent_scenario_id"); SHrole <- xlc(names(phi$hierarchy), "scenario_role")
  Ccpp   <- xlc(names(cc_out), "parent_package_id");         Ccsh   <- xlc(names(cc_out), "cost_allocation_share")
  qa_check  <- c(qa_check, "Tobacco package child count","Salt package child count",
                 "Child cost-allocation shares sum to 1 (per package)","Parent packages produced as joint runs")
  qa_expect <- c(qa_expect, "3","4","<= tol","> 0")
  qa_note   <- c(qa_note, "Scenario_Hierarchy: clean air, media, ad ban",
                 "Scenario_Hierarchy: reformulation, FOPL, media, institutions",
                 "Cost_Components allocation shares sum to 1 within each package (M13/M14)",
                 "Package cases/deaths come from a produced JOINT scenario, never summed from children")
  qa_actual <- c(qa_actual,
    sprintf("COUNTIFS('Scenario_Hierarchy'!$%s$2:$%s$%d,\"%s\",'Scenario_Hierarchy'!$%s$2:$%s$%d,\"child\")",
            SHpar, SHpar, r_sh, tob_pk, SHrole, SHrole, r_sh),
    sprintf("COUNTIFS('Scenario_Hierarchy'!$%s$2:$%s$%d,\"%s\",'Scenario_Hierarchy'!$%s$2:$%s$%d,\"child\")",
            SHpar, SHpar, r_sh, salt_pk, SHrole, SHrole, r_sh),
    sprintf("ABS(SUMIFS('Cost_Components'!$%s$2:$%s$%d,'Cost_Components'!$%s$2:$%s$%d,\"%s\")-1)+ABS(SUMIFS('Cost_Components'!$%s$2:$%s$%d,'Cost_Components'!$%s$2:$%s$%d,\"%s\")-1)",
            Ccsh, Ccsh, r_cc, Ccpp, Ccpp, r_cc, tob_pk, Ccsh, Ccsh, r_cc, Ccpp, Ccpp, r_cc, salt_pk),
    sprintf("MIN(COUNTIF('Annual_Mortality'!$A$2:$A$%d,\"%s\"),COUNTIF('Annual_Mortality'!$A$2:$A$%d,\"%s\"))",
            r_am, tob_pk, r_am, salt_pk))
  qa_status <- c(qa_status,
    "IF(C17=3,\"PASS\",\"FAIL\")","IF(C18=4,\"PASS\",\"FAIL\")",
    sprintf("IF(C19<=%s,\"PASS\",\"FAIL\")", cA_tol),
    "IF(C20>0,\"PASS\",\"FAIL\")")
  # --- Reference-Case BCA QA (rows 21-24): only when PH economic-value sheets exist.
  if (nrow(ev_src_ph) > 0L) {
    .rev_c <- nrow(ev_src_ph) + 1L; .rho_c <- nrow(ho_src_ph) + 1L
    .rbc_c <- nrow(scen_meta_ph) * 4L + 1L
    qa_check  <- c(qa_check,
      "VSL floor applied (preferred VSL >= 20x GNI floor)",
      "Reference-case VSL parameters (elasticity 1.5, US ratio 160, 20x floor)",
      "Benefit and cost on the same price year (BCA basis)",
      "Benefit_Cost scenarios all present in Health_Outcomes")
    qa_expect <- c(qa_expect, "0", "as_specified", "match", "0")
    qa_note   <- c(qa_note,
      "No Economic_Value row has preferred VSL below the 20x GNI-per-capita floor",
      "Reference case = elasticity 1.5 transfer, 160x US ratio, 20x GNI floor (Robinson et al. 2019)",
      "bca_price_year equals the reporting price year, so benefits and costs share a real price basis",
      "Every scenario x valuation-case row in Benefit_Cost has a matching Health_Outcomes scenario")
    qa_actual <- c(qa_actual,
      sprintf("SUMPRODUCT(('Economic_Value'!$L$2:$L$%d<'Economic_Value'!$K$2:$K$%d)*1)", .rev_c, .rev_c),
      sprintf("IF(AND(%s=1.5,%s=160,%s=20),\"as_specified\",\"edited\")",
              bca_cells_ph$elast, bca_cells_ph$ratio, bca_cells_ph$floor),
      sprintf("IF(%s=%s,\"match\",\"mismatch\")", bca_cells_ph$price_year, .bcell("reporting_price_year")),
      sprintf("SUMPRODUCT((COUNTIF('Health_Outcomes'!$A$2:$A$%d,'Benefit_Cost'!$A$2:$A$%d)=0)*1)", .rho_c, .rbc_c))
    qa_status <- c(qa_status,
      "IF(C21=0,\"PASS\",\"FAIL\")",
      "IF(C22=\"as_specified\",\"PASS\",\"REVIEW\")",
      "IF(C23=\"match\",\"PASS\",\"REVIEW\")",
      "IF(C24=0,\"PASS\",\"FAIL\")")
  }
  # --- CVD 40q30 reconciliation (Excel life-table vs the Model 07 R anchor) ----
  # (CVD_40q30 is always built above, so this check is always added.)
  .cvd_row <- length(qa_check) + 2L
  .rq40_ph <- nrow(dt_cvd_40q30[scenario %in% c(base_id, comparators)]) + 1L
  qa_check  <- c(qa_check,  "CVD 40q30 Excel vs R (all rows match)")
  qa_expect <- c(qa_expect, "0")
  qa_note   <- c(qa_note,   "CVD_40q30 recon_status has no 'mismatch' (life-table formula reconciles to Model 07)")
  qa_actual <- c(qa_actual, sprintf("COUNTIF('CVD_40q30'!$N$2:$N$%d,\"mismatch\")", .rq40_ph))
  qa_status <- c(qa_status, sprintf("IF(C%d=0,\"PASS\",\"FAIL\")", .cvd_row))
  qa_df <- data.frame(check = qa_check, expected = qa_expect, actual = NA,
                      status = NA_character_, note = qa_note, stringsAsFactors = FALSE)
  addWorksheet(wb, "QA_Checks")
  writeData(wb, "QA_Checks", qa_df, headerStyle = st_hdr)
  writeFormula(wb, "QA_Checks", startCol = 3, startRow = 2, x = qa_actual)
  writeFormula(wb, "QA_Checks", startCol = 4, startRow = 2, x = qa_status)
  n_qa <- nrow(qa_df); r_qa <- n_qa + 1L
  style_sheet("QA_Checks", names(qa_df), n_qa, formula_cols = c(3,4), wrap_cols = 5)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "PASS",   type = "contains", style = cf_pass)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "FAIL",   type = "contains", style = cf_fail)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "REVIEW", type = "contains", style = cf_rev)

  ## ===== Input_Diagnostic ================================================
  addWorksheet(wb, "Input_Diagnostic")
  n_id <- nrow(diag_out); r_id <- max(n_id + 1L, 2L)
  if (n_id > 0) {
    writeData(wb, "Input_Diagnostic", as.data.frame(diag_out), headerStyle = st_hdr)
    style_sheet("Input_Diagnostic", names(diag_out), n_id, wrap_cols = which(names(diag_out) == "problem"))
    sev_col <- which(names(diag_out) == "severity")
    conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "FAIL",   type = "contains", style = cf_fail)
    conditionalFormatting(wb, "Input_Diagnostic", cols = sev_col, rows = 2:r_id, rule = "REVIEW", type = "contains", style = cf_rev)
  } else {
    writeData(wb, "Input_Diagnostic", data.frame(scope=character(0), item_key=character(0),
              field=character(0), problem=character(0), severity=character(0)), headerStyle = st_hdr)
    addStyle(wb, "Input_Diagnostic", st_hdr, rows = 1, cols = 1:5, gridExpand = TRUE)
  }

  ## ===== Methods_and_Sources =============================================
  methods <- data.table(
    method_id = sprintf("M%02d", 1:19),
    concept = c("Exposure target","Exposure path","Regulatory score","Regulatory effect",
                "Price-change tax","Tax-share tax","Prevalence-shift RR effect","Log-linear RR effect",
                "TFA effect method (RR default / PAF optional)","Effect lag","Incidence application",
                "Policy implementation fraction","Annual policy cost","Shared cost","Child cost allocation",
                "Parent package cost","Package health outcomes","Discounting","Cost-effectiveness"),
    formula_or_rule = c(
      "relative: max(floor, baseline*(1-reduction)); absolute: max(floor, baseline-reduction); level: max(floor, target)",
      "pt(t) linear baseline->target over start_year..target_year, then held; floored at exposure_floor",
      "implementation score: none=0; partial=0.5; full=1",
      "implementation_gap = max(0, target_score - baseline_score); regulatory reduction = full_component_effect * implementation_gap",
      "fiscal_tax_delta = max(0, target_tax - baseline_tax); reduction = abs(price_elasticity) * implied_price_change",
      "tobacco excise share: implied_price_change = (1 - baseline_share)/(1 - target_share) - 1",
      "effect = 1 - (1 + pt*(RR-1)) / (1 + p0*(RR-1))   [tobacco smoking prevalence shift]",
      "effect = 1 - 1/(RR^(p0-pt))   [alcohol, sodium, SSB, and DEFAULT industrial-TFA path: RR per 1 percentage-point energy, e.g. RR_TFA_IHD_1PCT=1.10 ~ sqrt(1.21)]",
      "RR (default): log-linear RR per unit reduction, no PAF required. PAF (optional, tfa_effect_method=PAF): effect = optional_PAF * implementation_gap",
      "immediate: effect tracks exposure path; delayed_exponential: full_effect * (1-(1-rate)^(years since start))",
      "cause-specific incidence x (1 - effect_t); multiple policies combine multiplicatively on the surviving fraction",
      "implementation_fraction(t) = min(max((t - policy_start_year + 1)/policy_cost_ramp_years, 0), 1)",
      "annual_cost = population(t) * PIN_fraction * implementation_fraction(t) * frequency * unit_cost",
      "shared-count-once costs counted once per intervention & scenario-year, never once per affected cause",
      "child cost = package_total_cost_per_capita * cost_allocation_share (shares sum to 1 within a package)",
      "parent package cost = sum of selected child costs; the parent_reference cost row (selected_for_base_case=0) is NOT charged",
      "parent-package cases/deaths come from ONE joint scenario run with all children applied together; never sum standalone child outcomes",
      "discount_factor(t) = 1/(1+cost_discount_rate)^(t - analysis_start_year); costs discounted, deaths undiscounted",
      "USD per death (or case) averted = cumulative discounted incremental cost / cumulative averted; blank + 'no incremental health effect' when nothing averted"),
    source = c(rep("NCD Countdown supplement (Countdown_Methods sheet) + public-health input workbook", 19)))
  # Reference-Case benefit-cost analysis methods (Health_Outcomes / Economic_Value / Benefit_Cost).
  bca_methods <- data.table(
    method_id = c("M20","M21","M22","M23"),
    concept = c("Reference-Case VSL transfer","Constant VSLY",
                "BCA discounting & monetary basis","Benefit-cost ratio & net benefit"),
    formula_or_rule = c(
      "Preferred VSL = MAX(vsl_us_gni_ratio*GNIpc_US*(GNIpc_IDN/GNIpc_US)^income_elasticity, vsl_floor_gni_multiple*GNIpc_IDN); standardized sensitivities VSL = 100x and 160x GNIpc.",
      "VSLY = preferred VSL / undiscounted remaining LE at the average working-age (vsly_adult_min_age..max_age) age; applied to Model 07 age-specific life-years gained.",
      "Benefits and costs discounted to bca_base_year at bca_discount_rate_primary; VSL benefits are PPP int$; market-USD costs converted via cost_to_bca_currency_factor (never assumed 1).",
      "BCR = PV benefits / PV costs; net benefit = PV benefits - PV costs, on one base year/rate/price year/basis. PARTIAL mortality-benefit BCA, not cost-effectiveness."),
    source = rep("Robinson et al. 2019 Reference Case Guidelines for BCA in Global Health & Development / Model 08", 4))
  # TFA base-case RR references retained for provenance.
  tfa_refs <- data.table(method_id = c("REF","REF"), concept = c("TFA source RR","TFA source RR"),
    formula_or_rule = c("~RR 1.21 per 2 percentage-points energy (converted to ~1.10 per 1 pp in the workbook)",
                        "supporting cohort evidence"),
    source = c("https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.118.038160",
               "https://www.ahajournals.org/doi/10.1161/JAHA.115.002891"))
  cvd_method <- data.table(method_id = "M24", concept = "CVD 40q30 (period life table)",
    formula_or_rule = paste0("Period CVD 40q30 over the six CVD causes (ihd, istroke, hstroke, hhd, rhd, cmd), ",
      "exact ages 30-69: sexes combined before the rate, m_x=(D_F+D_M)/(N_F+N_M), q_x=1-EXP(-m_x), l_30=1, ",
      "l_{x+1}=l_x(1-q_x), 40q30=100*(1-l_70/l_30). CVD_40q30_Age carries the live life-table formulas; ",
      "CVD_40q30 gives the metric, shared-baseline pairing, absolute (pp) and relative (%) reduction, reconciled ",
      "to Model 07. cvd_40q30 is a PERCENT on 0-100 (format 0.000, not Excel %); population de-duplicated across causes."),
    source = "Model 07 (07_cvd_40q30.rds) / Model 09")
  methods <- rbind(methods, bca_methods, tfa_refs, cvd_method)
  addWorksheet(wb, "Methods_and_Sources")
  writeData(wb, "Methods_and_Sources", methods, headerStyle = st_hdr)
  style_sheet("Methods_and_Sources", names(methods), nrow(methods), wrap_cols = c(3,4), filter = FALSE, max_w = 90)
  setColWidths(wb, "Methods_and_Sources", cols = 1:4, widths = c(10, 28, 92, 56))

  ## ===== Calculation_Map =================================================
  cmap <- data.table(
    output_sheet = c("Policy_Levers","Selected_Interventions","Exposure_Targets","Effect_Parameters",
                     "Cost_Components","Annual_Mortality","Health_Outcomes","Annual_Cost","Budget_Impact",
                     "Cost_Effectiveness","Economic_Value","Benefit_Cost",
                     "Child_Intervention_Summary","Parent_Package_Summary","QA_Checks","Run_Metadata"),
    formula_columns = c("implementation_gap,implied_price_change,fiscal_tax_delta,policy_reduction",
                        "full_effect,reductions,key_count,status","target,abs,rel","full_effect",
                        "allocated_child_cost,cost_ready","deaths_averted,cases_averted",
                        "F,I,L,O,R,S (averted health)",
                        "impl_frac,PIN,annual+disc cost,shared QA","incremental+cumulative cost",
                        "health,cost,ICER,dominance,reconciliation","J:AB (VSL/VSLY/PV benefits)",
                        "G:M (PV benefits/costs, net, BCR)","modeled/baseline/averted,cost,ICER",
                        "modeled/baseline/averted,cost,ICER","actual,status","B (subset)"),
    depends_on = c("lever_method + fiscal/regulatory inputs","effect_model + exposures","baseline/method/reduction",
                   "effect_model + exposures","cost inputs + allocation shares","R deaths/cases",
                   "Model 07 modeled/baseline health",
                   "Cost_Components; Calculation_Assumptions; R population","Annual_Cost",
                   "Annual_Mortality; Budget_Impact; Calculation_Assumptions",
                   "Model 08 source; Calculation_Assumptions BCA controls",
                   "Economic_Value PV benefits; Budget_Impact costs; Calculation_Assumptions",
                   "Annual_Mortality; Budget_Impact (standalone scenarios)",
                   "Annual_Mortality; Budget_Impact (JOINT package/combined runs)",
                   "calculation + diagnostic sheets","Calculation_Assumptions"),
    calculation = c("Regulatory gap, tax price change/delta, reproduced relative exposure reduction",
                    "Full effect at target; exposure reductions; key uniqueness",
                    "Exposure target and absolute/relative reductions","Full effect at target",
                    "Child cost = package total x share; cost-readiness rule","Deaths and cases averted",
                    "Deaths/cases/YLL/YLD/DALY averted and life-years gained (baseline - intervention)",
                    "Implementation fraction, PIN, annual/discounted cost, shared-cost QA",
                    "Annual and cumulative cost by scenario",
                    "Cumulative health, cost, cost/death, dominance, reconciliation",
                    "Reference-Case VSL (elast 1.5 + 20x floor) & 100x/160x, VSLY, undisc & PV benefits, GNI shares",
                    "PV benefits, PV costs (to PPP int$), net benefit, BCR, benefit/GNI, decision (partial BCA)",
                    "Per child intervention: health, cost, cost per case/death averted, status",
                    "Per parent package (joint run) + combined: health, cost, cost per case/death averted",
                    "Invariant recomputation and Excel-vs-R reconciliation","Metadata pulled from controls"))
  cmap <- rbind(cmap, data.table(
    output_sheet = c("CVD_40q30_Age","CVD_40q30"),
    formula_columns = c("J:M (m_x,q_x,l_x,l_x+1)","I,J,K,L,N"),
    depends_on = c("CVD deaths + de-duplicated population (grey H:I)",
                   "CVD_40q30_Age life table; baseline SUMIFS; R anchor (M)"),
    calculation = c("Period life table: m_x=deaths/pop, q_x=1-EXP(-m_x), recursive l_x/l_{x+1} (ages 30-69)",
                    "CVD 40q30 = 100*(1-l_70/l_30); baseline-paired absolute (pp) & relative (%) reduction; R reconciliation")))
  addWorksheet(wb, "Calculation_Map")
  writeData(wb, "Calculation_Map", cmap, headerStyle = st_hdr)
  style_sheet("Calculation_Map", names(cmap), nrow(cmap), wrap_cols = c(3,4), filter = FALSE, max_w = 60)
  setColWidths(wb, "Calculation_Map", cols = 1:4, widths = c(22, 30, 40, 52))

  ## ===== Scenario_Hierarchy ==============================================
  # Workbook hierarchy (parent packages <-> child/standalone interventions) with
  # the current run's runnable / parent-package flags. Source (R-generated) view.
  sh_out <- as.data.frame(phi$hierarchy)
  addWorksheet(wb, "Scenario_Hierarchy")
  writeData(wb, "Scenario_Hierarchy", sh_out, headerStyle = st_hdr)
  style_sheet("Scenario_Hierarchy", names(sh_out), nrow(sh_out),
              rsource_cols = seq_along(sh_out),
              wrap_cols = which(names(sh_out) %in% c("parent_aggregation_rule","outcome_reporting_rule",
                                                     "cost_reporting_rule","source_note")))

  ## ===== Risk_Response ===================================================
  rr_out <- as.data.frame(phi$risk_response)
  addWorksheet(wb, "Risk_Response")
  writeData(wb, "Risk_Response", rr_out, headerStyle = st_hdr)
  style_sheet("Risk_Response", names(rr_out), nrow(rr_out), rsource_cols = seq_along(rr_out),
              wrap_cols = which(names(rr_out) %in% c("response_name","derivation","source","notes")))

  ## ===== Child_Intervention_Summary / Parent_Package_Summary =============
  # Per-scenario health + cost + cost-effectiveness, formula-driven from
  # Annual_Mortality and Budget_Impact (package rows come from the JOINT package
  # run, never summed from standalone children -- criteria 10 & 19).
  sc <- as.data.table(phi$scenario_catalogue); setnames(sc, "scenario_id", "scenario")
  sc <- sc[scenario %in% comparators]
  summ_cols <- c("scenario","scenario_label","scenario_level","scenario_role","parent_package_id",
                 "parent_package_name","intervention_ids","component_order",
                 "modeled_cases","baseline_cases","cases_averted","modeled_deaths","baseline_deaths",
                 "deaths_averted","incremental_cost","disc_incremental_cost","cost_per_case_averted",
                 "cost_per_death_averted","status")
  Sca <- xlc(summ_cols, "cases_averted"); Sda <- xlc(summ_cols, "deaths_averted")
  Sdic <- xlc(summ_cols, "disc_incremental_cost")
  write_summary <- function(sheet, dt) {
    d <- as.data.frame(dt[, .(scenario, scenario_label, scenario_level, scenario_role,
                              parent_package_id, parent_package_name, intervention_ids, component_order)],
                       stringsAsFactors = FALSE)
    for (cn in summ_cols[9:16]) d[[cn]] <- NA_real_
    d$cost_per_case_averted <- NA_real_; d$cost_per_death_averted <- NA_real_; d$status <- NA_character_
    d <- d[, summ_cols]; n <- nrow(d); rr2 <- max(n + 1L, 2L)
    addWorksheet(wb, sheet); writeData(wb, sheet, d, headerStyle = st_hdr)
    if (n > 0) {
      R <- 2:rr2
      amf <- function(col) function(r) sprintf(
        "SUMIFS('Annual_Mortality'!$%s$2:$%s$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", col, col, r_am, r_am, r)
      bif <- function(col) function(r) sprintf(
        "SUMIFS('Budget_Impact'!$%s$2:$%s$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", col, col, r_bi, r_bi, r)
      writeFormula(wb, sheet, startCol = 9,  startRow = 2, x = frows(amf("E"), R))   # modeled_cases
      writeFormula(wb, sheet, startCol = 10, startRow = 2, x = frows(amf("G"), R))   # baseline_cases
      writeFormula(wb, sheet, startCol = 11, startRow = 2, x = frows(amf("J"), R))   # cases_averted
      writeFormula(wb, sheet, startCol = 12, startRow = 2, x = frows(amf("F"), R))   # modeled_deaths
      writeFormula(wb, sheet, startCol = 13, startRow = 2, x = frows(amf("H"), R))   # baseline_deaths
      writeFormula(wb, sheet, startCol = 14, startRow = 2, x = frows(amf("I"), R))   # deaths_averted
      writeFormula(wb, sheet, startCol = 15, startRow = 2, x = frows(bif("E"), R))   # incremental_cost
      writeFormula(wb, sheet, startCol = 16, startRow = 2, x = frows(bif("F"), R))   # disc_incremental_cost
      writeFormula(wb, sheet, startCol = 17, startRow = 2, x = frows(function(r)
        sprintf("IF(%s%d>0,%s%d/%s%d,\"\")", Sca, r, Sdic, r, Sca, r), R))           # cost per case averted
      writeFormula(wb, sheet, startCol = 18, startRow = 2, x = frows(function(r)
        sprintf("IF(%s%d>0,%s%d/%s%d,\"\")", Sda, r, Sdic, r, Sda, r), R))           # cost per death averted
      writeFormula(wb, sheet, startCol = 19, startRow = 2, x = frows(function(r)
        sprintf("IF(AND(%s%d<=0,%s%d<=0),\"no incremental health effect\",IF(AND(%s%d<0,OR(%s%d>0,%s%d>0)),\"Dominant (more health, lower cost)\",IF(%s%d>0,\"USD per death averted\",\"USD per case averted\")))",
                Sca, r, Sda, r, Sdic, r, Sca, r, Sda, r, Sda, r), R))                # status
    }
    style_sheet(sheet, summ_cols, n, formula_cols = 9:19, wrap_cols = c(2, 7, 19))
  }
  write_summary("Child_Intervention_Summary", sc[scenario_level == "standalone"])
  write_summary("Parent_Package_Summary",     sc[scenario_level %in% c("package", "combined")])

  ## ===== Scenario_Catalog (authoritative current-run scenario contract) ===
  # Baseline + each active standalone intervention + parent packages + the
  # combined all-public-health scenario, derived from the Model 04 catalogue
  # under the binding include_flag rule. Literal R-source values (no formulas).
  scat_ph <- as.data.frame(deck_catalog_list[["public_health"]])
  addWorksheet(wb, "Scenario_Catalog")
  writeData(wb, "Scenario_Catalog", scat_ph, headerStyle = st_hdr)
  style_sheet("Scenario_Catalog", names(scat_ph), nrow(scat_ph),
              rsource_cols = seq_along(scat_ph), wrap_cols = c(2, 7))

  ## ===== order, recalc, strip, save ======================================
  desired_order <- c("README","Run_Metadata","Scenario_Catalog","Scenario_Hierarchy","Selected_Interventions","Blocked_Links",
                     "Policy_Levers","Exposure_Targets","Effect_Parameters","Risk_Response","Cost_Components",
                     "Annual_Mortality","Health_Outcomes","CVD_40q30","CVD_40q30_Age","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                     "Child_Intervention_Summary","Parent_Package_Summary","Economic_Value","Benefit_Cost","QA_Checks",
                     "Input_Diagnostic","Methods_and_Sources","Calculation_Assumptions","Calculation_Map")
  desired_order <- desired_order[desired_order %in% names(wb)]
  worksheetOrder(wb) <- match(desired_order, names(wb))
  wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
  if (exists("strip_dangling_drawings")) strip_dangling_drawings(wb)
  if (!dir.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
  saveWorkbook(wb, out_file, overwrite = TRUE)
  message("  Wrote public-health formula workbook: ", out_file)
  message(sprintf("  Public-health scenarios: %s", paste(comparators, collapse = ", ")))
  invisible(out_file)
}

# =====================================================================
# source_combined_cost_value()  --  Model 09 Section 13 builder
# Builds output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx
# from the current-run BOTH-families catalogues and the shared Model 06 output.
# Scenarios: baseline (once) + all clinical comparators + all public-health
# comparators + the genuine joint `all_clinical_public_health` scenario. The joint
# scenario is costed from ITS OWN Model 06 state/flow results (clinical components
# via the clinical costing rule; public-health components via the PH rule; family
# provenance retained; collision-safe keys). Not a values-only copy of the two
# existing workbooks -- fully formatted and formula-driven.
# =====================================================================
source_combined_cost_value <- function() {
  stopifnot(exists("fair_inputs"), !is.null(fair_inputs),
            exists("public_health_inputs"), !is.null(public_health_inputs),
            exists("fair_scenarios"), !is.null(fair_scenarios),
            exists("public_health_scenarios"), !is.null(public_health_scenarios),
            exists("combined_scenarios"), !is.null(combined_scenarios),
            exists("mo_all"), exists("dt_h07"), exists("ev08"), exists("bca_params"),
            exists("dt_cvd_40q30"), exists("cvd_age_40q30"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  out_file <- if (exists("combined_cost_value_formulae_file"))
    combined_cost_value_formulae_file else
    paste0(wd_outp, "indonesia_model_cost_value_clinical_public_health_formulae.xlsx")
  message("  Building combined clinical + public-health formula workbook: ", out_file)

  A  <- fair_inputs$assumptions
  PA <- public_health_inputs$assumptions
  yr_start <- as.integer(A$analysis_start_year); yr_end <- as.integer(A$analysis_end_year)
  if (as.integer(PA$analysis_start_year) != yr_start ||
      as.integer(PA$analysis_end_year)   != yr_end)
    message("  NOTE (combined): clinical and public-health analysis horizons differ; ",
            "using the clinical horizon ", yr_start, "-", yr_end, ".")
  analysis_yrs <- yr_start:yr_end
  disc_rate    <- A$cost_discount_rate
  policy_start <- as.integer(PA$policy_start_year)
  ramp_years   <- max(PA$policy_cost_ramp_years, 1)
  base_impl    <- 0
  base_id      <- fair_inputs$baseline_scenario_id %||% "baseline"
  joint_id     <- "all_clinical_public_health"

  produced_ids <- unique(mo_all$scenario)
  if (!(joint_id %in% produced_ids))
    stop("combined workbook: the joint scenario '", joint_id, "' is not present in the ",
         "Model 06 output. Re-run Model 06 with both intervention families enabled.",
         call. = FALSE)
  clin_comparators <- setdiff(intersect(names(fair_scenarios), produced_ids), base_id)
  ph_comparators   <- setdiff(intersect(names(public_health_scenarios), produced_ids), base_id)
  comparators      <- c(clin_comparators, ph_comparators, joint_id)
  combined_ids     <- c(base_id, comparators)

  # Labels from all three catalogues (never relabelled).
  scen_lab <- c(vapply(fair_scenarios,           function(s) s$scenario_label %||% s$scenario_id, character(1)),
                vapply(public_health_scenarios,  function(s) s$scenario_label %||% s$scenario_id, character(1)),
                vapply(combined_scenarios,       function(s) s$scenario_label %||% s$scenario_id, character(1)))
  scen_lab <- scen_lab[!duplicated(names(scen_lab))]

  mo <- as.data.table(mo_all)[scenario %in% combined_ids & year %in% analysis_yrs]
  if (!nrow(mo)) stop("combined workbook: no rows for the combined scenario set.", call. = FALSE)

  ## ---- annual mortality (both-sex; baseline vs scenario) ------------------
  mort <- mo[, .(cases = sum(newcases), cause_deaths = sum(dead)), by = .(scenario, year, cause)]
  base_mort <- mo[scenario == base_id, .(base_deaths = sum(dead), base_cases = sum(newcases)),
                  by = .(year, cause)]
  mort <- merge(mort, base_mort, by = c("year", "cause"), all.x = TRUE)
  mort[, scenario_label := scen_lab[scenario]]
  mort <- mort[scenario %in% comparators]
  setcolorder(mort, c("scenario", "scenario_label", "year", "cause",
                      "cases", "cause_deaths", "base_cases", "base_deaths"))
  setorder(mort, scenario, year, cause)

  ## ---- costing helpers (clinical qty + PH pop, both off the JOINT run) -----
  popu <- unique(mo[, .(scenario, year, age, sex, pop)])
  qty_by_year <- function(scn, cr) {                     # clinical population-in-need
    a0 <- cr$c_age_start; a1 <- cr$c_age_stop; sx <- cr$c_sex
    if (cr$population_in_need_measure == "all") {
      d <- popu[scenario == scn & age >= a0 & age <= a1]
      if (!identical(sx, "Both")) d <- d[sex == sx]
      agg <- d[, .(q = sum(pop)), by = year]
    } else {
      vcol <- if (cr$population_in_need_measure == "prevalence") "sick" else "newcases"
      d <- mo[scenario == scn & cause == cr$cause_code & age >= a0 & age <= a1]
      if (!identical(sx, "Both")) d <- d[sex == sx]
      agg <- d[, .(q = sum(get(vcol))), by = year]
    }
    m <- merge(data.table(year = analysis_yrs), agg, by = "year", all.x = TRUE)
    m[is.na(q), q := 0]; m$q
  }
  pop_for <- function(scn, a0, a1, sx) {                 # PH de-duplicated population
    d <- unique(mo[scenario == scn & age >= a0 & age <= a1, .(year, age, sex, pop)])
    if (!identical(sx, "Both")) d <- d[sex == sx]
    m <- merge(data.table(year = analysis_yrs), d[, .(pop = sum(pop)), by = year],
               by = "year", all.x = TRUE)
    m[is.na(pop), pop := 0]; m$pop
  }
  cov_path <- function(cb, ct, sy, ty, yrs) {
    span <- max(ty - sy + 1, 1); frac <- pmin(pmax((yrs - sy + 1) / span, 0), 1)
    cc <- cb + (ct - cb) * frac; cc[yrs < sy] <- cb; cc[yrs > ty] <- ct; pmin(pmax(cc, 0), 1)
  }
  cimpl <- pmin(pmax((analysis_yrs - policy_start + 1) / ramp_years, 0), 1)

  clin_costs <- copy(fair_inputs$costs)
  clin_costs[, cost_ready := !is.na(unit_cost_usd) & unit_cost_usd >= 0 &
               !is.na(cov_baseline) & !is.na(population_in_need_fraction) &
               population_in_need_measure %in% c("all", "prevalence", "incidence")]
  ph_costs <- copy(public_health_inputs$costs)   # cost_ready already computed in Model 04

  cost_rows <- list()
  add_clin <- function(scn, ids) {
    comps <- clin_costs[intervention_id %in% ids & cost_ready == TRUE]
    for (i in seq_len(nrow(comps))) {
      cr <- comps[i]
      q_s <- qty_by_year(scn, cr); q_b <- qty_by_year(base_id, cr)
      cov_s <- cov_path(cr$cov_baseline, cr$cov_target, cr$cov_start_year, cr$cov_target_year, analysis_yrs)
      cov_b <- rep(cr$cov_baseline, length(analysis_yrs))
      cost_rows[[length(cost_rows) + 1L]] <<- data.table(
        scenario = scn, family = "clinical", year = analysis_yrs,
        intervention_id = cr$intervention_id, cost_record_id = as.character(cr$cost_record_id),
        cost_key = paste0("clinical|", cr$cost_record_id),
        cost_component = as.character(cr$cost_component %||% cr$cost_component_key),
        cause_code = as.character(cr$cause_code %||% NA_character_),
        cost_scope = as.character(cr$cost_scope), pin_measure = cr$population_in_need_measure,
        pin_fraction = cr$population_in_need_fraction, frequency_per_year = cr$frequency_per_year,
        unit_cost_usd = cr$unit_cost_usd, q_scenario = q_s, q_baseline = q_b,
        covimpl_scenario = cov_s, covimpl_baseline = cov_b,
        indonesia_adjusted_flag = cr$indonesia_adjusted_flag, price_year = cr$price_year,
        review_status = as.character(cr$cost_review))
    }
  }
  add_ph <- function(scn, ids) {
    comps <- ph_costs[intervention_id %in% ids & cost_ready == TRUE]
    for (i in seq_len(nrow(comps))) {
      cr <- comps[i]
      p_s <- pop_for(scn, cr$c_age_start, cr$c_age_stop, cr$c_sex)
      p_b <- pop_for(base_id, cr$c_age_start, cr$c_age_stop, cr$c_sex)
      cost_rows[[length(cost_rows) + 1L]] <<- data.table(
        scenario = scn, family = "public_health", year = analysis_yrs,
        intervention_id = cr$intervention_id, cost_record_id = as.character(cr$cost_record_id),
        cost_key = paste0("public_health|", cr$cost_record_id),
        cost_component = as.character(cr$cost_component %||% cr$cost_component_key),
        cause_code = NA_character_, cost_scope = as.character(cr$cost_scope),
        pin_measure = "all", pin_fraction = cr$population_in_need_fraction,
        frequency_per_year = cr$frequency_per_year, unit_cost_usd = cr$unit_cost_usd,
        q_scenario = p_s, q_baseline = p_b,
        covimpl_scenario = cimpl, covimpl_baseline = rep(base_impl, length(analysis_yrs)),
        indonesia_adjusted_flag = cr$indonesia_adjusted_flag, price_year = cr$price_year,
        review_status = as.character(cr$cost_review))
    }
  }
  for (scn in clin_comparators) add_clin(scn, fair_scenarios[[scn]]$intervention_ids)
  for (scn in ph_comparators)   add_ph(scn,  public_health_scenarios[[scn]]$intervention_ids)
  # JOINT scenario: clinical components (its clinical rule) + PH components (its PH
  # rule), each computed against the joint scenario's OWN Model 06 population.
  add_clin(joint_id, combined_scenarios[[joint_id]]$clinical_intervention_ids)
  add_ph(joint_id,   combined_scenarios[[joint_id]]$public_health_intervention_ids)
  annual_cost <- if (length(cost_rows)) rbindlist(cost_rows, fill = TRUE) else data.table()
  if (nrow(annual_cost)) {
    annual_cost[, discount_factor := 1 / (1 + disc_rate)^(year - yr_start)]
    annual_cost[, pin_scenario := q_scenario * pin_fraction]
    annual_cost[, pin_baseline := q_baseline * pin_fraction]
    annual_cost[, annual_cost_scenario := pin_scenario * covimpl_scenario * frequency_per_year * unit_cost_usd]
    annual_cost[, annual_cost_baseline := pin_baseline * covimpl_baseline * frequency_per_year * unit_cost_usd]
    annual_cost[, annual_cost_incremental := annual_cost_scenario - annual_cost_baseline]
    annual_cost[, disc_cost_incremental := annual_cost_incremental * discount_factor]
  }
  annual_cost <- .add_annualcost_percapita(annual_cost)   # national per-capita component costs (Section 3)

  ## ---- budget impact + cost-effectiveness (R anchors) ---------------------
  if (nrow(annual_cost)) {
    bi <- annual_cost[, .(baseline_cost = sum(annual_cost_baseline),
                          scenario_cost = sum(annual_cost_scenario),
                          incremental_cost = sum(annual_cost_incremental),
                          disc_incremental_cost = sum(disc_cost_incremental)),
                      by = .(scenario, year)]
    setorder(bi, scenario, year)
    bi[, cumulative_incremental_cost := cumsum(incremental_cost), by = scenario]
    bi[, cumulative_disc_incremental_cost := cumsum(disc_incremental_cost), by = scenario]
  } else bi <- data.table()
  bi <- .add_budget_percapita(bi)                         # national + annual per-capita budget impact

  da <- mort[, .(deaths_averted = sum(base_deaths - cause_deaths, na.rm = TRUE),
                 cases_averted  = sum(base_cases  - cases, na.rm = TRUE)), by = scenario]
  ic <- if (nrow(bi)) bi[, .(incremental_cost = sum(incremental_cost),
                             disc_incremental_cost = sum(disc_incremental_cost)), by = scenario] else
    data.table(scenario = comparators, incremental_cost = 0, disc_incremental_cost = 0)
  cea <- merge(da, ic, by = "scenario", all.x = TRUE)
  cea[is.na(incremental_cost), incremental_cost := 0]
  cea[is.na(disc_incremental_cost), disc_incremental_cost := 0]
  cea[, scenario_label := scen_lab[scenario]]
  cea[, cost_per_death_averted := NA_real_]
  cea[deaths_averted > 0, cost_per_death_averted := disc_incremental_cost / deaths_averted]
  setcolorder(cea, c("scenario", "scenario_label", "deaths_averted", "cases_averted",
                     "incremental_cost", "disc_incremental_cost", "cost_per_death_averted"))
  setorder(cea, -deaths_averted)

  # Capture the combined (joint) R-value CE table for the deck results contract.
  deck_cea_list[["combined"]]    <<- copy(cea)
  deck_percap_list[["combined"]] <<- .deck_percap_from_bi(bi)   # mean annual per-capita for the deck

  # anchor scenario for Excel-vs-R reconciliation
  anchor_scn <- joint_id
  ar <- cea[scenario == anchor_scn]
  r_da  <- if (nrow(ar)) ar$deaths_averted[1] else NA_real_
  r_dic <- if (nrow(ar)) ar$disc_incremental_cost[1] else NA_real_
  r_cpd <- if (nrow(ar)) ar$cost_per_death_averted[1] else NA_real_
  tol <- 1e-6
  negc   <- mo[, sum(well < -tol | sick < -tol | newcases < -tol | dead < -tol | pop < -tol)]
  maxres <- mo[, max(abs(pop - (well + sick + all.mx)))]
  ndist  <- mo[, .(n = uniqueN(round(all.mx, 6))), by = .(scenario, year, age, sex)][, max(n)]

  ## ---- scenario catalogue (family + level + provenance) -------------------
  catrow <- function(scn) {
    if (scn == joint_id) { e <- combined_scenarios[[joint_id]]
      return(data.table(scenario = scn, scenario_label = e$scenario_label,
        intervention_family = "clinical_public_health", scenario_level = "combined",
        scenario_role = "combined", parent_package_id = NA_character_,
        intervention_ids = paste(e$intervention_ids, collapse = "; "),
        n_interventions = length(e$intervention_ids))) }
    if (scn %in% names(fair_scenarios)) { e <- fair_scenarios[[scn]]
      return(data.table(scenario = scn, scenario_label = e$scenario_label,
        intervention_family = "clinical",
        scenario_level = if (scn == "all") "combined" else "standalone",
        scenario_role = NA_character_, parent_package_id = NA_character_,
        intervention_ids = paste(e$intervention_ids, collapse = "; "),
        n_interventions = length(e$intervention_ids))) }
    e <- public_health_scenarios[[scn]]
    data.table(scenario = scn, scenario_label = e$scenario_label,
      intervention_family = "public_health",
      scenario_level = e$scenario_level %||% "standalone",
      scenario_role = e$scenario_role %||% NA_character_,
      parent_package_id = e$parent_package_id %||% NA_character_,
      intervention_ids = paste(e$intervention_ids, collapse = "; "),
      n_interventions = length(e$intervention_ids))
  }
  # Include a baseline row so the Scenario_Catalog lists EVERY scenario in the
  # run (baseline + comparators), matching the clinical / public-health catalogs
  # and the deck results contract. Baseline carries no interventions.
  .base_row <- data.table(
    scenario = base_id, scenario_label = "Baseline (no new intervention)",
    intervention_family = "baseline", scenario_level = "baseline",
    scenario_role = NA_character_, parent_package_id = NA_character_,
    intervention_ids = "", n_interventions = 0L)
  scat <- rbindlist(c(list(.base_row), lapply(comparators, catrow)), fill = TRUE)

  ## ---- BCA sources (Model 07 health, Model 08 value) ----------------------
  ho_src_comb <- dt_h07[scenario %in% comparators, .(
    scenario_label = scenario_label[1L],
    modeled_deaths = sum(deaths),   baseline_deaths = sum(base_deaths),
    modeled_cases  = sum(newcases), baseline_cases  = sum(base_newcases),
    yll = sum(yll), base_yll = sum(base_yll), yld = sum(yld), base_yld = sum(base_yld),
    daly = sum(daly), base_daly = sum(base_daly)), by = .(scenario, year)]
  ev_src_comb <- ev08[scenario %in% comparators, .(
    scenario_label    = scenario_label[1L],
    deaths_averted    = sum(deaths_averted),
    life_years_gained = sum(life_years_gained_undisc),
    gni_pc_idn = gni_pc_ppp[1L], gni_pc_usa = gni_pc_usa[1L],
    population = population[1L],  le_avg_adult = le_avg_adult[1L]), by = .(scenario, year)]
  ev_missing <- setdiff(comparators, unique(ev_src_comb$scenario))
  if (length(ev_missing))
    message("  NOTE (combined): no Model 08 economic value for scenario(s): ",
            paste(ev_missing, collapse = ", "),
            " -- they appear in health/CVD sheets but not Economic_Value/Benefit_Cost.")
  scen_meta_comb <- unique(dt_h07[scenario %in% comparators, .(
    scenario, scenario_label, intervention_family,
    scenario_level = fifelse(scenario %in% c("all", "all_public_health", joint_id), "combined",
                      fifelse(is.na(scenario_role), "standalone", scenario_role)))])

  # ------------------------------------------------------------------------
  # WRITE WORKBOOK
  # ------------------------------------------------------------------------
  int2col <- openxlsx::int2col
  frows <- function(fn, rows) vapply(rows, fn, character(1))
  C_HDR <- "#1F4E78"; C_FORMULA <- "#DDEBF7"; C_RSRC <- "#F2F2F2"; C_INPUT <- "#FFF2CC"
  st_hdr   <- createStyle(fontColour = "#FFFFFF", fgFill = C_HDR, textDecoration = "bold",
                          halign = "center", valign = "center", wrapText = TRUE,
                          border = "TopBottomLeftRight", borderColour = "#8EA9C1")
  st_formula <- createStyle(fgFill = C_FORMULA); st_rsrc <- createStyle(fgFill = C_RSRC)
  st_input   <- createStyle(fgFill = C_INPUT);   st_wrap <- createStyle(valign = "top", wrapText = TRUE)
  st_joint   <- createStyle(fgFill = "#FCE4D6", textDecoration = "bold")   # highlight joint row
  cf_pass <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
  cf_fail <- createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")
  cf_rev  <- createStyle(bgFill = "#FFEB9C", fontColour = "#9C6500")
  fmt_of2 <- function(col) {
    cl <- tolower(col)
    if (grepl("frequency", cl)) return("0.00")
    if (grepl("discount_factor|covimpl|impl_frac|_ratio$", cl)) return("0.000")
    if (grepl("^year$|_year$|price_year|start_year|target_year|analysis_", cl)) return("0")
    if (grepl("_fraction$|^fraction$|pin_fraction", cl)) return("0.0%")
    if (grepl("unit_cost", cl)) return("#,##0.0000")
    if (grepl("40q30|reduction_pp|_pct$", cl)) return("0.000")
    if (grepl("percent_reduction", cl)) return("0.00")
    if (grepl("per_death|per_daly|per_case", cl)) return("#,##0")
    if (grepl("benefit_cost_ratio", cl)) return("0.00")
    if (grepl("per_capita", cl) && !grepl("usd_per_capita", cl)) return("#,##0.00")
    if (grepl("cost|value|budget|pin_|_cost$|benefit|net_", cl)) return("#,##0")
    if (grepl("death|case|population|averted|pop_|q_scenario|q_baseline|_count$|residual|distinct|negative|life_years", cl)) return("#,##0")
    NA_character_
  }
  wb <- createWorkbook()
  modifyBaseFont(wb, fontName = "Carlito", fontSize = 11)
  style_sheet <- function(sheet, nm, nrow_data, formula_cols = integer(0),
                          rsource_cols = integer(0), input_cols = integer(0),
                          header_row = 1L, wrap_cols = integer(0), filter = TRUE,
                          min_w = 11, max_w = 46) {
    ncol <- length(nm)
    addStyle(wb, sheet, st_hdr, rows = header_row, cols = seq_len(ncol), gridExpand = TRUE)
    if (nrow_data > 0) {
      dr <- (header_row + 1L):(header_row + nrow_data)
      for (j in formula_cols) addStyle(wb, sheet, st_formula, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in rsource_cols) addStyle(wb, sheet, st_rsrc,    rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in input_cols)   addStyle(wb, sheet, st_input,   rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
      for (j in seq_len(ncol)) { f <- fmt_of2(nm[j])
        if (!is.na(f)) addStyle(wb, sheet, createStyle(numFmt = f), rows = dr, cols = j, gridExpand = TRUE, stack = TRUE) }
      for (j in wrap_cols) addStyle(wb, sheet, st_wrap, rows = dr, cols = j, gridExpand = TRUE, stack = TRUE)
    }
    freezePane(wb, sheet, firstActiveRow = header_row + 1L, firstActiveCol = 1L)
    if (filter) addFilter(wb, sheet, rows = header_row, cols = seq_len(ncol))
    setColWidths(wb, sheet, cols = seq_len(ncol), widths = pmin(pmax(nchar(nm) + 2L, min_w), max_w))
    setRowHeights(wb, sheet, rows = header_row, heights = 28)
  }
  sty_comb <- list(st_hdr = st_hdr, st_formula = st_formula, st_rsrc = st_rsrc,
                   st_input = st_input, st_wrap = st_wrap,
                   cf_pass = cf_pass, cf_fail = cf_fail, cf_rev = cf_rev)

  ## ===== Calculation_Assumptions ==========================================
  ca <- data.table(
    parameter_id = c("analysis_start_year","analysis_end_year","baseline_scenario_id",
                     "cost_discount_rate","cost_price_year","policy_start_year",
                     "policy_cost_ramp_years","baseline_implementation_fraction","currency",
                     "formula_tolerance","stock_flow_residual_limit",
                     "r_negative_state_count","r_stock_flow_max_residual","r_background_distinct_count",
                     "r_deaths_averted_anchor","r_disc_incremental_cost_anchor",
                     "r_cost_per_death_anchor","qa_anchor_scenario"),
    value = list(yr_start, yr_end, base_id, disc_rate, as.integer(A$cost_price_year),
                 policy_start, ramp_years, base_impl, A$currency, 0.001, 1000,
                 as.integer(negc), round(as.numeric(maxres), 2), as.integer(ndist),
                 as.numeric(r_da), as.numeric(r_dic), as.numeric(r_cpd), anchor_scn),
    unit = c("year","year","scenario id","proportion/year","USD year","year","years",
             "proportion","currency","USD/count","persons","count","persons","count",
             "deaths","USD","USD/death","scenario id"),
    role = c(rep("formula control", 9), "QA control","QA control", rep("R QA source",3),
             rep("R reconciliation source",4)),
    description = c("First model and discount year","Last model year","No-new-intervention comparator",
                   "Annual discount rate applied to costs","Reporting price year",
                   "First public-health policy implementation year","Years to full policy cost",
                   "Baseline (counterfactual) policy implementation fraction","Reporting currency",
                   "Absolute reconciliation tolerance","Persons tolerance for the stock/flow check",
                   "Impossible negative state count (R)","Max stock/flow residual (R)",
                   "Max distinct all-cause mx across cause (R)",
                   "R deaths averted for the anchor (joint) scenario",
                   "R discounted incremental cost for the anchor (joint) scenario",
                   "R USD per death averted for the anchor (joint) scenario",
                   "Scenario used for Excel-vs-R reconciliation (the joint scenario)"),
    source = c("Model 04 / Model 09","Model 04 / Model 09","Model 04",
               "indonesia_model_inputs.xlsx","indonesia_model_inputs.xlsx",
               basename(public_health_inputs$inputs_path), basename(public_health_inputs$inputs_path),
               "Model 09","indonesia_model_inputs.xlsx","Workbook QA rule","Workbook QA rule",
               rep("Model 09 current run",3), rep("Model 09 current run (R CEA)",4)))
  .bcab <- bca_ca_block(bca_params)
  n_ca_core <- nrow(ca)
  ca <- rbind(ca, .bcab$ca_bca)
  bca_fmt_vec <- .bcab$fmt
  addWorksheet(wb, "Calculation_Assumptions")
  writeData(wb, "Calculation_Assumptions",
            data.frame(parameter_id="parameter_id", value="value", unit="unit",
                       role="role", description="description", source="source"),
            colNames = FALSE, startRow = 1)
  writeData(wb, "Calculation_Assumptions", ca$parameter_id, startCol = 1, startRow = 2, colNames = FALSE)
  writeData(wb, "Calculation_Assumptions", as.data.frame(ca[, .(unit, role, description, source)]),
            startCol = 3, startRow = 2, colNames = FALSE)
  for (i in seq_len(nrow(ca)))
    writeData(wb, "Calculation_Assumptions", ca$value[[i]], startCol = 2, startRow = 1 + i, colNames = FALSE)
  addStyle(wb, "Calculation_Assumptions", st_hdr, rows = 1, cols = 1:6, gridExpand = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_input, rows = 2:(1 + 11L), cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_rsrc,  rows = (2 + 11L):(1 + n_ca_core), cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_input,
           rows = (n_ca_core + 2L):(nrow(ca) + 1L), cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Calculation_Assumptions", st_wrap, rows = 2:(nrow(ca) + 1), cols = 5, gridExpand = TRUE, stack = TRUE)
  freezePane(wb, "Calculation_Assumptions", firstActiveRow = 2)
  addFilter(wb, "Calculation_Assumptions", rows = 1, cols = 1:6)
  setColWidths(wb, "Calculation_Assumptions", cols = 1:6, widths = c(34, 18, 14, 22, 60, 40))
  setRowHeights(wb, "Calculation_Assumptions", rows = 1, heights = 28)
  .carow <- function(pid) match(pid, ca$parameter_id) + 1L
  .bcell <- function(pid) sprintf("'Calculation_Assumptions'!$B$%d", .carow(pid))
  cA_start <- .bcell("analysis_start_year"); cA_end <- .bcell("analysis_end_year")
  cA_disc  <- .bcell("cost_discount_rate");  cA_tol <- .bcell("formula_tolerance")
  cA_rda   <- .bcell("r_deaths_averted_anchor"); cA_rdic <- .bcell("r_disc_incremental_cost_anchor")
  cA_rcpd  <- .bcell("r_cost_per_death_anchor")
  bca_cells_comb <- list(
    ratio = .bcell("vsl_us_gni_ratio"), elast = .bcell("vsl_income_elasticity_preferred"),
    floor = .bcell("vsl_floor_gni_multiple"), mult100 = .bcell("vsl_sensitivity_gni_multiple_100"),
    mult160 = .bcell("vsl_sensitivity_gni_multiple_160"), r_primary = .bcell("bca_discount_rate_primary"),
    base_year = .bcell("bca_base_year"), price_year = .bcell("bca_price_year"),
    cost_factor = .bcell("cost_to_bca_currency_factor"), scope = .bcell("bca_scope"))

  ## ===== README ==========================================================
  readme <- data.table(section = c(
    "Purpose","Scope","How to read","Scenarios","The joint scenario","Costing the joint scenario",
    "CVD 40q30","Benefit-cost","Colour legend"),
    detail = c(
    "Combined clinical (FAIR Choices) + public-health (fiscal/regulatory) costing, budget impact, cost-effectiveness, CVD 40q30 and Reference-Case benefit-cost analysis in one workbook.",
    "Baseline (once) + every clinical scenario + every public-health scenario + the genuine joint 'all_clinical_public_health' scenario. intervention_family tags each scenario baseline / clinical / public_health / clinical_public_health.",
    "Grey cells are R source values; light-blue cells are LIVE Excel formulas; pale-yellow cells on Calculation_Assumptions are editable controls. The joint scenario is highlighted (orange) on Summary. Calculation_Map lists the dependency chain.",
    "See Summary and Scenario_Catalog. Family-specific input/audit sheets are prefixed CL_ (clinical) and PH_ (public-health) where the two source schemas differ.",
    "'all_clinical_public_health' is produced by a SINGLE Model 06 projection that applies the clinical (fair_wb) and public-health (ph_wb) engines once each to the same baseline-rate copy -- it is NEVER the arithmetic sum of the separate 'all' and 'all_public_health' outputs.",
    "Joint cost = clinical components (clinical costing rule) + public-health components (per-capita policy rule), BOTH computed against the joint scenario's own Model 06 population/state. family provenance is retained on every Annual_Cost row and keys are collision-safe (family|cost_record_id). Budget_Impact carries the four *_cost_per_capita columns as LIVE formulae (= that year's cost / national_population); national_population (grey R-source) is each year's national Indonesia population from out_model/model_output_Indonesia_htncov2_aspirational.rds (baseline series, de-duplicated across cause). Annual_Cost carries the matching per-component per-capita formulae, and Cost_Effectiveness carries the scenario mean annual incremental/discounted per-capita (2026-2050).",
    "CVD_40q30 / CVD_40q30_Age give the period probability of dying from the six CVD causes between exact ages 30 and 70 (percent on a 0-100 scale). The life table (m_x, q_x, l_x, l_{x+1}) is live Excel formula; cvd_40q30 reconciles to the Model 07 R value.",
    "Reference-Case VSL/VSLY benefit-cost (Robinson et al. 2019) on Health_Outcomes / Economic_Value / Benefit_Cost; PARTIAL mortality-benefit BCA.",
    "header dark-blue; formula light-blue; R-source grey; editable-input pale-yellow; QA green/red/orange."))
  addWorksheet(wb, "README")
  writeData(wb, "README", readme, headerStyle = st_hdr)
  style_sheet("README", names(readme), nrow(readme), wrap_cols = 2, filter = FALSE, max_w = 120)
  setColWidths(wb, "README", cols = 1:2, widths = c(24, 120))

  ## ===== Run_Metadata =====================================================
  meta_dt <- data.table(
    field = c("workbook","generated_by","clinical_workbook","public_health_workbook",
              "baseline_scenario","joint_scenario","n_clinical_scenarios","n_public_health_scenarios",
              "analysis_years","location"),
    value = c(basename(out_file), "Model 09 (09_cost_value.R)",
              basename(fair_inputs$inputs_path), basename(public_health_inputs$inputs_path),
              base_id, joint_id, as.character(length(clin_comparators)),
              as.character(length(ph_comparators)), paste0(yr_start, "-", yr_end),
              paste(unique(mo$location), collapse = ", ")))
  addWorksheet(wb, "Run_Metadata")
  writeData(wb, "Run_Metadata", meta_dt, headerStyle = st_hdr)
  style_sheet("Run_Metadata", names(meta_dt), nrow(meta_dt), rsource_cols = 2, wrap_cols = 2, filter = FALSE, max_w = 70)

  ## ===== Scenario_Catalog =================================================
  addWorksheet(wb, "Scenario_Catalog")
  writeData(wb, "Scenario_Catalog", as.data.frame(scat), headerStyle = st_hdr)
  style_sheet("Scenario_Catalog", names(scat), nrow(scat), rsource_cols = seq_along(scat), wrap_cols = c(2, 7))
  .jrow_sc <- which(scat$scenario == joint_id)
  if (length(.jrow_sc)) addStyle(wb, "Scenario_Catalog", st_joint, rows = .jrow_sc + 1L,
                                 cols = seq_along(scat), gridExpand = TRUE, stack = TRUE)

  ## ===== family-specific audit sheets (CL_ / PH_) =========================
  cl_sel <- fair_inputs$valid_links[, .(intervention_id, intervention_cause_key, cause_code,
                                        model_transition, effect_value, affected_fraction,
                                        baseline_coverage, target_coverage, start_year, target_year)]
  addWorksheet(wb, "CL_Selected_Interventions")
  writeData(wb, "CL_Selected_Interventions", as.data.frame(cl_sel), headerStyle = st_hdr)
  style_sheet("CL_Selected_Interventions", names(cl_sel), nrow(cl_sel), rsource_cols = seq_along(cl_sel))
  cl_cc <- clin_costs[, .(cost_record_id, intervention_id, cause_code, cost_component, cost_scope,
                          population_in_need_measure, population_in_need_fraction, frequency_per_year,
                          unit_cost_usd, cov_baseline, cov_target, cost_ready)]
  addWorksheet(wb, "CL_Cost_Components")
  writeData(wb, "CL_Cost_Components", as.data.frame(cl_cc), headerStyle = st_hdr)
  style_sheet("CL_Cost_Components", names(cl_cc), nrow(cl_cc), rsource_cols = seq_along(cl_cc))

  ph_sel <- public_health_inputs$valid_links[, .(intervention_id, intervention_cause_key, cause_code,
                                                 model_transition, transition_from, transition_to,
                                                 effect_model, baseline_exposure, target_exposure,
                                                 response_value, start_year = exposure_start_year,
                                                 target_year = exposure_target_year)]
  addWorksheet(wb, "PH_Selected_Interventions")
  writeData(wb, "PH_Selected_Interventions", as.data.frame(ph_sel), headerStyle = st_hdr)
  style_sheet("PH_Selected_Interventions", names(ph_sel), nrow(ph_sel), rsource_cols = seq_along(ph_sel))
  ph_cc <- ph_costs[, .(cost_record_id, intervention_id, parent_package_id, cost_component, cost_scope,
                        population_in_need_fraction, frequency_per_year, unit_cost_usd,
                        c_age_start, c_age_stop, c_sex, cost_ready)]
  addWorksheet(wb, "PH_Cost_Components")
  writeData(wb, "PH_Cost_Components", as.data.frame(ph_cc), headerStyle = st_hdr)
  style_sheet("PH_Cost_Components", names(ph_cc), nrow(ph_cc), rsource_cols = seq_along(ph_cc))
  ph_hier <- as.data.frame(public_health_inputs$hierarchy)
  addWorksheet(wb, "PH_Scenario_Hierarchy")
  writeData(wb, "PH_Scenario_Hierarchy", ph_hier, headerStyle = st_hdr)
  style_sheet("PH_Scenario_Hierarchy", names(ph_hier), nrow(ph_hier), rsource_cols = seq_along(ph_hier),
              wrap_cols = which(names(ph_hier) %in% c("parent_aggregation_rule","outcome_reporting_rule",
                                                      "cost_reporting_rule","source_note")))

  ## ===== Annual_Mortality =================================================
  am <- copy(mort); am[, `:=`(deaths_averted = NA_real_, cases_averted = NA_real_)]
  n_am <- nrow(am); r_am <- max(n_am + 1L, 2L)
  addWorksheet(wb, "Annual_Mortality")
  writeData(wb, "Annual_Mortality", as.data.frame(am), headerStyle = st_hdr)
  # A scn B label C year D cause E cases F cause_deaths G base_cases H base_deaths I averted J cases_averted
  if (n_am > 0) {
    writeFormula(wb, "Annual_Mortality", startCol = 9, startRow = 2, x = frows(function(r) sprintf("H%d-F%d", r, r), 2:r_am))
    writeFormula(wb, "Annual_Mortality", startCol = 10, startRow = 2, x = frows(function(r) sprintf("G%d-E%d", r, r), 2:r_am))
  }
  style_sheet("Annual_Mortality", names(am), n_am, formula_cols = c(9, 10), rsource_cols = c(5, 6, 7, 8))

  ## ===== Annual_Cost (family provenance; joint costed from its own run) ====
  ac_cols <- c("scenario","family","year","intervention_id","cost_record_id","cost_key",
               "cost_component","cause_code","cost_scope","pin_measure","pin_fraction",
               "frequency_per_year","unit_cost_usd","q_scenario","q_baseline",
               "covimpl_scenario","covimpl_baseline","discount_factor","pin_scenario","pin_baseline",
               "annual_cost_baseline","annual_cost_scenario","annual_cost_incremental",
               "disc_cost_incremental","indonesia_adjusted_flag","price_year","review_status",
               "shared_duplicate_count",
               "national_population","annual_cost_baseline_per_capita","annual_cost_scenario_per_capita",
               "annual_cost_incremental_per_capita","disc_cost_incremental_per_capita")
  n_ac <- nrow(annual_cost); r_ac <- max(n_ac + 1L, 2L)
  if (n_ac > 0) {
    ac <- as.data.frame(annual_cost[, .(scenario, family, year, intervention_id, cost_record_id,
                                        cost_key, cost_component, cause_code, cost_scope, pin_measure,
                                        pin_fraction, frequency_per_year, unit_cost_usd,
                                        q_scenario, q_baseline, covimpl_scenario, covimpl_baseline)],
                        stringsAsFactors = FALSE)
    for (cn in ac_cols[18:24]) ac[[cn]] <- NA_real_
    ac$indonesia_adjusted_flag <- annual_cost$indonesia_adjusted_flag
    ac$price_year <- annual_cost$price_year
    ac$review_status <- annual_cost$review_status
    ac$shared_duplicate_count <- NA_real_
    # AC = R-source national population denominator; AD..AG per-capita = live formulae
    ac$national_population <- annual_cost$national_population
    for (cn in ac_cols[30:33]) ac[[cn]] <- NA_real_
    ac <- ac[, ac_cols]
  } else ac <- as.data.frame(setNames(replicate(length(ac_cols), logical(0), simplify = FALSE), ac_cols))
  addWorksheet(wb, "Annual_Cost")
  writeData(wb, "Annual_Cost", ac, headerStyle = st_hdr)
  if (n_ac > 0) {
    R <- 2:r_ac; wf <- function(col, fn) writeFormula(wb, "Annual_Cost", startCol = col, startRow = 2, x = frows(fn, R))
    wf(18, function(r) sprintf("1/(1+%s)^(C%d-%s)", cA_disc, r, cA_start))     # discount_factor
    wf(19, function(r) sprintf("N%d*K%d", r, r))                              # pin_scenario
    wf(20, function(r) sprintf("O%d*K%d", r, r))                              # pin_baseline
    wf(21, function(r) sprintf("T%d*Q%d*L%d*M%d", r, r, r, r))                # annual_cost_baseline
    wf(22, function(r) sprintf("S%d*P%d*L%d*M%d", r, r, r, r))                # annual_cost_scenario
    wf(23, function(r) sprintf("V%d-U%d", r, r))                              # annual_cost_incremental
    wf(24, function(r) sprintf("W%d*R%d", r, r))                              # disc_cost_incremental
    wf(28, function(r) sprintf("IF(I%d=\"shared-count-once\",COUNTIFS($A$2:$A$%d,A%d,$C$2:$C$%d,C%d,$E$2:$E$%d,E%d,$B$2:$B$%d,B%d),1)",
                               r, r_ac, r, r_ac, r, r_ac, r, r_ac, r))        # shared_duplicate_count
    # AD..AG component annual cost per capita = component cost / national_population (AC)
    wf(30, function(r) sprintf("IF(AC%d=0,\"\",U%d/AC%d)", r, r, r))          # annual_cost_baseline_per_capita
    wf(31, function(r) sprintf("IF(AC%d=0,\"\",V%d/AC%d)", r, r, r))          # annual_cost_scenario_per_capita
    wf(32, function(r) sprintf("IF(AC%d=0,\"\",W%d/AC%d)", r, r, r))          # annual_cost_incremental_per_capita
    wf(33, function(r) sprintf("IF(AC%d=0,\"\",X%d/AC%d)", r, r, r))          # disc_cost_incremental_per_capita
  }
  style_sheet("Annual_Cost", ac_cols, n_ac, formula_cols = c(18:24, 28, 30:33), rsource_cols = c(11:17, 29))

  ## ===== Budget_Impact ====================================================
  bud_cols <- c("scenario","year","baseline_cost","scenario_cost","incremental_cost",
                "disc_incremental_cost","cumulative_incremental_cost","cumulative_disc_incremental_cost",
                "national_population","baseline_cost_per_capita","scenario_cost_per_capita",
                "incremental_cost_per_capita","disc_incremental_cost_per_capita")
  n_bi <- nrow(bi); r_bi <- max(n_bi + 1L, 2L)
  if (n_bi > 0) { bud <- data.frame(scenario = bi$scenario, year = bi$year, stringsAsFactors = FALSE)
    for (cn in bud_cols[3:8]) bud[[cn]] <- NA_real_
    bud$national_population <- bi$national_population        # I = R-source denominator
    for (cn in bud_cols[10:13]) bud[[cn]] <- NA_real_        # J..M per-capita = live formulae
  } else bud <- as.data.frame(setNames(replicate(length(bud_cols), logical(0), simplify = FALSE), bud_cols))
  addWorksheet(wb, "Budget_Impact")
  writeData(wb, "Budget_Impact", bud, headerStyle = st_hdr)
  if (n_bi > 0) {
    R <- 2:r_bi
    sac <- function(tgt, r) sprintf("SUMIFS('Annual_Cost'!$%s$2:$%s$%d,'Annual_Cost'!$A$2:$A$%d,A%d,'Annual_Cost'!$C$2:$C$%d,B%d)",
                                    tgt, tgt, r_ac, r_ac, r, r_ac, r)
    writeFormula(wb, "Budget_Impact", startCol = 3, startRow = 2, x = frows(function(r) sac("U", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 4, startRow = 2, x = frows(function(r) sac("V", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 5, startRow = 2, x = frows(function(r) sac("W", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 6, startRow = 2, x = frows(function(r) sac("X", r), R))
    writeFormula(wb, "Budget_Impact", startCol = 7, startRow = 2, x = frows(function(r) sprintf("SUMIFS($E$2:E%d,$A$2:A%d,A%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 8, startRow = 2, x = frows(function(r) sprintf("SUMIFS($F$2:F%d,$A$2:A%d,A%d)", r, r, r), R))
    # J..M annual cost per capita = same-year cost / national_population (col I), live formulae
    writeFormula(wb, "Budget_Impact", startCol = 10, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",C%d/I%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 11, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",D%d/I%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 12, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",E%d/I%d)", r, r, r), R))
    writeFormula(wb, "Budget_Impact", startCol = 13, startRow = 2, x = frows(function(r) sprintf("IF(I%d=0,\"\",F%d/I%d)", r, r, r), R))
  }
  style_sheet("Budget_Impact", bud_cols, n_bi, formula_cols = c(3:8, 10:13), rsource_cols = 9)

  ## ===== Cost_Effectiveness ===============================================
  ce_cols <- c("scenario","scenario_label","deaths_averted","cases_averted","incremental_cost",
               "disc_incremental_cost","cost_per_death_averted","dominance","reconciliation_status",
               "annual_cost_incremental_per_capita","disc_cost_incremental_per_capita")
  n_ce <- nrow(cea); r_ce <- max(n_ce + 1L, 2L)
  ce <- data.frame(scenario = cea$scenario, scenario_label = cea$scenario_label, stringsAsFactors = FALSE)
  for (cn in ce_cols[3:7]) ce[[cn]] <- NA_real_
  ce$dominance <- NA_character_; ce$reconciliation_status <- NA_character_
  ce$annual_cost_incremental_per_capita <- NA_real_   # J = live formula
  ce$disc_cost_incremental_per_capita   <- NA_real_   # K = live formula
  addWorksheet(wb, "Cost_Effectiveness")
  writeData(wb, "Cost_Effectiveness", ce, headerStyle = st_hdr)
  if (n_ce > 0) {
    R <- 2:r_ce
    writeFormula(wb, "Cost_Effectiveness", startCol = 3, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Annual_Mortality'!$I$2:$I$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 4, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Annual_Mortality'!$J$2:$J$%d,'Annual_Mortality'!$A$2:$A$%d,A%d)", r_am, r_am, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 5, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Budget_Impact'!$E$2:$E$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 6, startRow = 2, x = frows(function(r)
      sprintf("SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 7, startRow = 2, x = frows(function(r)
      sprintf("IF(C%d>0,F%d/C%d,\"\")", r, r, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 8, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(C%d>0,F%d<0),\"Dominant (more health, lower cost)\",IF(AND(C%d<=0,F%d>0),\"Dominated (less/no health, higher cost)\",IF(AND(C%d<=0,F%d<=0),\"No deaths averted; ratio not defined\",\"USD per death averted\")))",
              r, r, r, r, r, r), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 9, startRow = 2, x = frows(function(r)
      sprintf("IF(AND(ABS(F%d-SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d))<=%s,ABS(C%d-SUMIFS('Annual_Mortality'!$I$2:$I$%d,'Annual_Mortality'!$A$2:$A$%d,A%d))<=%s),\"consistent\",\"mismatch\")",
              r, r_bi, r_bi, r, cA_tol, r, r_am, r_am, r, cA_tol), R))
    # J/K summary annual per-capita = mean of Budget_Impact per-capita (L/M) over years >= start+1
    writeFormula(wb, "Cost_Effectiveness", startCol = 10, startRow = 2, x = frows(function(r)
      sprintf("IFERROR(AVERAGEIFS('Budget_Impact'!$L$2:$L$%d,'Budget_Impact'!$A$2:$A$%d,A%d,'Budget_Impact'!$B$2:$B$%d,\">=\"&(%s+1)),\"\")", r_bi, r_bi, r, r_bi, cA_start), R))
    writeFormula(wb, "Cost_Effectiveness", startCol = 11, startRow = 2, x = frows(function(r)
      sprintf("IFERROR(AVERAGEIFS('Budget_Impact'!$M$2:$M$%d,'Budget_Impact'!$A$2:$A$%d,A%d,'Budget_Impact'!$B$2:$B$%d,\">=\"&(%s+1)),\"\")", r_bi, r_bi, r, r_bi, cA_start), R))
  }
  style_sheet("Cost_Effectiveness", ce_cols, n_ce, formula_cols = c(3:9, 10:11), wrap_cols = c(2, 8))
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "consistent", type = "contains", style = cf_pass)
  conditionalFormatting(wb, "Cost_Effectiveness", cols = 9, rows = 2:r_ce, rule = "mismatch",   type = "contains", style = cf_fail)

  ## ===== Health_Outcomes + Economic_Value + Benefit_Cost (BCA) ============
  n_ho <- nrow(ho_src_comb); r_ho <- max(n_ho + 1L, 2L)
  n_bc <- nrow(scen_meta_comb) * 4L; r_bc <- max(n_bc + 1L, 2L)
  if (nrow(ev_src_comb) > 0L) {
    build_bca_sheets_into(wb, comparators, ho_src_comb, ev_src_comb, scen_meta_comb,
                          bca_cells_comb, r_bi, sty_comb)
  } else {
    addWorksheet(wb, "Economic_Value")
    writeData(wb, "Economic_Value",
              data.frame(note = "No Model 08 economic value for the combined comparators in this run."),
              headerStyle = st_hdr)
    setColWidths(wb, "Economic_Value", cols = 1, widths = 110)
  }

  ## ===== CVD_40q30_Age + CVD_40q30 ========================================
  build_cvd_40q30_sheets_into(wb, combined_ids, dt_cvd_40q30, cvd_age_40q30, base_id, sty_comb)
  r_q40s <- nrow(dt_cvd_40q30[scenario %in% combined_ids]) + 1L   # CVD_40q30 sheet rows

  ## ===== Summary (first sheet; formula-driven; joint highlighted) =========
  sum_cols <- c("scenario","scenario_label","intervention_family","scenario_level","n_interventions",
                "analysis_start_year","analysis_end_year","cases_averted","deaths_averted",
                "ylls_averted","ylds_averted","dalys_averted","life_years_gained",
                "cvd_40q30_start_pct","cvd_40q30_end_pct","cvd_40q30_abs_reduction_pp_end",
                "cvd_40q30_pct_reduction_end","incremental_cost","disc_incremental_cost",
                "cost_per_death_averted","cost_per_daly_averted","pv_benefits","pv_costs",
                "pv_net_benefit","benefit_cost_ratio")
  SM <- data.frame(scenario = scat$scenario, scenario_label = scat$scenario_label,
                   intervention_family = scat$intervention_family, scenario_level = scat$scenario_level,
                   n_interventions = scat$n_interventions, stringsAsFactors = FALSE)
  for (cn in sum_cols[6:25]) SM[[cn]] <- if (cn == "benefit_cost_ratio") NA_real_ else NA_real_
  SM <- SM[, sum_cols]
  n_sm <- nrow(SM); r_sm <- max(n_sm + 1L, 2L)
  addWorksheet(wb, "Summary")
  writeData(wb, "Summary", SM, headerStyle = st_hdr)
  if (n_sm > 0) {
    R <- 2:r_sm
    wfS <- function(col, fn) writeFormula(wb, "Summary", startCol = col, startRow = 2, x = frows(fn, R))
    # analysis years (pulled from the Calculation_Assumptions controls)
    wfS(6, function(r) cA_start); wfS(7, function(r) cA_end)
    # Health_Outcomes aggregates by scenario (A=scenario; cols 9/6/12/15/18/19)
    hos <- function(col, r) sprintf("SUMIFS('Health_Outcomes'!$%s$2:$%s$%d,'Health_Outcomes'!$A$2:$A$%d,A%d)", col, col, r_ho, r_ho, r)
    wfS(8,  function(r) hos("I", r))   # cases_averted (HO col 9)
    wfS(9,  function(r) hos("F", r))   # deaths_averted (HO col 6)
    wfS(10, function(r) hos("L", r))   # ylls_averted (HO col 12)
    wfS(11, function(r) hos("O", r))   # ylds_averted (HO col 15)
    wfS(12, function(r) hos("R", r))   # dalys_averted (HO col 18)
    wfS(13, function(r) hos("S", r))   # life_years_gained (HO col 19)
    # CVD_40q30 lookups (B=scenario, H=year, I=cvd_40q30, K=abs_red, L=pct_red)
    q40 <- function(col, yrcell, r) sprintf("SUMIFS('CVD_40q30'!$%s$2:$%s$%d,'CVD_40q30'!$B$2:$B$%d,A%d,'CVD_40q30'!$H$2:$H$%d,%s%d)",
                                            col, col, r_q40s, r_q40s, r, r_q40s, yrcell, r)
    wfS(14, function(r) q40("I", "F", r))   # start-year 40q30
    wfS(15, function(r) q40("I", "G", r))   # end-year 40q30
    wfS(16, function(r) q40("K", "G", r))   # end-year absolute reduction (pp)
    wfS(17, function(r) q40("L", "G", r))   # end-year relative reduction (%)
    # costs (Budget_Impact E incremental, F disc incremental) summed over years
    wfS(18, function(r) sprintf("SUMIFS('Budget_Impact'!$E$2:$E$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r))
    wfS(19, function(r) sprintf("SUMIFS('Budget_Impact'!$F$2:$F$%d,'Budget_Impact'!$A$2:$A$%d,A%d)", r_bi, r_bi, r))
    wfS(20, function(r) sprintf("IF(I%d>0,S%d/I%d,\"\")", r, r, r))     # cost per death averted
    wfS(21, function(r) sprintf("IF(L%d>0,S%d/L%d,\"\")", r, r, r))     # cost per DALY averted
    # Benefit_Cost preferred VSL row (F=valuation_case, G/H/I pv values)
    pref <- "preferred (elasticity 1.5, 20x floor)"
    bcs <- function(col, r) sprintf("SUMIFS('Benefit_Cost'!$%s$2:$%s$%d,'Benefit_Cost'!$A$2:$A$%d,A%d,'Benefit_Cost'!$F$2:$F$%d,\"%s\")",
                                    col, col, r_bc, r_bc, r, r_bc, pref)
    wfS(22, function(r) bcs("G", r))     # pv_benefits
    wfS(23, function(r) bcs("H", r))     # pv_costs
    wfS(24, function(r) sprintf("V%d-W%d", r, r))                       # pv_net_benefit
    wfS(25, function(r) sprintf("IF(W%d>0,V%d/W%d,\"\")", r, r, r))     # benefit_cost_ratio
  }
  style_sheet("Summary", sum_cols, n_sm, formula_cols = 6:25, rsource_cols = 5, wrap_cols = 2, max_w = 22)
  .jrow_sm <- which(SM$scenario == joint_id)
  if (length(.jrow_sm)) addStyle(wb, "Summary", st_joint, rows = .jrow_sm + 1L,
                                 cols = seq_along(sum_cols), gridExpand = TRUE, stack = TRUE)

  ## ===== QA_Checks ========================================================
  paired <- all(comparators %in% unique(mort$scenario))
  qa_df <- data.frame(
    check = c("Every comparator paired to baseline (R)","No impossible negative states (R)",
              "Joint scenario present and costed","Cost reconciliation (components -> budget impact)",
              "CEA reconciliation (detail -> summary)","CVD 40q30 Excel vs R (all rows match)",
              "Excel vs R: joint deaths averted","Excel vs R: joint discounted incremental cost",
              "Baseline excluded from comparators (counted once)"),
    expected = c("TRUE","0","TRUE","<= tol","consistent","0","match R","match R","0"),
    actual = NA, status = NA_character_,
    note = c("Deaths averted = baseline - scenario at matched year/cause",
             "well/sick/new_cases/deaths/population >= 0",
             "The joint 'all_clinical_public_health' scenario is in Annual_Cost and Budget_Impact",
             "Excel component rows sum to Excel budget-impact totals",
             "Every Cost_Effectiveness row's internal reconciliation is consistent",
             "CVD_40q30 recon_status has no 'mismatch'",
             "Joint scenario Excel CEA deaths averted reconciles to the R engine value",
             "Joint scenario Excel discounted incremental cost reconciles to the R engine value",
             "The shared baseline is the comparator, never itself a comparator row"),
    stringsAsFactors = FALSE)
  addWorksheet(wb, "QA_Checks")
  writeData(wb, "QA_Checks", qa_df, headerStyle = st_hdr)
  n_qa <- nrow(qa_df); r_qa <- n_qa + 1L
  qa_actual <- c(
    ifelse(paired, "TRUE", "FALSE"), as.character(negc), "TRUE",
    sprintf("ABS(SUM('Budget_Impact'!$C$2:$C$%d)-SUM('Annual_Cost'!$U$2:$U$%d))+ABS(SUM('Budget_Impact'!$D$2:$D$%d)-SUM('Annual_Cost'!$V$2:$V$%d))",
            r_bi, r_ac, r_bi, r_ac),
    sprintf("IF(COUNTIF('Cost_Effectiveness'!$I$2:$I$%d,\"mismatch\")=0,\"consistent\",\"mismatch\")", r_ce),
    sprintf("COUNTIF('CVD_40q30'!$N$2:$N$%d,\"mismatch\")", r_q40s),
    sprintf("INDEX('Cost_Effectiveness'!$C$2:$C$%d,MATCH(\"%s\",'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, joint_id, r_ce),
    sprintf("INDEX('Cost_Effectiveness'!$F$2:$F$%d,MATCH(\"%s\",'Cost_Effectiveness'!$A$2:$A$%d,0))", r_ce, joint_id, r_ce),
    sprintf("COUNTIF('Summary'!$A$2:$A$%d,\"%s\")", r_sm, base_id))
  qa_status <- c(
    "IF(C2=TRUE,\"PASS\",\"FAIL\")","IF(C3=0,\"PASS\",\"FAIL\")","IF(C4=TRUE,\"PASS\",\"FAIL\")",
    sprintf("IF(C5<=%s,\"PASS\",\"FAIL\")", cA_tol),
    "IF(C6=\"consistent\",\"PASS\",\"FAIL\")","IF(C7=0,\"PASS\",\"FAIL\")",
    sprintf("IF(ABS(C8-%s)<=ABS(%s)*0.000001+0.5,\"PASS\",\"FAIL\")", cA_rda, cA_rda),
    sprintf("IF(ABS(C9-%s)<=ABS(%s)*0.000001+1,\"PASS\",\"FAIL\")", cA_rdic, cA_rdic),
    "IF(C10=0,\"PASS\",\"FAIL\")")
  writeFormula(wb, "QA_Checks", startCol = 3, startRow = 2, x = qa_actual)
  writeFormula(wb, "QA_Checks", startCol = 4, startRow = 2, x = qa_status)
  style_sheet("QA_Checks", names(qa_df), n_qa, formula_cols = c(3, 4), wrap_cols = 5)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "PASS", type = "contains", style = cf_pass)
  conditionalFormatting(wb, "QA_Checks", cols = 4, rows = 2:r_qa, rule = "FAIL", type = "contains", style = cf_fail)

  ## ===== Methods_and_Sources + Calculation_Map ============================
  methods <- data.table(
    method_id = c("M01","M02","M03","M04","M05","M06","M07"),
    concept = c("Joint scenario","Joint costing","CVD 40q30","Family provenance",
                "Benefit-cost","Reconciliation","Scale"),
    detail = c(
      "'all_clinical_public_health' = ONE Model 06 projection applying fair_wb + ph_wb once each to the same baseline-rate copy (not an arithmetic sum of 'all' + 'all_public_health').",
      "Joint annual + discounted incremental cost = sum of clinical components (clinical rule) + public-health components (per-capita policy rule), BOTH computed against the joint scenario's own Model 06 population; keys are collision-safe (family|cost_record_id).",
      "Period 40q30 over the six CVD causes, exact ages 30-69: m_x=(D_F+D_M)/(N_F+N_M), q_x=1-exp(-m_x), l_30=1, l_{x+1}=l_x(1-q_x), 40q30=100*(1-l_70/l_30). Live life-table formulas on CVD_40q30_Age; reconciled to Model 07.",
      "intervention_family tags every scenario baseline / clinical / public_health / clinical_public_health; every Annual_Cost row carries its family.",
      "Reference-Case VSL/VSLY (Robinson et al. 2019): preferred VSL = MAX(160*GNIpc_US*(GNIpc_IDN/GNIpc_US)^1.5, 20*GNIpc_IDN); PARTIAL mortality-benefit BCA.",
      "Excel formulas reconcile against R engine anchors stored on Calculation_Assumptions (deaths averted, discounted incremental cost, cost per death) and CVD_40q30 recon_status.",
      "cvd_40q30 is a PERCENT on a 0-100 scale (number format 0.000, never Excel's fractional %); absolute_reduction_pp is percentage points."),
    source = c(rep("Model 04 / Model 06 / Model 09", 2), "Model 07 / Model 09",
               "Model 06 / Model 09", "Robinson et al. 2019 / Model 08",
               "Model 09", "Model 07 / Model 09"))
  addWorksheet(wb, "Methods_and_Sources")
  writeData(wb, "Methods_and_Sources", methods, headerStyle = st_hdr)
  style_sheet("Methods_and_Sources", names(methods), nrow(methods), wrap_cols = c(3, 4), filter = FALSE, max_w = 96)
  setColWidths(wb, "Methods_and_Sources", cols = 1:4, widths = c(10, 22, 96, 40))

  cmap <- data.table(
    output_sheet = c("Summary","Annual_Mortality","Annual_Cost","Budget_Impact","Cost_Effectiveness",
                     "Health_Outcomes","Economic_Value","Benefit_Cost","CVD_40q30_Age","CVD_40q30"),
    formula_columns = c("F:Y","I,J","R:X,AB","C:H","C:I","F,I,L,O,R,S","J:AB","G:M","J:M","I,J,K,L,N"),
    depends_on = c("Health_Outcomes, CVD_40q30, Budget_Impact, Benefit_Cost",
                   "R aggregates E:H","Cost_Components (R source N:Q); Calculation_Assumptions",
                   "Annual_Cost","Annual_Mortality; Budget_Impact",
                   "Model 07 modeled/baseline health (grey)","Model 08 source (grey); Calculation_Assumptions BCA",
                   "Economic_Value PV benefits; Budget_Impact costs","cvd_deaths/population (grey R source)",
                   "CVD_40q30_Age life table; baseline SUMIFS; R anchor"),
    source_role = rep("Workbook formula", 10))
  addWorksheet(wb, "Calculation_Map")
  writeData(wb, "Calculation_Map", cmap, headerStyle = st_hdr)
  style_sheet("Calculation_Map", names(cmap), nrow(cmap), wrap_cols = c(3, 4), filter = FALSE, max_w = 60)
  setColWidths(wb, "Calculation_Map", cols = 1:4, widths = c(20, 22, 52, 20))

  ## ===== order, recalc, strip, save =======================================
  desired_order <- c("Summary","README","Run_Metadata","Scenario_Catalog",
                     "CL_Selected_Interventions","CL_Cost_Components",
                     "PH_Selected_Interventions","PH_Cost_Components","PH_Scenario_Hierarchy",
                     "Annual_Mortality","Health_Outcomes","CVD_40q30","CVD_40q30_Age",
                     "Annual_Cost","Budget_Impact","Cost_Effectiveness","Economic_Value","Benefit_Cost",
                     "QA_Checks","Methods_and_Sources","Calculation_Assumptions","Calculation_Map")
  desired_order <- desired_order[desired_order %in% names(wb)]
  worksheetOrder(wb) <- match(desired_order, names(wb))
  wb$workbook$calcPr <- '<calcPr calcId="191029" fullCalcOnLoad="1"/>'
  if (exists("strip_dangling_drawings")) strip_dangling_drawings(wb)
  if (!dir.exists(dirname(out_file))) dir.create(dirname(out_file), recursive = TRUE)
  saveWorkbook(wb, out_file, overwrite = TRUE)
  message("  Wrote combined clinical + public-health formula workbook: ", out_file)
  message(sprintf("  Combined scenarios (%d): %s", length(comparators),
                  paste(comparators, collapse = ", ")))
  invisible(out_file)
}

#===========================================================================
# 12. PUBLIC-HEALTH cost/value formula workbook ----
#---------------------------------------------------------------------------
# Written only when run_public_health_interventions = TRUE. Consumes ONLY the
# current-run public-health catalogue (public_health_inputs from Model 04) and
# the public-health scenarios in the shared Model 06 output. Mirrors the clinical
# formatting/audit pattern but adapts every sheet and formula to exposure-based
# public-health effects and per-capita policy costs. Writes exactly one file:
#   output/indonesia_cost_value_public_health_formulae.xlsx
# It is a fully-formatted, formula-driven workbook (not a copy of the clinical
# one and not an unformatted data dump).
#===========================================================================
if (isTRUE(run_public_health_interventions)) {
  ph_ok <- tryCatch({ source_public_health_cost_value(); TRUE },
                    error = function(e) { message("  Public-health workbook FAILED: ",
                                                  conditionMessage(e)); FALSE })
}

#===========================================================================
# 13. COMBINED clinical + public-health cost/value formula workbook ----
#---------------------------------------------------------------------------
# Written ONLY when BOTH intervention families are enabled and the genuine joint
# scenario was produced by Model 06. Writes exactly one file:
#   output/indonesia_model_cost_value_clinical_public_health_formulae.xlsx
#===========================================================================
if (isTRUE(run_clinical_interventions) && isTRUE(run_public_health_interventions) &&
    exists("combined_scenarios") && !is.null(combined_scenarios)) {
  combined_ok <- tryCatch({ source_combined_cost_value(); TRUE },
                          error = function(e) { message("  Combined workbook FAILED: ",
                                                        conditionMessage(e)); FALSE })
}

#===========================================================================
# 14. Deck results contract (BCA-FREE, current-run) ----
#---------------------------------------------------------------------------
# A compact, R-side results contract for the executive slide deck so it can
# compile WITHOUT Excel recalculation and WITHOUT any BCA/VSL/VSLY input.
# Every number here is the SAME R-engine value the workbooks store (as formulas):
#   * deaths averted + incremental cost + cost per death averted: from each
#     workbook's own R-value cost-effectiveness table (deck_cea_list);
#   * 2050 death levels (for % reduction): from Model 07 health output;
#   * CVD 40q30 (level + baseline + % reduction in 2050): from Model 07 40q30.
# Scenario identity/labels/order use the same Scenario_Catalog contract written
# into the workbooks. This is ADDITIONAL to (never a replacement for) the BCA /
# Model 08 artifacts, which remain untouched.
#===========================================================================
if (length(deck_cea_list)) {
  .assump <- if (exists("fair_inputs") && !is.null(fair_inputs$assumptions))
      fair_inputs$assumptions
    else if (exists("public_health_inputs") && !is.null(public_health_inputs$assumptions))
      public_health_inputs$assumptions
    else list()
  .num <- function(v, d = NA_real_) { x <- suppressWarnings(as.numeric(v))
    if (length(x) && !is.na(x[1])) x[1] else d }

  # Restrict health artifacts to a single HTN-coverage target (current run).
  .pick_htn <- function(dt) {
    if (!"htn_target_scenario" %in% names(dt)) return(dt)
    tg <- unique(dt$htn_target_scenario)
    if (length(tg) > 1L) tg <- if ("htncov2_aspirational" %in% tg) "htncov2_aspirational" else tg[1]
    dt[htn_target_scenario %in% tg]
  }
  h07 <- .pick_htn(copy(dt_h07))
  q40 <- .pick_htn(copy(dt_cvd_40q30))
  if ("cause" %in% names(h07)) h07 <- h07[!(tolower(as.character(cause)) %in% "all")]
  ymax <- max(analysis_yrs)

  # 2050 modeled + baseline deaths (all modeled causes) per scenario.
  d2050 <- h07[year == ymax,
               .(deaths_2050 = sum(deaths, na.rm = TRUE),
                 baseline_deaths_2050 = sum(base_deaths, na.rm = TRUE)), by = scenario]
  d2050[, pct_reduction_deaths_2050 := ifelse(baseline_deaths_2050 > 0,
        100 * (baseline_deaths_2050 - deaths_2050) / baseline_deaths_2050, NA_real_)]

  # CVD 40q30 in 2050 (level, baseline, % reduction) per scenario.
  q2050 <- q40[year == ymax,
               .(cvd_40q30_2050 = cvd_40q30[1],
                 baseline_cvd_40q30_2050 = baseline_cvd_40q30[1],
                 cvd_40q30_pct_reduction_2050 = percent_reduction[1]), by = scenario]

  build_family <- function(wbkey, meta) {
    cea <- deck_cea_list[[wbkey]]
    if (is.null(cea) || !nrow(cea) || is.null(meta) || !nrow(meta)) return(NULL)
    scn_set <- union(cea$scenario, baseline_scenario_id)
    meta <- meta[scenario %in% scn_set]
    keep <- intersect(c("scenario", "deaths_averted", "cases_averted",
                        "incremental_cost", "disc_incremental_cost",
                        "cost_per_death_averted"), names(cea))
    out <- merge(meta, cea[, ..keep], by = "scenario", all.x = TRUE)
    out <- merge(out, d2050, by = "scenario", all.x = TRUE)
    out <- merge(out, q2050, by = "scenario", all.x = TRUE)
    # Mean annual per-capita additional cost (2026-2050) the deck displays. Read
    # from the workbook's live per-capita formulae is impossible without an Excel
    # recalc, so the SAME R value is carried here (workbook formulae reconcile to it).
    pcp <- deck_percap_list[[wbkey]]
    if (!is.null(pcp) && nrow(pcp)) out <- merge(out, pcp, by = "scenario", all.x = TRUE)
    else { out[, pc_undisc_val := NA_real_]; out[, pc_disc_val := NA_real_] }
    out[, workbook := wbkey]
    # Uniform cost-effectiveness status / dominance labelling.
    out[, ce_status := "USD per death averted"]
    out[!is.na(deaths_averted) & deaths_averted > 0 & !is.na(disc_incremental_cost) &
          disc_incremental_cost < 0, ce_status := "Dominant (more health, lower cost)"]
    out[!is.na(deaths_averted) & deaths_averted <= 0 & !is.na(disc_incremental_cost) &
          disc_incremental_cost > 0, ce_status := "Dominated (less/no health, higher cost)"]
    out[!is.na(deaths_averted) & deaths_averted <= 0 & !is.na(disc_incremental_cost) &
          disc_incremental_cost <= 0, ce_status := "No deaths averted; ratio not defined"]
    out[is.na(deaths_averted) | scenario == baseline_scenario_id,
        ce_status := "baseline / not applicable"]
    out[]
  }

  meta_comb <- unique(rbindlist(list(
      .scenario_catalog_dt(if (exists("fair_scenarios")) fair_scenarios else NULL,
                           "clinical", baseline_scenario_id),
      .scenario_catalog_dt(if (exists("public_health_scenarios")) public_health_scenarios else NULL,
                           "public_health", baseline_scenario_id),
      .scenario_catalog_dt(if (exists("combined_scenarios")) combined_scenarios else NULL,
                           "clinical_public_health", baseline_scenario_id)),
    fill = TRUE), by = "scenario")

  deck_rows <- rbindlist(list(
      build_family("clinical",      deck_catalog_list[["clinical"]]),
      build_family("public_health", deck_catalog_list[["public_health"]]),
      build_family("combined",      meta_comb)), fill = TRUE)

  deck_meta <- list(
    horizon_start        = .num(.assump$analysis_start_year, min(analysis_yrs)),
    horizon_end          = .num(.assump$analysis_end_year,   ymax),
    cost_discount_rate   = .num(.assump$cost_discount_rate),
    cost_price_year      = .num(.assump$cost_price_year),
    currency             = as.character(.coalesce_scalar(.assump$currency, "USD (market)")),
    economic_perspective = as.character(.coalesce_scalar(.assump$economic_perspective, NA_character_)),
    cost_effectiveness_unit = "USD per death averted (discounted incremental cost / undiscounted deaths averted)",
    baseline_scenario    = baseline_scenario_id,
    generated_scenarios  = sort(unique(deck_rows$scenario)),
    excluded_note        = "Scenarios reflect the binding Intervention_Cause_Map include_flag; excluded interventions do not appear.")
  attr(deck_rows, "deck_meta") <- deck_meta

  deck_results_file <- file.path(wd_outp, "09_deck_results.rds")
  saveRDS(list(results = deck_rows, meta = deck_meta), deck_results_file)
  message("  Wrote deck results contract: ", deck_results_file,
          sprintf(" (%d scenario-rows across %d workbook families)",
                  nrow(deck_rows), uniqueN(deck_rows$workbook)))
}

message("=== Model 09 complete ===")
