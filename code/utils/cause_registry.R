################################################################################
# INDONESIA INTEGRATED NCD MODEL — CAUSE REGISTRY
# R/cause_registry.R
# ─────────────────────────────────────────────────────────────────────────────
# Defines the cause-agnostic state-transition framework for all disease modules.
# Cause definitions (state vectors, transition adjacency, calibration targets)
# live here as metadata. Intervention effect functions live in R/interventions_cvd.R.
# Adding a new cause = writing a new spec. Zero new engine code required.
#
# CONTENTS:
#   SHARED_PARAMS    — global model parameters (years, ages, tolerance)
#   CVD_CAUSE_MAP    — module_id → GBD cause label (used in scripts 4, 5, 6)
#   CVD_MODEL_TYPE   — module_id → "tpm" or "direct_mortality"
#   CAUSE_REGISTRY   — full list of 23 cause specs
#   Registry helpers — get_cause_spec(), get_causes_by_group(), etc.
#
# FRAMEWORK COMPONENTS per cause spec:
#   1. STATE VECTOR     K named states, including absorbing states (dead_*)
#   2. ADJACENCY LIST   Which transitions are non-zero (graph structure)
#   3. MECHANISM TAGS   How each transition probability is computed
#   4. CALIBRATION      Target data and validation criterion
#   5. INTERVENTIONS    Modifications to specific transition slots
#
# TRANSITION MECHANISM TAGS:
#   "GBD_incidence"   p ≈ 1 - exp(-ir)            [Well → first sick state]
#   "GBD_CFR"         p = mx_cause / incidence      [Sick → Dead_cause, acute]
#   "GBD_stage_CFR"   p = stage-specific CFR        [Stage → Dead_cause]
#   "GBD_progression" p = back-calculated from stage distribution balance
#   "bgmx"            p = mx_WPP − Σ mx_cause       [Any live → Dead_bg]
#   "complement"      p = 1 − Σ other outflows       [diagonal stay-in-state]
#
# INTERVENTION MECHANISM TAGS:
#   "care_cascade"        CVD: population-level SBP reduction via coverage ramp
#   "PAF_shift"           Dietary/risk-factor PIF on incidence
#   "stage_shift"         Cancer: screening redistributes stage at detection
#   "adherence_CFR"       Cancer: treatment adherence modifies stage CFR
#   "incidence_reduction" Direct proportional reduction in incidence
#
# MARKOV CONVENTION:
#   TPM rows = FROM state, TPM columns = TO state. All rows sum to 1.
#   Dead states are absorbing: TPM[dead, dead] = 1.
#   Note: specs use "dead_background" as conceptual label; runtime modules
#   use "dead_bg" as state name. Both are valid — specs are metadata, not code.
################################################################################

# ── SHARED PARAMETERS ─────────────────────────────────────────────────────────
SHARED_PARAMS <- list(
  calib_year   = 2023L,
  run_years    = 2025:2100,
  warmup_cycles = 0L,          # V1 initializes from GBD prevalence/incidence anchors.
                               # Warmup is disabled by default because it can move
                               # the initial state distribution away from the GBD
                               # 2023 anchor before projection starts.
  ages         = 0:100,
  sexes        = c("Female", "Male"),
  cycle_length = 1L,          # annual cycles
  tol_calib    = 0.05         # max |rel error| at validation ages (5%)
)

# ── CVD CAUSE MAP (authoritative — used by scripts 04, 05, 06) ────────────────
# Maps module_id → GBD cause label string.
CVD_CAUSE_MAP <- list(
  ihd             = "Ischemic heart disease",
  ischemic_stroke = "Ischemic stroke",
  ich             = "Intracerebral hemorrhage",
  hhd             = "Hypertensive heart disease"
)

# Maps module_id → engine type.
#   "tpm"              — 5-state Markov (well/incident/prevalent/dead_cause/dead_bg)
#   "direct_mortality" — mx_cause = mx_WPP × frac_cause; no TPM required
# HHD uses direct_mortality: GBD does not provide incidence and the prevalence-
# mortality relationship makes TPM calibration structurally impossible.
CVD_MODEL_TYPE <- list(
  ihd             = "tpm",
  ischemic_stroke = "tpm",
  ich             = "tpm",
  hhd             = "direct_mortality"
)

# ── GENERIC HELPERS ───────────────────────────────────────────────────────────

# Build and validate a cause_spec list.
make_cause_spec <- function(cause_id, cause_name, gbd_cause_name, group,
                            markov_cat = NA,
                            states, transitions, calibration, interventions,
                            notes = NULL) {
  stopifnot(is.character(states), length(states) >= 2)

  for (tr in transitions) {
    bad_from <- setdiff(tr$from, states)
    bad_to   <- setdiff(tr$to,   states)
    if (length(bad_from) > 0)
      stop(cause_id, ": transition 'from' '", paste(bad_from, collapse = ","),
           "' not in states")
    if (length(bad_to) > 0)
      stop(cause_id, ": transition 'to' '", paste(bad_to, collapse = ","),
           "' not in states")
  }

  absorbing     <- states[grepl("^dead", states, ignore.case = TRUE)]
  non_absorbing <- setdiff(states, absorbing)

  structure(
    list(
      cause_id       = cause_id,
      cause_name     = cause_name,
      gbd_cause_name = gbd_cause_name,
      group          = group,
      markov_cat     = markov_cat,
      states         = states,
      n_states       = length(states),
      absorbing      = absorbing,
      non_absorbing  = non_absorbing,
      dead_cause_idx = which(states == "dead_cause"),
      dead_bg_idx    = which(states == "dead_background"),
      transitions    = transitions,
      calibration    = calibration,
      interventions  = interventions,
      notes          = notes
    ),
    class = "cause_spec"
  )
}

# Build a transition slot descriptor.
make_transition <- function(id, from, to, mechanism,
                            age_varying  = TRUE,
                            sex_varying  = TRUE,
                            year_varying = FALSE,
                            data_source  = NULL,
                            params       = list(),
                            notes        = NULL) {
  list(id = id, from = from, to = to, mechanism = mechanism,
       age_varying  = age_varying,  sex_varying  = sex_varying,
       year_varying = year_varying, data_source  = data_source,
       params = params, notes = notes)
}

# Build an intervention slot descriptor.
make_intervention <- function(id, label, mechanism,
                              transition_ids, effect_params, scale_up,
                              data_sources = list(), notes = NULL) {
  list(id = id, label = label, mechanism = mechanism,
       transition_ids = transition_ids, effect_params = effect_params,
       scale_up = scale_up, data_sources = data_sources, notes = notes)
}

###############################################################################
# CVD MODULE SPECS
# 4-cause care-cascade framework: Well → Incident → Prevalent → Dead_cause
# All 4 CVD causes share the same 5-state structure; differ only in
# calibration targets and CF reduction parameters.
###############################################################################

# CF reduction per unit of incremental coverage above baseline.
# Source: Ettehad et al. Lancet 2016; CVD model cf_etihad table.
CVD_CF_REDUCTION <- list(
  ihd             = 0.24,
  ischemic_stroke = 0.36,
  ich             = 0.76,
  hhd             = 0.20
)

CVD_STATES <- c("well", "incident", "prevalent", "dead_cause", "dead_background")

make_cvd_transitions <- function() {
  list(
    make_transition(
      id = "well_to_incident", from = "well", to = "incident",
      mechanism = "GBD_incidence", year_varying = TRUE,
      notes = "Annual incidence probability ≈ 1 - exp(-ir). Calibrated to GBD."
    ),
    make_transition(
      id = "incident_to_prevalent", from = "incident", to = "prevalent",
      mechanism = "complement",
      notes = "Survive acute event, enter prevalent pool. = 1 - p_inc_dead - p_inc_bgmx."
    ),
    make_transition(
      id = "incident_to_dead_cause", from = "incident", to = "dead_cause",
      mechanism = "GBD_CFR",
      notes = "Acute CFR = GBD mx_cause / GBD incidence at calibration year."
    ),
    make_transition(
      id = "prevalent_to_dead_cause", from = "prevalent", to = "dead_cause",
      mechanism = "GBD_CFR",
      params = list(cfr_type = "prevalent"),
      notes = paste("Prevalent CFR, back-solved from GBD steady-state:",
                    "p_prev_dead = deaths_remaining / prev (residual method).")
    ),
    make_transition(
      id = "any_to_dead_background",
      from = c("well", "incident", "prevalent"), to = "dead_background",
      mechanism = "bgmx", year_varying = TRUE,
      notes = "Background mortality = mx_WPP - mx_cause. Applied to all live rows."
    )
  )
}

make_cvd_calibration <- function() {
  list(
    primary_target    = "GBD_mx_cause",
    secondary_targets = c("GBD_prevalence", "GBD_incidence"),
    method            = "direct_then_backsolve",
    validation_ages   = 30:69,
    tol               = SHARED_PARAMS$tol_calib
  )
}

make_hypertension_intervention <- function(cause_id) {
  make_intervention(
    id    = "hypertension_control",
    label = "Hypertension diagnosis, treatment and control",
    mechanism = "care_cascade",
    transition_ids = c("well_to_incident", "incident_to_dead_cause",
                       "prevalent_to_dead_cause"),
    effect_params = list(
      ir_rr_per_unit_control   = 0.17,
      cf_reduction_per_control = CVD_CF_REDUCTION[[cause_id]],
      coverage_type            = "incremental"
    ),
    scale_up = list(start_year = 2025L, end_year = 2030L,
                    baseline_cov = 0.40, target_cov = 0.70, fn = "linear"),
    data_sources = list(effect   = "Ettehad et al. Lancet 2016",
                        coverage = "GBD 2023 / WHO NCD Progress Monitor 2023")
  )
}

make_sodium_intervention <- function() {
  make_intervention(
    id    = "sodium_reduction", label = "Dietary sodium reduction",
    mechanism = "PAF_shift", transition_ids = "well_to_incident",
    effect_params = list(risk_id = "high_sodium", rr_per_unit = 1.06,
                         unit_size = 1, tmrel = 3, direction = "harmful",
                         delta_g_day = -1.0),
    scale_up = list(start_year = 2025L, end_year = 2030L, fn = "linear"),
    data_sources = list(rr = "GBD 2019 CRA (Afshin et al., Lancet 2019)")
  )
}

ihd_spec <- make_cause_spec(
  cause_id = "ihd", cause_name = "Ischemic heart disease",
  gbd_cause_name = "Ischemic heart disease", group = "CVD",
  states = CVD_STATES, transitions = make_cvd_transitions(),
  calibration = make_cvd_calibration(),
  interventions = list(make_hypertension_intervention("ihd"),
                       make_sodium_intervention())
)

ischemic_stroke_spec <- make_cause_spec(
  cause_id = "ischemic_stroke", cause_name = "Ischemic stroke",
  gbd_cause_name = "Ischemic stroke", group = "CVD",
  states = CVD_STATES, transitions = make_cvd_transitions(),
  calibration = make_cvd_calibration(),
  interventions = list(make_hypertension_intervention("ischemic_stroke"),
                       make_sodium_intervention())
)

ich_spec <- make_cause_spec(
  cause_id = "ich", cause_name = "Intracerebral hemorrhage",
  gbd_cause_name = "Intracerebral hemorrhage", group = "CVD",
  states = CVD_STATES, transitions = make_cvd_transitions(),
  calibration = make_cvd_calibration(),
  interventions = list(make_hypertension_intervention("ich"))
)

hhd_spec <- make_cause_spec(
  cause_id = "hhd", cause_name = "Hypertensive heart disease",
  gbd_cause_name = "Hypertensive heart disease", group = "CVD",
  states = CVD_STATES, transitions = make_cvd_transitions(),
  calibration = make_cvd_calibration(),
  interventions = list(make_hypertension_intervention("hhd"))
)

###############################################################################
# CANCER MODULE SPECS
#
# Three state structures:
#   Cat 1 — Simple:          Well → Sick → Dead_cause / Dead_bg        (11 causes)
#   Cat 2 — Staged:          Well → Local → Regional → Distant → Dead  ( 3 causes)
#   Cat 3 — Staged+Precancer: adds Precancer before Local               ( 2 causes)
###############################################################################

CANCER_STATES <- list(
  cat1 = c("well", "sick",      "dead_cause", "dead_background"),
  cat2 = c("well", "local", "regional", "distant", "dead_cause", "dead_background"),
  cat3 = c("well", "precancer", "local", "regional", "distant",
            "dead_cause", "dead_background")
)

make_cancer_transitions_cat1 <- function() {
  list(
    make_transition("well_to_sick", "well", "sick",
                    mechanism = "GBD_incidence", year_varying = TRUE),
    make_transition("sick_to_dead_cause", "sick", "dead_cause",
                    mechanism = "GBD_CFR",
                    notes = "CFR = GBD mx / GBD prevalence at calib year."),
    make_transition("any_to_dead_background", c("well", "sick"), "dead_background",
                    mechanism = "bgmx", year_varying = TRUE)
  )
}

make_cancer_transitions_cat2 <- function() {
  list(
    make_transition("well_to_local",    "well",     "local",    mechanism = "GBD_incidence", year_varying = TRUE),
    make_transition("local_to_regional","local",    "regional", mechanism = "GBD_progression"),
    make_transition("regional_to_distant","regional","distant", mechanism = "GBD_progression"),
    make_transition("local_to_dead_cause",    "local",    "dead_cause",
                    mechanism = "GBD_stage_CFR", params = list(stage = "local")),
    make_transition("regional_to_dead_cause", "regional", "dead_cause",
                    mechanism = "GBD_stage_CFR", params = list(stage = "regional")),
    make_transition("distant_to_dead_cause",  "distant",  "dead_cause",
                    mechanism = "GBD_stage_CFR", params = list(stage = "distant")),
    make_transition("any_to_dead_background",
                    c("well", "local", "regional", "distant"), "dead_background",
                    mechanism = "bgmx", year_varying = TRUE)
  )
}

make_cancer_transitions_cat3 <- function() {
  c(
    list(
      make_transition("well_to_precancer", "well", "precancer",
                      mechanism = "GBD_incidence", year_varying = TRUE,
                      notes = "CIN incidence (precancer pool initiation)."),
      make_transition("precancer_to_local", "precancer", "local",
                      mechanism = "GBD_progression",
                      notes = "CIN progression back-calculated from cancer incidence.")
    ),
    make_cancer_transitions_cat2()[-1]   # drop well_to_local (replaced above)
  )
}

make_cancer_calibration <- function(cat) {
  list(primary_target  = "GBD_mx_cause",
       method          = if (cat == 1) "direct" else "stage_steady_state",
       validation_ages = 15:69,
       tol             = SHARED_PARAMS$tol_calib)
}

make_screening_intervention <- function(cause_id, local_shift = 0.20) {
  make_intervention(
    id = "screening", label = paste("Cancer screening (stage shift)", cause_id),
    mechanism = "stage_shift",
    transition_ids = c("distant_to_dead_cause", "regional_to_dead_cause"),
    effect_params  = list(local_shift = local_shift),
    scale_up = list(start_year = 2025L, end_year = 2030L,
                    baseline_cov = 0.15, target_cov = 0.70, fn = "linear")
  )
}

make_treatment_intervention <- function(bsln_adh = 0.35, trgt_adh = 0.70) {
  make_intervention(
    id = "treatment_adherence", label = "Cancer treatment adherence improvement",
    mechanism = "adherence_CFR",
    transition_ids = c("local_to_dead_cause","regional_to_dead_cause",
                       "distant_to_dead_cause"),
    effect_params  = list(bsln_adh = bsln_adh, trgt_adh = trgt_adh,
                           rr_lcl = 0.60, rr_rgn = 0.55, rr_dst = 0.70),
    scale_up = list(start_year = 2025L, end_year = 2030L, fn = "linear")
  )
}

# Cancer Cat 3 (staged + precancer)
cervical_ca_spec <- make_cause_spec(
  cause_id = "cervical_ca", cause_name = "Cervical cancer",
  gbd_cause_name = "Cervical cancer", group = "Cancer", markov_cat = 3L,
  states = CANCER_STATES$cat3, transitions = make_cancer_transitions_cat3(),
  calibration = make_cancer_calibration(3L),
  interventions = list(
    make_intervention(
      id = "hpv_vaccination", label = "HPV vaccination",
      mechanism = "incidence_reduction", transition_ids = "well_to_precancer",
      effect_params = list(hpv_attributable = 0.70, vaccine_efficacy = 0.90),
      scale_up = list(start_year = 2025L, end_year = 2030L,
                      baseline_cov = 0.10, target_cov = 0.70, fn = "linear")
    ),
    make_screening_intervention("cervical_ca"),
    make_treatment_intervention()
  )
)

crc_spec <- make_cause_spec(
  cause_id = "crc", cause_name = "Colon and rectum cancer",
  gbd_cause_name = "Colon and rectum cancer", group = "Cancer", markov_cat = 3L,
  states = CANCER_STATES$cat3, transitions = make_cancer_transitions_cat3(),
  calibration = make_cancer_calibration(3L),
  interventions = list(make_screening_intervention("crc"),
                       make_treatment_intervention())
)

# Cancer Cat 2 (staged, no precancer)
breast_ca_spec <- make_cause_spec(
  cause_id = "breast_ca", cause_name = "Breast cancer",
  gbd_cause_name = "Breast cancer", group = "Cancer", markov_cat = 2L,
  states = CANCER_STATES$cat2, transitions = make_cancer_transitions_cat2(),
  calibration = make_cancer_calibration(2L),
  interventions = list(make_screening_intervention("breast_ca"),
                       make_treatment_intervention())
)

# Cancer Cat 1 (simple 4-state — 13 causes)
make_simple_cancer <- function(cause_id, cause_name, gbd_name,
                               bsln_adh = 0.35, trgt_adh = 0.60) {
  make_cause_spec(
    cause_id = cause_id, cause_name = cause_name,
    gbd_cause_name = gbd_name, group = "Cancer", markov_cat = 1L,
    states = CANCER_STATES$cat1, transitions = make_cancer_transitions_cat1(),
    calibration = make_cancer_calibration(1L),
    interventions = list(
      make_treatment_intervention(bsln_adh = bsln_adh, trgt_adh = trgt_adh)
    )
  )
}

bladder_ca_spec    <- make_simple_cancer("bladder_ca",    "Bladder cancer",         "Bladder cancer")
esophageal_ca_spec <- make_simple_cancer("esophageal_ca", "Esophageal cancer",      "Esophageal cancer")
liver_ca_spec      <- make_simple_cancer("liver_ca",      "Liver cancer",           "Liver cancer")
nasopharynx_spec   <- make_simple_cancer("nasopharynx_ca","Nasopharynx cancer",     "Nasopharynx cancer")
pharynx_ca_spec    <- make_simple_cancer("pharynx_ca",    "Other pharynx cancer",   "Other pharynx cancer")
oral_ca_spec       <- make_simple_cancer("oral_ca",       "Lip and oral cavity cancer","Lip and oral cavity cancer")
ovarian_ca_spec    <- make_simple_cancer("ovarian_ca",    "Ovarian cancer",         "Ovarian cancer")
pancreatic_ca_spec <- make_simple_cancer("pancreatic_ca", "Pancreatic cancer",      "Pancreatic cancer")
stomach_ca_spec    <- make_simple_cancer("stomach_ca",    "Stomach cancer",         "Stomach cancer")
thyroid_ca_spec    <- make_simple_cancer("thyroid_ca",    "Thyroid cancer",         "Thyroid cancer")
uterine_ca_spec    <- make_simple_cancer("uterine_ca",    "Uterine cancer",         "Uterine cancer")
lung_ca_spec       <- make_simple_cancer("lung_ca",
                                          "Tracheal, bronchus, and lung cancer",
                                          "Tracheal, bronchus, and lung cancer",
                                          bsln_adh = 0.30, trgt_adh = 0.60)
prostate_ca_spec   <- make_simple_cancer("prostate_ca",   "Prostate cancer",        "Prostate cancer")

###############################################################################
# OTHER NCD MODULE SPECS
# Simple 4-state: Well → Sick → Dead_cause / Dead_bg
###############################################################################

OTHER_STATES <- c("well", "sick", "dead_cause", "dead_background")

make_other_transitions <- function() {
  list(
    make_transition("well_to_sick",       "well", "sick",      mechanism = "GBD_incidence", year_varying = TRUE),
    make_transition("sick_to_dead_cause", "sick", "dead_cause",mechanism = "GBD_CFR"),
    make_transition("any_to_dead_background", c("well","sick"), "dead_background",
                    mechanism = "bgmx", year_varying = TRUE)
  )
}

t2d_spec <- make_cause_spec(
  cause_id = "t2d", cause_name = "Diabetes mellitus type 2",
  gbd_cause_name = "Diabetes mellitus type 2", group = "Other NCD",
  states = OTHER_STATES, transitions = make_other_transitions(),
  calibration = list(primary_target = "GBD_mx_cause", method = "direct",
                     validation_ages = 30:69, tol = SHARED_PARAMS$tol_calib),
  interventions = list()
)

copd_spec <- make_cause_spec(
  cause_id = "copd", cause_name = "Chronic obstructive pulmonary disease",
  gbd_cause_name = "Chronic obstructive pulmonary disease", group = "Other NCD",
  states = OTHER_STATES, transitions = make_other_transitions(),
  calibration = list(primary_target = "GBD_mx_cause", method = "direct",
                     validation_ages = 30:69, tol = SHARED_PARAMS$tol_calib),
  interventions = list()
)

dementia_spec <- make_cause_spec(
  cause_id = "dementia", cause_name = "Alzheimer's disease and other dementias",
  gbd_cause_name = "Alzheimer's disease and other dementias", group = "Other NCD",
  states = OTHER_STATES, transitions = make_other_transitions(),
  calibration = list(primary_target = "GBD_mx_cause", method = "direct",
                     validation_ages = 40:69, tol = SHARED_PARAMS$tol_calib),
  interventions = list()
)

###############################################################################
# CAUSE REGISTRY — single entry point for all 23 causes
###############################################################################

CAUSE_REGISTRY <- list(
  # CVD (4 causes)
  ihd             = ihd_spec,
  ischemic_stroke = ischemic_stroke_spec,
  ich             = ich_spec,
  hhd             = hhd_spec,
  # Cancer Cat 3 (staged + precancer)
  cervical_ca     = cervical_ca_spec,
  crc             = crc_spec,
  # Cancer Cat 2 (staged)
  breast_ca       = breast_ca_spec,
  # Cancer Cat 1 (simple — 13 causes)
  bladder_ca      = bladder_ca_spec,
  esophageal_ca   = esophageal_ca_spec,
  liver_ca        = liver_ca_spec,
  nasopharynx_ca  = nasopharynx_spec,
  pharynx_ca      = pharynx_ca_spec,
  oral_ca         = oral_ca_spec,
  ovarian_ca      = ovarian_ca_spec,
  pancreatic_ca   = pancreatic_ca_spec,
  stomach_ca      = stomach_ca_spec,
  thyroid_ca      = thyroid_ca_spec,
  uterine_ca      = uterine_ca_spec,
  lung_ca         = lung_ca_spec,
  prostate_ca     = prostate_ca_spec,
  # Other NCD
  t2d             = t2d_spec,
  copd            = copd_spec,
  dementia        = dementia_spec
)

# ── REGISTRY HELPERS ──────────────────────────────────────────────────────────
get_cause_spec           <- function(id) {
  s <- CAUSE_REGISTRY[[id]]
  if (is.null(s)) stop("Cause '", id, "' not in CAUSE_REGISTRY")
  s
}
get_causes_by_group      <- function(group) names(Filter(function(s) s$group == group, CAUSE_REGISTRY))
get_causes_by_markov_cat <- function(cat)   names(Filter(function(s) !is.na(s$markov_cat) && s$markov_cat == cat, CAUSE_REGISTRY))
get_n_states             <- function(id)    get_cause_spec(id)$n_states
get_transition           <- function(spec, tr_id) {
  tr <- Filter(function(t) t$id == tr_id, spec$transitions)
  if (length(tr) == 0) stop("Transition '", tr_id, "' not in '", spec$cause_id, "'")
  tr[[1]]
}
get_intervention <- function(spec, int_id) {
  iv <- Filter(function(i) i$id == int_id, spec$interventions)
  if (length(iv) == 0) stop("Intervention '", int_id, "' not in '", spec$cause_id, "'")
  iv[[1]]
}

# ── SUMMARY ON LOAD ───────────────────────────────────────────────────────────
message("── R/cause_registry.R loaded ───────────────────────────────────────────")
message(sprintf("  Total causes: %d  |  CVD: %d  |  Cancer: %d  |  Other NCD: %d",
  length(CAUSE_REGISTRY),
  length(get_causes_by_group("CVD")),
  length(get_causes_by_group("Cancer")),
  length(get_causes_by_group("Other NCD"))))
message(sprintf(
  "  V1 active modules: %s",
  paste(c(names(CVD_CAUSE_MAP), "cervical_ca"), collapse = ", ")
))
