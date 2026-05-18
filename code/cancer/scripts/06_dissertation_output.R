# Results

# Load functions
source("R/library.R")

load(file = paste0("R/outputs/RData/",
                   country_select,
                   "/intv/",
                   country_select, "_intv.RData"))


### Cost calculations----------------------------------------------------------#

## Read cost data
cancer_costs <- readxl::read_excel("../../DCP/Cancer-commission/R/inputs/Total costs per cancer per country.xlsm",
                                   sheet = "Markov-Country Costs",
                                   range = "A1:S190") %>%
      pivot_longer(cols = 4:last_col(), names_to = "cause", values_to = "cost") %>%
      select(location = 2, gni = 3, cause, cost) %>%
      mutate(cause = gsub(pattern = "\\(C2\\)|\\(C3\\)", replacement = "", cause),
             cause = trimws(cause))

## Extract treatment initiation coverage in baseline scenario
bsln_cov <- intv_modif_rr %>%
      filter(year == sim_start_year) %>%
      select(cause, cov = init_ttt, adh = adh_ttt)

## Extract treatment initiation coverage in intervention scenario
intv_cov <- intv_modif_rr %>%
      select(cause, year, cov = init_ttt, adh = adh_ttt)

## Calculate costs for initiating treatment
tot_pop_proj <- full_join(
      # Baseline scenario
      x  = left_join(
            x  = left_join(
                  x  = bsln_pop_projection,
                  y  = bsln_markov_trace,
                  by = join_by(age, year, sex, location)
            ),
            y  = bsln_cov,
            by = join_by(cause)
      ),

      # Intervention scenario
      y  = left_join(
            x  = left_join(
                  x  = intv_pop_projection,
                  y  = intv_markov_trace,
                  by = join_by(age, year, sex, location)
            ),
            y  = intv_cov,
            by = join_by(cause, year)
      ),

      by = join_by(age, year, sex, location,
                   Nx, mx, fx, Dx, iteration, scen, #cohort,
                   cause, pwell, pprc, plcl, prgn, pdst, pcdx, pbgdx,
                   inc_rate, cdeath_rate,
                   lplcl, lpcdx,
                   cov, adh)
) %>%
      mutate(cohort = year - age, .after = scen)

cost_output <- left_join(
      x  = tot_pop_proj,
      y  = cancer_costs,
      by = join_by(location, cause)
) %>%
      # mutate(pop_cost = Nx * (plcl + prgn + pdst) * cov * cost) %>%
      mutate(tpop_cost =
                   (Nx * plcl * cov * cost) + # proportion local
                   (Nx * prgn * cov * cost) + # proportion regional
                   (Nx * pdst * cov * cost)   # proportion distant
      )

# any(endsWith(names(cost_output), ".x|.y"))

### Financial Risk Protection Analysis-----------------------------------------#

## Combine both GNI PC and Gini Ceofficient data
econ_data <- full_join(x  = gni_per_cap %>% rename(year_pc = year),
                       y  = gini_coef_dat %>% rename(year_coef = year),
                       by = join_by(country, code)) %>%
      as.data.frame()

## Subset to complete data
econ_data_cmplt <- econ_data[complete.cases(econ_data),]

## Estimate mu and sigma (parameters for the log-normal distribution)
econ_data_cmplt_sim <- econ_data_cmplt %>%
      split(.$country) %>%
      map_df(.x = .,
             .f = ~ sim_gni_gini(dat = .x))

## Combine with simulated data to incorporate both mu and sigma (log-norm dist)
pov_line_dat <- left_join(x  = pov_ratio_dat,
                          y  = econ_data_cmplt_sim,
                          by = join_by(country)) %>%

      # Calculate country-specific poverty line value using:
      # poverty proportion, mu, and sigma
      split(.$country) %>%
      map_df(.x = .,
             .f = function(dat) {
                   dat %>%
                         mutate(pov_line = qlnorm(p       = dat$pov_prop,
                                                  meanlog = dat$lognorm_mu,
                                                  sdlog   = dat$lognorm_sigma))
             }
      )



col_5 <- c("#c7522a", "#FF7F24", "#e5c185", "#96BB9F", "#006464")


# Total deaths (baseline)---approach #1
#------------------------------------There is a problem with the lpcdx approach!
# Check lpcdx for 85-YOs in 2019
cost_output %>%
      filter(cause %in% target_cancers) %>%
      # summary()
      group_by(cause, scen) %>%
      reframe(cumm_dths = sum(lpcdx * Nx)) %>%
      view()
      group_by(scen) %>%
      reframe(tot_dth = sum(cumm_dths)) %>%
      # view()
      reframe(dth_diff = diff(tot_dth),
              rrr = (tot_dth[scen == "bsln"] - tot_dth[scen == "intv"]) / tot_dth[scen == "bsln"])


# Total deaths---approach #2
dth_avrt <- cost_output %>%
      filter(cause %in% target_cancers) %>%
      filter(year == 2050) %>%
      group_by(cause, scen) %>%
      reframe(cumm_dths = sum(pcdx * Nx)) %>%
      # view()
      group_by(scen) %>%
      reframe(tot_dth = sum(cumm_dths)) %>%
      spread(scen, tot_dth) %>%
      # mutate(across(2, ~ scales::label_comma()(.)))
      # view()
      mutate(dth_avrt = bsln - intv,
             rrr = dth_avrt / bsln)

# Deaths by cancer type
cum_dth_by_cnx <- cost_output %>%
      filter(cause %in% target_cancers) %>%
      filter(year == 2050) %>%
      # filter(scen == "bsln") %>%
      group_by(cause, scen) %>%
      reframe(cum_dth = sum(pcdx * Nx)) %>%
      group_by(scen) %>%
      mutate(prop_dth = scales::label_percent(accuracy = 0.1)(cum_dth / sum(cum_dth))) %>%
      arrange(desc(parse_number(prop_dth))) %>%
      ungroup()

# Deaths table
cost_output %>%
      filter(cause %in% target_cancers) %>%
      filter(year == 2050) %>%
      group_by(cause, scen) %>%
      reframe(cum_dth = sum(pcdx * Nx)) %>%
      group_by(scen) %>%
      mutate(prop_dth = scales::label_percent(accuracy = 0.1)(cum_dth / sum(cum_dth))) %>%
      arrange(desc(parse_number(prop_dth))) %>%
      ungroup() %>%
      pivot_wider(id_cols = cause,
                  names_from = scen,
                  values_from = c(cum_dth, prop_dth)) %>%
      select(1, 2, 4, 3, 5) %>%
      mutate(incr_n = cum_dth_bsln - cum_dth_intv) %>%
      mutate(prop_incr = scales::label_percent(accuracy = 0.1)(incr_n / sum(incr_n))) %>%
      # https://stackoverflow.com/questions/39507019/add-margin-row-totals-in-dplyr-chain
      janitor::adorn_totals("row") %>%
      mutate(across(c(3, 5, 7), ~ ifelse(row_number() == 6, "100%", .)),
             across(c(2, 4, 6), ~ scales::label_comma()(.)),
             across(c(3, 5, 7), ~ str_c(., ")"))) %>%
      # names()
      unite(cum_dth_bsln, cum_dth_bsln, prop_dth_bsln, sep = " (", remove = TRUE) %>%
      unite(cum_dth_intv, cum_dth_intv, prop_dth_intv, sep = " (", remove = TRUE) %>%
      unite(incr_n, incr_n, prop_incr, sep = " (", remove = TRUE)


# Literature distribution: Esoph(58.2%), liver (30.0%), cervical (6.8%), breast (4.4%), and prostate (0.6%)

# Deaths averted by cancer type
dth_avrt_by_cnx <- cum_dth_by_cnx %>%
      group_by(cause) %>%
      reframe(dth_avrt = cum_dth[scen == "bsln"] - cum_dth[scen == "intv"]) %>%
      mutate(prop_dth_avrt = scales::label_percent(accuracy = 0.01)(dth_avrt / sum(dth_avrt))) %>%
      arrange(desc(parse_number(prop_dth_avrt))) %>%
      ungroup()

# Deaths averted by cancer type
dth_avrt_by_cnx_position <- dth_avrt_by_cnx %>%
      mutate(position = case_match(cause,
                                   dth_avrt_by_cnx$cause[1] ~ 2300,
                                   dth_avrt_by_cnx$cause[2] ~ 1450,
                                   dth_avrt_by_cnx$cause[3] ~ 750,
                                   dth_avrt_by_cnx$cause[4] ~ 300,
                                   .default = NA))

# Deaths averted by cancer type: Area plot
cost_output %>%
      filter(cause %in% target_cancers) %>%
      group_by(cause, year, scen) %>%
      reframe(cum_dth = sum(Nx * pcdx)) %>%
      # reframe(cum_dth = sum(cdeath_rate)) %>%
      pivot_wider(id_cols = cause:year, names_from = scen, values_from = cum_dth) %>%
      group_by(cause, year) %>%
      reframe(dth_avrt = bsln - intv) %>%

      # mutate(cause = factor(cause, levels = dth_avrt_by_cnx$cause)) %>%
      # view()
      # filter(cause != cause3[1]) %>%

      ggplot(aes(x = year, y = dth_avrt, fill = cause)) +
      geom_area() +
      # geom_text(data = dth_avrt_by_cnx_position,
      #           aes(x = 2048, y = position,
      #               label = prop_dth_avrt),
      #           # position = position_stack(vjust = 0.5),
      #           color = "white") +
      labs(x = "Year", y = "Cumulative deaths averted\nby cancer type (thousands)", fill = NULL) +
      scale_y_continuous(labels = scales::label_comma(scale = 1e-3),
                         # trans = "log10"
                         ) +
      scale_fill_manual(values = col_5) +
      # theme_bw() +
      theme_caviz +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      theme(legend.position = "bottom",
            aspect.ratio = 1 / 2,
            panel.grid.major.x = element_blank())

ggsave(filename = "R/outputs/cancer_dth_by_cause.png", width = 5, height = 4, dpi = "print")


# Mortality rate
cost_output %>%
      filter(cause %in% target_cancers,
             year == 2050) %>%
      group_by(year, scen) %>%
      reframe(mort_rate = sum(Nx * lpcdx) / sum(Nx)) %>%
      spread(scen, mort_rate) %>%
      select(!year) %>%
      mutate(diff = bsln - intv,
             rrr  = diff / bsln,
             across(1:3, ~ scales::label_number(scale = 1e5, accuracy = 0.01)(.)))

cost_output %>%
      filter(cause %in% target_cancers) %>%
      group_by(year, scen) %>%
      reframe(mort_rate = sum(Nx * cdeath_rate) / sum(Nx)) %>%
      ggplot(aes(x = year, y = mort_rate, color = scen)) +
      geom_line() +
      scale_y_continuous(name = "Mortality rate (per 100,000)",
                         labels = scales::label_number(scale = 1e5)) +
      labs(x = "Year") +
      theme_caviz +
      theme(legend.position = "bottom")

# Mortality rate by cause
cost_output %>%
      filter(cause %in% target_cancers) %>%
      # filter(year == max(year)) %>%
      # summary()
      group_by(year, scen, cause) %>%
      reframe(mort_rate = sum(Nx * cdeath_rate) / sum(Nx)) %>%
      mutate(cause = recode_values(cause,
                                   target_cancers[1] ~ "Breast",
                                   target_cancers[2] ~ "Cervix",
                                   target_cancers[3] ~ "Esophagus",
                                   target_cancers[4] ~ "Liver",
                                   target_cancers[5] ~ "Prostate")) %>%
      # reframe(mr_diff = diff(mort_rate),
      #         mrr = (mort_rate[scen == "bsln"] - mort_rate[scen == "intv"]) / mort_rate[scen == "bsln"])


      ggplot(aes(x = year, y = mort_rate, color = scen)) +
      geom_line() +
      scale_y_continuous(name = "Mortality rate (per 100,000)",
                         labels = scales::label_number(scale = 1e5),
                         n.breaks = 4) +
      facet_wrap(. ~ cause, scale = "free_y") +
      labs(x = "Year") +
      theme_caviz +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

gbd$Uganda %>%
      filter(cause %in% target_cancers,
             measure == "Deaths",
             metric  == "Rate") %>%
      group_by(year, cause) %>%
      reframe(mort_rate = sum(val, na.rm = TRUE)) %>%

      ggplot(aes(x = year, y = mort_rate)) +
      geom_line() +
      scale_y_continuous(name = "Mortality rate (per 100,000)",
                         labels = scales::label_number(scale = 1e-5)) +
      facet_wrap(. ~ cause) +
      labs(x = "Year") +
      theme_caviz +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

full_join(
      x  = cost_output %>%
            filter(cause %in% target_cancers) %>%
            group_by(year, scen, cause) %>%
            reframe(mort_rate = sum(Nx * lpcdx) / sum(Nx) * 1e5) %>%
            mutate(source = "sim"),

      y  = gbd %>%
            filter(cause %in% target_cancers,
                   measure == "Deaths",
                   metric  == "Rate") %>%
            group_by(year, cause) %>%
            reframe(mort_rate = sum(val, na.rm = TRUE) / 1e5) %>%
            mutate(source = "gbd",
                   scen   = "bsln"),

      by = join_by(year, cause, mort_rate, scen, source)
) %>%
      # view()
      ggplot(aes(x = year, y = mort_rate, color = source, linetype = scen)) +
      geom_line() +
      scale_y_continuous(name = "Mortality rate (per 100,000)",
                         labels = scales::label_number(scale = 1)) +
      facet_wrap(. ~ cause, scale = "free_y") +
      labs(x = "Year") +
      theme_caviz +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Tot cost
tot_country_cost <- cost_output %>%
      filter(year == 2050,
             cause %in% target_cancers) %>% #view()
      group_by(scen, year) %>%
      reframe(cumm_cost = sum(tpop_cost)) %>%
      pivot_wider(id_cols = year, names_from = scen, values_from = cumm_cost) %>%
      mutate(delta = intv - bsln) %>%
      select(delta) %>%
      pull()

scales::label_comma()(tot_country_cost)

# Average incremental cost
avg_cost_per_capita <- cost_output %>%
      filter(year == 2050,
             cause %in% target_cancers) %>%
      group_by(scen, year) %>%
      # reframe(cumm_cost = sum(tpop_cost)) %>%
      # pivot_wider(id_cols = year, names_from = scen, values_from = cumm_cost) %>%
      reframe(cumm_cost = sum(tpop_cost)) %>%
      pivot_wider(id_cols = year, names_from = scen, values_from = cumm_cost) %>%
      mutate(delta = intv - bsln) %>%
      summarise(avg_incr_cost = delta / length(2025:2050)) %>%
      pull() #%>%

scales::label_comma()(avg_cost_per_capita)

# Extract Uganda's GDP and total population
uganda_gdp <- WDI::WDI(country = "UGA",
                       indicator = c("gdp_per_capita" = "NY.GDP.PCAP.KD",
                                     "population"     = "SP.POP.TOTL"),
                       start = 2019, end = 2025)

6317532 / 41117856
6317532 / 41117856 / uganda_gdp[5, 5]

# Average annual cost per capita
scales::label_dollar()(avg_cost_per_capita / uganda_gdp[6, 6])

# Average cost % of GDP
scales::label_percent(accuracy = 0.001)(avg_cost_per_capita / (uganda_gdp[6, 5] * uganda_gdp[6, 6]))

# Cost per death averted
scales::label_dollar()(tot_country_cost / dth_avrt$dth_avrt)

# Cost share by cancer type
cost_output %>%
      filter(#year == 2050,
             year > 2025,
             cause %in% target_cancers) %>%
      group_by(scen, cause, year) %>%
      reframe(cumm_cost = sum(tpop_cost)) %>%
      pivot_wider(id_cols = c(cause, year),
                  names_from = scen,
                  values_from = cumm_cost) %>%
      mutate(delta = intv - bsln) %>%
      # filter(scen == "intv") %>%
      group_by(cause) %>%
      reframe(avg_incr_cost_share = mean(delta)) %>%
      mutate(prop = avg_incr_cost_share / sum(avg_incr_cost_share)) %>%
      arrange(desc(prop))

#----------another approach: close enough results
cost_output %>%
      filter(year == 2050,
             # year > 2025,
             cause %in% target_cancers) %>%
      group_by(scen, cause) %>%
      reframe(cumm_cost = sum(tpop_cost)) %>%
      pivot_wider(id_cols = cause,
                  names_from = scen,
                  values_from = cumm_cost) %>%
      mutate(delta = intv - bsln) %>%
      # filter(scen == "intv") %>%
      # group_by(cause) %>%
      mutate(cost_share = delta / sum(delta)) %>%
      select(cause, cost_share) %>%
      arrange(desc(cost_share)) %>%
      mutate(cost_share = scales::label_percent(accuracy = 0.001)(cost_share))


# Total incremental cost
tot_cost <- cost_output %>%
      filter(year > 2025,
             cause %in% target_cancers) %>%
      group_by(scen, year) %>%
      reframe(cumm_cost = sum(tpop_cost)) %>%
      pivot_wider(id_cols = year, names_from = scen, values_from = cumm_cost) %>%
      mutate(delta = intv - bsln) %>%
      group_by(year) %>%
      reframe(cumm_incr_cost = cumsum(delta)) %>%
      filter(year == max(year)) %>%
      select(2) %>% pull()


# Cost per death averted----------#
# 17892247 / 28337

tot_cost / sum(dth_avrt_by_cnx$dth_avrt) # SAME NUMBER, 2 different calculations!!!!!!! YEAH!!!!


## Gov Costs and Induced Poverty Cases--Scale-up Only

### FRP Calculations
## OOP remain
sim_output_oop_remain <- calc_pov(bsln_oop     = 0.8,
                                  trgt_oop_end = 0.8,
                                  trgt_oop_bgn = 0.8,
                                  pop_project  = cost_output,
                                  pov_dat_inpt = pov_line_dat)

oop_remain <- left_join(x  = sim_output_oop_remain$all_groups %>%
                              filter(state != "prc",
                                     scen == "intv") %>%
                              group_by(year, scen) %>%
                              reframe(ptn_cost = sum(Nx * prop * cov * cost * oop),
                                      gov_cost = sum(Nx * prop * cov * cost * (1 - oop))),
                        y  = sim_output_oop_remain$pov_cases %>%
                              group_by(year, scen) %>%
                              reframe(tot_pov  = sum(delta_pov)),
                        by = join_by(year, scen)) %>%
      arrange(year, scen) %>%

      group_by(scen) %>%
      mutate(cum_tot_cost = cumsum(ptn_cost + gov_cost),
             cum_gov_cost = cumsum(gov_cost),
             cum_pov  = cumsum(tot_pov),
             frp_scen = "80%") %>%
      ungroup()

## OOP remove
sim_output_oop_remove <- calc_pov(bsln_oop     = 0.8,
                                  trgt_oop_end = 0.0,
                                  trgt_oop_bgn = 0.8,
                                  pop_project  = cost_output,
                                  pov_dat_inpt = pov_line_dat)

oop_remove <- left_join(x  = sim_output_oop_remove$all_groups %>%
                              filter(state != "prc",
                                     scen == "intv") %>%
                              group_by(year, scen) %>%
                              reframe(ptn_cost = sum(Nx * prop * cov * cost * oop),
                                      gov_cost = sum(Nx * prop * cov * cost * (1 - oop))),
                        y  = sim_output_oop_remove$pov_cases %>%
                              group_by(year, scen) %>%
                              reframe(tot_pov  = sum(delta_pov)),
                        by = join_by(year, scen)) %>%
      arrange(year, scen) %>%

      group_by(scen) %>%
      mutate(cum_tot_cost = cumsum(ptn_cost + gov_cost),
             cum_gov_cost = cumsum(gov_cost),
             cum_pov  = cumsum(tot_pov),
             frp_scen = "0%") %>%
      ungroup()


# Costs table
sim_output_oop_remain$all_groups %>% #view()
      filter(year > 2025) %>%
      group_by(scen, cause, year) %>%
      reframe(ptn_cost = sum(Nx * prop * cost * cov * oop),
              gov_cost = sum(Nx * prop * cost * cov* (1 - oop))) %>%
      pivot_wider(id_cols = c(cause, year), names_from = scen,
                  values_from = c(ptn_cost, gov_cost)) %>%
      select(cause, year, starts_with("gov"), starts_with("ptn")) %>%
      mutate(gov_incr = gov_cost_intv - gov_cost_bsln,
             .after = gov_cost_intv) %>%
      mutate(ptn_incr = ptn_cost_intv - ptn_cost_bsln,
             .after = ptn_cost_intv) %>%
      filter(year == max(year)) %>%
      view()

cost_output %>%
      filter(year < 2025) %>%
      group_by(scen, cause, year) %>%
      reframe(ptn_cost = sum(Nx * (pprc + plcl + prgn + pdst) * cost * 0.8),
              gov_cost = sum(Nx * (pprc + plcl + prgn + pdst) * cost * (1 - 0.8))) %>%
      pivot_wider(id_cols = c(cause, year), names_from = scen,
                  values_from = c(ptn_cost, gov_cost)) %>%
      select(cause, year, starts_with("gov"), starts_with("ptn")) %>%
      view()
      reframe(tot_cost_bsln = )
      summarise(tot_cost_bsln       = last(cum_gov_cost_bsln) + last(cum_ptn_cost_bsln),
                tot_gov_share_bsln  = last(cum_gov_cost_bsln),
                tot_cost_intv       = last(cum_gov_cost_intv) + last(cum_ptn_cost_intv),
                tot_scalup_cost     = tot_cost_intv - tot_cost_bsln,
                tot_gov_share_intv  = last(cum_gov_cost_intv),
                tot_incr_gov_share  = last(cum_gov_cost_intv - cum_gov_cost_bsln),
                avg_gov_incr_share  = mean(gov_cost_intv - gov_cost_bsln),
                avg_tot_incr_cost   = mean((gov_cost_intv + ptn_cost_intv) - (gov_cost_bsln + ptn_cost_bsln))) %>%
      mutate(across(everything(), ~ scales::label_dollar()(.)))




# Cumulative
full_join(x  = oop_remain,
          y  = oop_remove,
          by = join_by(year, scen, ptn_cost, gov_cost,
                       tot_pov, cum_cost, cum_pov, frp_scen)) %>%

      ggplot(aes(x = year, linetype = frp_scen)) +
      geom_line(aes(y = cum_cost), color = "#89C348") +
      geom_line(aes(y = cum_pov * 1e3), color = "darkgreen") +

      # Cost labels
      geom_text(data    = oop_remain %>% filter(year == max(year)),
                mapping = aes(y     = cum_cost,
                              label = scales::label_dollar(scale = 1e-6, suffix = "M")(cum_cost)),
                color   = "#89C348",
                nudge_x = -2.8,
                nudge_y = -2.1e7,
                size    = 3) +
      geom_text(data    = oop_remove %>% filter(year == max(year)),
                mapping = aes(y     = cum_cost,
                              label = scales::label_dollar(scale = 1e-6, suffix = "M")(cum_cost)),
                color   = "#89C348",
                nudge_x = -3.5,
                nudge_y = 0,
                size    = 3) +

      # Count labels
      geom_text(data    = oop_remain %>% filter(year == max(year)),
                mapping = aes(y     = cum_pov * 1e3,
                              label = scales::label_comma(scale = 1e-3, suffix = "K")(cum_pov)),
                color   = "darkgreen",
                nudge_x = -2,
                nudge_y = 1e6,
                size    = 3) +
      geom_text(data    = oop_remove %>% filter(year == max(year)),
                mapping = aes(y     = cum_pov * 1e3,
                              label = scales::label_comma(scale = 1e-3, suffix = "K")(cum_pov)),
                color   = "darkgreen",
                nudge_x = -1.5,
                nudge_y = 7.5e6,
                size    = 3) +

      scale_y_continuous(name     = "Cumulative government cost\n(USD, millions)",
                         # labels   = function(x) {scales::comma(x / 1e6)},
                         labels   = function(x) scales::label_number(scale = 1e-6)(x),
                         # limits   = c(0, 8.1e7),
                         # breaks   = seq(0, 8e7, length.out = 5),
                         breaks   = seq(0, 1.5e8, length.out = 6),
                         limits   = c(0, 1.5e8),
                         sec.axis = sec_axis(~ . / 1e3, #265,
                                             name   = "Cumulative cases of poverty\n(thousands)",
                                             labels = function(x) {scales::comma(x / 1e3)},
                                             # labels = function(x) scales::label_comma(scale = 1e-3, suffix = "K")(x),
                                             breaks = seq(0, 150e3, length.out = 6))) +
      scale_linetype_manual(values = c("dashed", "solid")) +
      labs(x = "Year") +
      # facet_wrap(. ~ frp_scen) +
      theme_caviz +
      guides(linetype = guide_legend(title = "OOP target",
                                     reverse = TRUE)) +
                                     # override.aes = list(linetype = c("solid", "dashed")))) +
      theme(legend.position    = "bottom",
            legend.title       = element_text(color = "black"),
            axis.title.y.left  = element_text(color = "#89C348"),
            axis.text.y.left   = element_text(color = "#89C348"),
            # axis.line.y.right  = element_line(color = "blue"),
            axis.ticks.y.right = element_blank(),
            axis.title.y.right = element_text(color = "darkgreen"),
            axis.text.y.right  = element_text(color = "darkgreen",
                                              margin = margin(t = 0, r = 0, b = 0, l = -4)))



#--------------------------------------------------------------CA's feedback

full_join(x  = oop_remain,
          y  = oop_remove,
          by = join_by(year, scen, ptn_cost, gov_cost,
                       tot_pov, cum_tot_cost, cum_gov_cost, cum_pov, frp_scen)) %>%
      # view()
      #------------------------------------------------------------------------#
      select(!cum_tot_cost) %>%
      #------------------------------------------------------------------------#
      pivot_longer(cols      = starts_with("cum"),
                   names_to  = "metric",
                   values_to = "value") %>%
      mutate(value = ifelse(metric == "cum_pov", value * 1e3, value)) %>%
      mutate(metric = ifelse(metric == "cum_pov",
                             "Poverty cases\n(cumulative,\nmillions)",
                             "Government costs\n(cumulative,\nbillions, USD)")) %>%
      # mutate(metric = ifelse(metric == "cum_pov",
      #                        "Poverty cases\n(cumulative,\nthousands)",
      #                        "Government costs\n(cumulative,\nmillions, USD)")) %>%
      # view()

      ggplot(aes(x        = year,
                 y        = value,
                 linetype = frp_scen,
                 group    = frp_scen,
                 size     = frp_scen)) +
      geom_line(color = "#89C348") +

      scale_y_continuous(name   = NULL,
                         labels = function(x) scales::label_number(scale = 1e-9)(x)) + # change back to 9 instead of 6
      scale_size_manual(values = c(1.75, 1.75)) +
      scale_linetype_manual(values = c("dashed", "solid")) +
      labs(x = "Year") +
      facet_grid(metric ~ .,
                 scales = "free_y",
                 switch = "y") +
      theme_caviz +
      guides(linetype = guide_legend(title = "OOP\ntarget",
                                     reverse = TRUE)) +
      guides(color    = guide_legend(override.aes = list(size = c(1, 1)))) +
      guides(size = "none") +
      theme(# legend.position    = "inside",
            # legend.position.inside = c(0.2, 0.9),
            # legend.background  = element_rect(fill = "white", colour = NA),
            panel.grid.major.x = element_blank(),
            aspect.ratio = 0.5,
            strip.text.y       = element_text(size = 10),
            legend.title       = element_text(color = "black"),
            axis.ticks.y.right = element_blank())


#----------------For plot labels
full_join(x  = oop_remain,
          y  = oop_remove,
          by = join_by(year, scen, ptn_cost, gov_cost,
                       tot_pov, cum_tot_cost, cum_gov_cost, cum_pov, frp_scen)) %>%
      # view()
      #------------------------------------------------------------------------#
      select(!cum_tot_cost) %>%
      #------------------------------------------------------------------------#
      pivot_longer(cols      = starts_with("cum"),
                   names_to  = "metric",
                   values_to = "value") %>%
      # mutate(value = ifelse(metric == "cum_pov", value * 1e3, value)) %>%
      filter(year == sim_end_year) %>%
      view()


# ## Gov Costs and Induced Poverty Cases--Scale-up and Subsidize
#
# # FRP Calculations
# # Cumulative
# placeholder %>%
#       ggplot(aes(x = year)) +
#       geom_line(aes(y = cum_cost), color = "red") +
#       geom_line(aes(y = cum_pov * 1e3), color = "blue") +
#       scale_y_continuous(name     = "Cumulative government cost\n(USD, millions)",
#                          # labels   = function(x) {scales::comma(x / 1e6)},
#                          labels   = function(x) scales::label_number(scale = 1e-6)(x),
#                          breaks   = seq(0, 1.5e8, length.out = 5),
#                          limits   = c(0, 1.5e8),
#                          sec.axis = sec_axis(~ . / 1e3,
#                                              name   = "Cumulative cases of poverty\n(thousands)",
#                                              # labels = function(x) {scales::comma(x / 1e3)},
#                                              labels   = function(x) scales::label_comma(scale = 1e-3)(x),
#                                              breaks = seq(0, 150e3, length.out = 5))) +
#       labs(x = "Year") +
#       theme_caviz +
#       theme(#axis.line.y.left   = element_line(color = "red"),
#             # axis.ticks.y.left  = element_line(color = "red"),
#             axis.title.y.left  = element_text(color = "red"),
#             axis.text.y.left   = element_text(color = "red"),
#             # axis.line.y.right  = element_line(color = "blue"),
#             axis.ticks.y.right = element_blank(),
#             axis.title.y.right = element_text(color = "blue"),
#             axis.text.y.right  = element_text(color = "blue",
#                                               margin = margin(t = 0, r = -4, b = 0, l = -4)))
#


# Average incremental cost, OOP remain
cost_output %>%
      mutate(oop = 0.8) %>%
      filter(year > 2025,
             cause %in% target_cancers) %>%

      group_by(scen, year) %>%
      reframe(ptn_cost = sum(tpop_cost * oop),
              gov_cost = sum(tpop_cost * (1 - oop))) %>%
      mutate(cum_ptn_cost = cumsum(ptn_cost),
             cum_gov_cost = cumsum(gov_cost)) %>%
      pivot_wider(id_cols = year, names_from = scen,
                  values_from = c(ptn_cost, gov_cost, cum_ptn_cost, cum_gov_cost)) %>%
      # view()
      summarise(tot_cost_bsln       = last(cum_gov_cost_bsln) + last(cum_ptn_cost_bsln),
                tot_gov_share_bsln  = last(cum_gov_cost_bsln),
                tot_cost_intv       = last(cum_gov_cost_intv) + last(cum_ptn_cost_intv),
                tot_scalup_cost     = tot_cost_intv - tot_cost_bsln,
                tot_gov_share_intv  = last(cum_gov_cost_intv),
                tot_incr_gov_share  = last(cum_gov_cost_intv - cum_gov_cost_bsln),
                avg_gov_incr_share  = mean(gov_cost_intv - gov_cost_bsln),
                avg_tot_incr_cost   = mean((gov_cost_intv + ptn_cost_intv) - (gov_cost_bsln + ptn_cost_bsln))) %>%
      mutate(across(everything(), ~ scales::label_dollar()(.))) %>%
      t()
      # unlist()


1263506 / 41117856
1263506 / 41117856 / uganda_gdp[5, 5]

# Average incremental cost, OOP remove
cost_output %>%
      mutate(oop      = case_when(
            scen == "bsln" |
                  (scen == "intv" & cause %!in% target_cancers) ~ 0.8,
            scen == "intv" & year <= 2025 &
                  cause %in% target_cancers                     ~ 0.8,
            scen == "intv" & year >= 2050 &
                  cause %in% target_cancers                     ~ 0.0,
            TRUE                                                ~ NA_real_)) %>%
      mutate(oop = zoo::na.approx(oop),
             .by = c(cause, scen, age, sex)) %>%

      # view()
      filter(year > 2025,
             cause %in% target_cancers) %>%

      group_by(scen, year) %>%
      reframe(ptn_cost = sum(tpop_cost * oop),
              gov_cost = sum(tpop_cost * (1 - oop))) %>%
      mutate(cum_ptn_cost = cumsum(ptn_cost),
             cum_gov_cost = cumsum(gov_cost)) %>%
      pivot_wider(id_cols = year, names_from = scen,
                  values_from = c(ptn_cost, gov_cost, cum_ptn_cost, cum_gov_cost)) %>%
      # view()
      summarise(tot_cost_bsln       = last(cum_gov_cost_bsln) + last(cum_ptn_cost_bsln),
                tot_gov_share_bsln  = last(cum_gov_cost_bsln),
                tot_cost_intv       = last(cum_gov_cost_intv) + last(cum_ptn_cost_intv),
                tot_scalup_cost     = tot_cost_intv - tot_cost_bsln,
                tot_gov_share_intv  = last(cum_gov_cost_intv),
                tot_incr_gov_share  = last(cum_gov_cost_intv - cum_gov_cost_bsln),
                avg_gov_incr_share  = mean(gov_cost_intv - gov_cost_bsln),
                avg_tot_incr_cost   = mean((gov_cost_intv + ptn_cost_intv) - (gov_cost_bsln + ptn_cost_bsln))) %>%
      mutate(across(everything(), ~ scales::label_dollar()(.))) %>%
      unlist()

5367894 / 41117856 # (avg incr gov share of scale-up costs per capita)
5367894 / 41117856 / uganda_gdp[5, 5]



## Visualizing differences-----------------------------------------------------#

process_sim_outcomes <- function(oop_remain_inpt = sim_output_oop_remain,
                                 oop_remove_inpt = sim_output_oop_remove) {

      # Extract OOP values
      # oop_remain_val <- last(sim_output_oop_remain$all_groups$oop)
      # oop_remove_val <- last(sim_output_oop_remove$all_groups$oop)

      # Process deaths averted (bsln vs. intv scen)
      deaths_averted <- oop_remain_inpt$all_groups %>%
            filter(state == "dst") %>%
            group_by(year, scen) %>%
            reframe(cum_dth  = sum(Nx * pcdx)) %>%
            arrange(year, scen) %>%
            # names()

            pivot_wider(id_cols = year,
                        names_from = scen,
                        values_from = cum_dth) %>%
            group_by(year) %>%
            reframe(dth_avrt = bsln - intv)

      cum_delta_gov_cost <- full_join(
            x  = oop_remain_inpt$all_groups %>%
                  filter(state != "prc") %>%
                  group_by(year, scen) %>%
                  reframe(ptn_cost = sum(Nx * prop * cov * cost * oop),
                          gov_cost = sum(Nx * prop * cov * cost * (1 - oop))) %>%
                  arrange(year, scen) %>%
                  pivot_wider(id_cols = year,
                              names_from = scen,
                              values_from = gov_cost) %>%
                  group_by(year) %>%
                  reframe(gov_cost_remain = intv - bsln) %>%
                  mutate(cum_gov_cost_remain = cumsum(gov_cost_remain)),

            y  = oop_remove_inpt$all_groups %>%
                  filter(state != "prc") %>%
                  group_by(year, scen) %>%
                  reframe(ptn_cost = sum(Nx * prop * cov * cost * oop),
                          gov_cost = sum(Nx * prop * cov * cost * (1 - oop))) %>%
                  arrange(year, scen) %>%
                  pivot_wider(id_cols = year,
                              names_from = scen,
                              values_from = gov_cost) %>%
                  group_by(year) %>%
                  reframe(gov_cost_remove = intv - bsln) %>%
                  mutate(cum_gov_cost_remove = cumsum(gov_cost_remove)),

            by = join_by(year)
      ) %>%
            group_by(year) %>%
            reframe(add_gov_cost = cum_gov_cost_remove - cum_gov_cost_remain)

      cum_pov_avert <- full_join(

            x  = oop_remain_inpt$pov_cases %>%
                  group_by(year, scen) %>%
                  reframe(tot_pov  = sum(delta_pov)) %>%
                  pivot_wider(id_cols = year,
                              names_from = scen,
                              values_from = tot_pov) %>%
                  group_by(year) %>%
                  reframe(remain_pov_n = intv - bsln) %>%
                  mutate(cum_remain_pov_n = cumsum(remain_pov_n)),

            y  = oop_remove_inpt$pov_cases %>%
                  group_by(year, scen) %>%
                  reframe(tot_pov  = sum(delta_pov)) %>%
                  pivot_wider(id_cols = year,
                              names_from = scen,
                              values_from = tot_pov) %>%
                  group_by(year) %>%
                  reframe(remove_pov_n = intv - bsln) %>%
                  mutate(cum_remove_pov_n = cumsum(remove_pov_n)),

            by = join_by(year)
      ) %>%
            group_by(year) %>%
            reframe(cum_pov_avrt = cum_remain_pov_n - cum_remove_pov_n)

      all_outputs <- purrr::reduce(.x = list(deaths_averted,
                                             cum_delta_gov_cost,
                                             cum_pov_avert),
                                   .f = full_join)
}


process_sim_outcomes() %>%
      # view()
      pivot_longer(cols = ends_with("avrt"),
                   names_to = "metric",
                   values_to = "value") %>%
      mutate(metric = ifelse(metric == "dth_avrt", "Deaths", "Poverty"),
             metric = factor(metric)) %>%
      # view()

      ggplot(aes(x = year)) +
      geom_line(aes(y = value * 2.727273e3,
                    linetype = metric),
                color = "#89C348",
                size = 2,
                show.legend = TRUE) +
      geom_line(aes(y = add_gov_cost),
                color = "darkgreen",
                size = 2,
                show.legend = FALSE) +
      scale_y_continuous(name     = "Cumulative, additional\ngovernment cost\n(USD, millions)",
                         # labels   = function(x) {scales::comma(x / 1e6)},
                         labels   = function(x) scales::label_number(scale = 1e-6)(x),
                         breaks   = seq(0, 1.2e8, length.out = 5),
                         limits   = c(0, 1.2e8),
                         sec.axis = sec_axis(~ . / 2.727273e3, #265,
                                             name   = "Cumulative cases averted\n(thousands)",
                                             labels = function(x) {scales::label_number(scale = 1e-3)(x)},
                                             # labels = function(x) scales::label_comma(scale = 1e-3, suffix = "K")(x),
                                             breaks = seq(0, 4.4e4, length.out = 5))) +
      scale_linetype_manual(name = "Cases averted",
                            values = c("solid", "dashed")) +
      labs(x = "Year") +
      # scale_linetype_manual(
      #       values = c("Deaths" = "solid", "Poverty" = "dashed"),  # Custom linetypes
      #       guide = guide_legend(override.aes = list(size = 0.6, color = "#89C348"))
      # ) +
      theme_caviz +
      guides(linetype = guide_legend(override.aes = list(size = 0.8),
                                     nrow = 2))  +
      # theme_bw() +
      theme(legend.position    = "bottom",
            legend.title       = element_text(color = "black"),
            legend.key.width   = unit(2, "cm"),
            # legend.box         = "vertical",
            # legend.margin      = margin(),
            # legend.direction   = "vertical",
            axis.title.y.left  = element_text(color = "darkgreen"),
            axis.text.y.left   = element_text(color = "darkgreen"),
            # axis.line.y.right  = element_line(color = "blue"),
            axis.ticks.y.right = element_blank(),
            axis.title.y.right = element_text(color = "#89C348"),
            axis.text.y.right  = element_text(color = "#89C348",
                                              margin = margin(t = 0, r = 0, b = 0, l = -4)))

c("#EE7600")


process_sim_outcomes() %>%
      # view()
      # pivot_longer(cols = add_gov_cost:cum_pov_avrt,
      #              names_to = "metric",
      #              values_to = "value") %>%
      # mutate(metric = ifelse(metric == "add_gov_cost", "Cost", "Poverty"),
      #        metric = factor(metric)) %>%
      # view()

      # filter(metric == "Cost") %>%
      # view()

      ggplot(aes(x = year, y = add_gov_cost)) +
      geom_line(color = "#89C348",
                size = 2,
                show.legend = FALSE) +
      scale_y_continuous(name     = "Cumulative, additional\ngovernment cost\n(USD, millions)",
                         # labels   = function(x) {scales::comma(x / 1e6)},
                         labels   = function(x) scales::label_number(scale = 1e-6)(x),
                         breaks   = seq(0, 1.2e8, length.out = 5),
                         limits   = c(0, 1.2e8)) +
      scale_linetype_manual(name = "Cases averted",
                            values = c("solid", "dashed")) +
      labs(x = "Year") +
      theme_caviz +
      guides(linetype = guide_legend(override.aes = list(size = 0.8),
                                     nrow = 2))  +
      # theme_bw() +
      theme(legend.position    = "bottom",
            legend.title       = element_text(color = "black"),
            legend.key.width   = unit(2, "cm"),
            # legend.box         = "vertical",
            # legend.margin      = margin(),
            # legend.direction   = "vertical",
            axis.title.y.left  = element_text(color = "darkgreen"),
            axis.text.y.left   = element_text(color = "darkgreen"),
            # axis.line.y.right  = element_line(color = "blue"),
            axis.ticks.y.right = element_blank(),
            axis.title.y.right = element_text(color = "#89C348"),
            axis.text.y.right  = element_text(color = "#89C348",
                                              margin = margin(t = 0, r = 0, b = 0, l = -4)))
