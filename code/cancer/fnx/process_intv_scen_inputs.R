#' @title Process User-Defined Stage Distribution Data
#' @author Mohamed Albirair, MBBS, MPH, PhDc; David Watkins
#' @param user_def_stg_dstrb User-defined stage distribution data
#'
#' @return An updated stage distribution data frame

process_intv_scen_inputs <- function(path_input     = z$scen_input_file,
                                     screen_scaleup = c(TRUE, FALSE)) {

      # Read "all_inputs" sheet------------------------------------------------#
      all_inputs_sheet <- readxl::read_xlsx(path  = path_input,
                                            sheet = "all_inputs",
                                            range = "A1:AA577") %>%

            mutate(intv_tag = case_match(intervention,
                                         "Alcohol mass media campaigns" ~ "alc_media",
                                         "Alcohol regulations - advertising, sales" ~ "alc_reg",
                                         "Palliative care" ~ "pall_care",
                                         "Standard of care treatment" ~ "soc",
                                         "Tobacco regulations - advertising, front-of-pack labeling" ~ "tob_reg",
                                         "Tobacco mass media campaigns" ~ "tob_media",
                                         .default = NA)) %>%

            filter(intv_tag == "soc",
                   setting  == country_setting)

      # Set default starting values for cascade and stage distribution
      bsln_cascade_stg_dist <- scen_input_param$bsln_cascade_stg_dist

      # Define whether screening scaleup impacts stage distribution------------#
      if (screen_scaleup) {

            # Define target stage distribution: read assumed data
            # (No data points for screenable cancer types)
            # trgt_cascade_stg_dist <- setting_cascade_stg_dist$end
            trgt_cascade_stg_dist <- scen_input_param$trgt_cascade_stg_dist

      } else {

            trgt_cascade_stg_dist <- scen_input_param$trgt_cascade_stg_dist
      }

      # Read cascade and stage distribution data-------------------------------#
      cascade_stg_years <- full_join(
            x  = bsln_cascade_stg_dist,
            y  = trgt_cascade_stg_dist,
            by = join_by(cause, setting, scaleup,
                         screened, diagnosed, treated, completed,
                         precancer, local, regional, distant)) %>%

            # Expand to all projection years
            # Set boundaries: begin and end years
            mutate(year = case_when(scaleup == "begin" ~ sim_start_year,
                                    scaleup == "end"   ~ sim_end_year,
                                    TRUE               ~ NA)) %>%
            complete(year = sim_start_year:sim_end_year, nesting(cause, setting)) %>%
            relocate(year, .before = cause) %>%
            arrange(year, cause)

      # Check: 816 / 16 / length(2000:2050)


      # Combine effectiveness, cascade, and stage distribution data------------#
      effect_cascade_stg_prim_1 <- full_join(
            x  = all_inputs_sheet %>%
                  select(cause, category, target_tp, tag, intv_tag, effect),
            y  = cascade_stg_years,
            by = join_by(cause),
            relationship = "many-to-many" # but why? check: nrow(effect_cascade_stg_prim1) / 16 / 6 / length(2000:2050) == 1
            ) %>%
            arrange(year, cause,
                    match(tag, c("tp0", "tp1", "tp2", "tp3", "cfr")))


      # Input stage-distribution values for before and after scale-up years
      effect_cascade_stg_prim_2 <- effect_cascade_stg_prim_1 %>%
            split(.$year) %>%
            map_df(.x = .,
                   .f = function(dat) {
                        if (unique(dat$year) <= scaleup_start_year) {
                              right_join(x  = dat %>% select(!(setting:distant)),
                                        y  = bsln_cascade_stg_dist,
                                        by = join_by(cause))

                        } else if (unique(dat$year) >= scaleup_end_year) {
                              right_join(x  = dat %>% select(!(setting:distant)),
                                        y  = trgt_cascade_stg_dist,
                                        by = join_by(cause))

                        } else {
                              dat
                        }
                   }) %>%

            arrange(year, cause,
                    match(tag, c("tp0", "tp1", "tp2", "tp3", "cfr")))


      # Input stage-distribution values for scale-up years for non-screenable cancer types
      # and interpolate cascade data for all cancer types
      effect_cascade_stg_prim_3 <- effect_cascade_stg_prim_2 %>%
            split(list(.$year, .$cause)) %>%
            map_df(.x = .,
                   .f = function(dat) {
                         if (unique(dat$cause) %!in% intersect(screenable_cancers, target_cancers) &
                             unique(dat$year) > scaleup_start_year &
                             unique(dat$year) < scaleup_end_year) {

                               left_join(x  = dat %>% select(!(precancer:distant)),
                                         y  = bsln_cascade_stg_dist %>%
                                               select(cause, precancer:distant),
                                         by = join_by(cause))
                        } else {
                              dat
                        }
                   }) %>%

            # Label % screened as 0 in non-screenable cancer types
            mutate(screened = case_when(cause %!in% screenable_cancers ~ 0.0,
                                        TRUE                           ~ screened)) %>%

            # Arrange data for easier processing
            arrange(year, cause,
                    match(tag, c("tp0", "tp1", "tp2", "tp3", "cfr"))) %>%

            # Interpolate stage distribution data
            #-------------------Assumption: linear cascade performance scale-up#
            group_by(cause, target_tp, tag) %>%
            mutate(across(screened:completed, ~ zoo::na.approx(.))) %>%
            ungroup()

      if (!screen_scaleup) {
            effect_cascade_stg_prim_3 <- effect_cascade_stg_prim_3 %>%
                  group_by(cause, target_tp, tag) %>%
                  mutate(across(precancer:distant, ~ zoo::na.approx(.))) %>%
                  ungroup()
      }


      # By now, the data set has stage distribution data for all cancer types,
      # over the years, except for screenable cancer types...
      # It also has cascade performance data for cancer types.

      # Extract starting (baseline) values for calculations
      effect_cascade_stg_prim_4 <- effect_cascade_stg_prim_3 %>%
            mutate(bsln_scr = unique(screened[scaleup == "begin"  & year == scaleup_start_year]),
                   bsln_adh = unique(completed[                     year == scaleup_start_year]),
                   bsln_s0  = unique(precancer[scaleup == "begin" & year == scaleup_start_year]),
                   bsln_s1  = unique(local[scaleup == "begin"     & year == scaleup_start_year]),
                   bsln_s2  = unique(regional[scaleup == "begin"  & year == scaleup_start_year]),
                   bsln_s3  = unique(distant[scaleup == "begin"   & year == scaleup_start_year]),
                   .by = cause)

      if (screen_scaleup) {
            # Stage re-distribution:-------------------------------------------#
            # Calculate stage re-distribution for screenable cancer types------#
            effect_cascade_stg_cmplt <- effect_cascade_stg_prim_4 %>%
                  mutate(tp_id = case_when(tag == "tp0"                                     ~ "a",
                                           tag == "tp1" & target_tp == "well-to-local"      ~ "b",
                                           tag == "tp1" & target_tp == "precancer-to-local" ~ "c",
                                           tag == "tp2"                                     ~ "d",
                                           tag == "tp3"                                     ~ "e",
                                           tag == "cfr"                                     ~ "f")) %>%
                  split(list(.$year, .$cause, .$tp_id)) %>%
                  map_df(.x = .,
                         .f = function(x) {

                               # Calculate stage distribution in breast cancer
                               brs_stg_dist <- calc_scrn_stg_shft(tot_pop       = select_country_pop,
                                                                  gbd_prev      = scr_cncr_prev[names(scr_cncr_prev) == screenable_cancers[1]],
                                                                  bsln_scrn_cov = x$bsln_scr,
                                                                  sclp_scrn_cov = x$screened,
                                                                  bsln_stg_dst  = c(x$bsln_s0, x$bsln_s1,
                                                                                    x$bsln_s2, x$bsln_s3),
                                                                  sclp_stg_dst  = stg_dist_w_enh_scrn[, "brs"])

                               # Calculate stage distribution in category 3 cancer types
                               cvx_stg_dist <- calc_scrn_stg_shft(tot_pop       = select_country_pop,
                                                                  gbd_prev      = scr_cncr_prev[names(scr_cncr_prev) == screenable_cancers[2]],
                                                                  bsln_scrn_cov = x$bsln_scr,
                                                                  sclp_scrn_cov = x$screened,
                                                                  bsln_stg_dst  = c(x$bsln_s0, x$bsln_s1,
                                                                                    x$bsln_s2, x$bsln_s3),
                                                                  sclp_stg_dst  = stg_dist_w_enh_scrn[, "cvx"])

                               # Calculate stage distribution in category 3 cancer types
                               crc_stg_dist <- calc_scrn_stg_shft(tot_pop       = select_country_pop,
                                                                  gbd_prev      = scr_cncr_prev[names(scr_cncr_prev) == screenable_cancers[3]],
                                                                  bsln_scrn_cov = x$bsln_scr,
                                                                  sclp_scrn_cov = x$screened,
                                                                  bsln_stg_dst  = c(x$bsln_s0, x$bsln_s1,
                                                                                    x$bsln_s2, x$bsln_s3),
                                                                  sclp_stg_dst  = stg_dist_w_enh_scrn[, "crc"])

                               if (unique(x$cause) %in% screenable_cancers &
                                   unique(x$year) >  scaleup_start_year &
                                   unique(x$year) <= scaleup_end_year) {

                                     x %>% mutate(
                                           precancer =
                                                 case_when(# is.na(precancer) & cause %in% cause2[2:3]     ~ 0,
                                                       is.na(precancer) & cause %in% target_cancers &
                                                             cause == cause2[1]                          ~ brs_stg_dist[1],
                                                       is.na(precancer) & cause %in% target_cancers &
                                                             cause == "Cervical cancer"                  ~ cvx_stg_dist[1],
                                                       is.na(precancer) & cause %in% target_cancers &
                                                             cause == "Colon and rectum cancer"          ~ crc_stg_dist[1],
                                                       TRUE                                              ~ precancer),

                                           local =
                                                 case_when(is.na(local) & cause %in% target_cancers &
                                                                 cause == cause2[1]                      ~ brs_stg_dist[2],
                                                           is.na(local) & cause %in% target_cancers &
                                                                 cause == "Cervical cancer"              ~ cvx_stg_dist[2],
                                                           is.na(local) & cause %in% target_cancers &
                                                                 cause == "Colon and rectum cancer"      ~ crc_stg_dist[2],
                                                           TRUE                                          ~ local),
                                           regional =
                                                 case_when(is.na(regional) & cause %in% target_cancers &
                                                                 cause == cause2[1]                      ~ brs_stg_dist[3],
                                                           is.na(regional) & cause %in% target_cancers &
                                                                 cause == "Cervical cancer"              ~ cvx_stg_dist[3],
                                                           is.na(regional) & cause %in% target_cancers &
                                                                 cause == "Colon and rectum cancer"      ~ crc_stg_dist[3],
                                                           TRUE                                          ~ regional),
                                           distant =
                                                 case_when(is.na(distant) & cause %in% target_cancers &
                                                                 cause == cause2[1]                      ~ brs_stg_dist[4],
                                                           is.na(distant) & cause %in% target_cancers &
                                                                 cause == "Cervical cancer"              ~ cvx_stg_dist[4],
                                                           is.na(distant) & cause %in% target_cancers &
                                                                 cause == "Colon and rectum cancer"      ~ crc_stg_dist[4],
                                                           TRUE                                          ~ distant)
                                     )
                               } else {
                                     x
                               }
                         }) %>%

                  arrange(year, cause,
                          match(tag, c("tp0", "tp1", "tp2", "tp3", "cfr"))) %>%
                  # view()
                  rename(init_ttt = treated,
                         adh_ttt  = completed)
      }

      if (screen_scaleup) {
            complete_data <- effect_cascade_stg_cmplt
      } else {
            complete_data <- effect_cascade_stg_prim_4 %>%
                  rename(init_ttt = treated,
                         adh_ttt  = completed)
      }

      # Extract effect on each TP per cancer type------------------------------#
      intv_effects <- complete_data %>%

            # Disregard TPs that do not match the natural history model
            filter(!(category == 3 & target_tp == "well-to-local")) %>%
            filter(!(category != 3 & target_tp == "precancer-to-local")) %>%
            filter(!(category == 1 & target_tp %!in% c("well-to-local", "distant-to-dead"))) %>%

            # Check: 2397 / length(2000:2050) / (5 * 5 + 11 * 2) == 1----------#

            # Apply formula only to target TPs
            mutate(targetted = ifelse(effect < 1 & cause %in% target_cancers, TRUE, FALSE),
                   tp_modif  = ifelse(targetted, 1 - ((1 - effect) * (adh_ttt - bsln_adh) / (1 - (1 - effect) * bsln_adh)), 1)) %>%

            pivot_wider(id_cols      = c(cause, year,
                                         screened, bsln_scr, diagnosed,
                                         init_ttt, adh_ttt, bsln_adh,
                                         precancer, local, regional, distant),
                        names_from   = tag,
                        names_prefix = "effect_on_",
                        values_from  = tp_modif,
                        values_fill  = 1) %>%
            relocate(effect_on_tp0, .before = effect_on_tp1) %>%
            relocate(effect_on_tp2, .after = effect_on_tp1) %>%
            relocate(effect_on_tp3, .after = effect_on_tp2) %>%
            relocate(effect_on_cfr, .after = effect_on_tp3)

      return(intv_effects)
}
