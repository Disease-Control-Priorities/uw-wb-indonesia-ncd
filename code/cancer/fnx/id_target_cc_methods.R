#' @title Extract Contraception Methods
#' @author Mohamed Albirair, MBBS, MPH, PhD
#' @param dset Country-specific data set
#' @param ptrn Regular expression pattern for detecting CC method as described in question label
#' @return A named vector of unique CC methods, and a list of duplicate columns


extract_cc_methods <- function(dset,
                               ptrn) {

      # Extract method labels from dictionary
      cc_mthd_raw <- dset %>%
            select(matches(ptrn)) %>%
            labelled::look_for(., details = "none") %>%
            # view()
            select(label) %>%
            pull()

      # Convert method labels to standardized labels
      cc_mthd_prim <- map(.x = cc_mthd_raw,
                          .f = relab_cc_method) %>% unlist()

      # Extract unique values
      cc_mthd <- cc_mthd_prim %>% unname() %>% unique()

      # Rename vector
      names(cc_mthd) <- str_extract(
            string  = str_remove(string  = names(cc_mthd_prim[match(cc_mthd,
                                                                    cc_mthd_prim)]),
                                 pattern =  ptrn),
            pattern = "^[a-z]?"
            )

      # ID duplicate columns to remove after coalease
      rm_cols <- names(cc_mthd_prim[-match(cc_mthd, cc_mthd_prim)])

      list(cc_mthd_raw  = cc_mthd_raw,
           cc_mthd_prim = cc_mthd_prim,
           cc_mthd      = cc_mthd,
           rm_cols      = rm_cols)

}
