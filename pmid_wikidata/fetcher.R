#' Making CRC 1002 publications available via Wikidata
library(tidyverse)
#' 1. Load pubs
#'
#' From:
#' <https://sfb1002.med.uni-goettingen.de/production/literature>
tt <- readxl::read_xlsx("~/Downloads/2018-02-14_excel_export.xlsx")
tt
#' 2. Check if publications are already available via Wikidata
#'
#' Helper function to check Wikidata for PubMed publications
#' @param pmid PMID
wikidata_fetch <- function(pmid) {
  resp <- httr::GET(paste0("https://tools.wmflabs.org/hub/P698:", pmid))
  if (httr::status_code(resp) != 200) {
    warning(
      sprintf(
        "No Wikidata entry available for [%s]\n%s",
        httr::status_code(resp),
        httr::content(resp)$message
      ),
      call. = FALSE
    )
    NA
  } else {
    resp$url
  }
}
#' Call:
my_list <- purrr::map(tt$PMID, wikidata_fetch)
#' Filter out missing
wikidata_df <- dplyr::data_frame(
  pmid = tt$PMID,
  doi = tt$DOI,
  wikidata_id = unlist(my_list)
)
wikidata_df
# which pmids are missing
wikidata_miss <- wikidata_df %>%
  filter(is.na(wikidata_id),!is.na(pmid))
#' 3. Adding PubMed records to Wikidata using Fatameh
#'
#' <https://www.wikidata.org/wiki/Wikidata:WikiProject_Source_MetaData/fatameh>
#'
my_token <- my_token
# Function to add PMIDs
pmid_add <- function(pmid) {
  u <- paste0("tools.wmflabs.org/fatameh/token/pmid/add/", pmid)
  resp <- httr::GET(u, httr::add_headers(Authorization = my_token))
  if (httr::status_code(resp) != 200) {
    warning("Upps, something went wrong")
    out <- NULL
  } else {
    wikidata_id <- httr::content(resp) %>%
      .$item
    out <- dplyr::data_frame(pmid, wikidata_id)
    message(paste(
      "Publication with PMID",
      pmid,
      "is now represented in Wikidata",
      wikidata_id
    ))
    out
  }
}
#' apply the function to the missing pmids
tmp <- purrr::map_df(wikidata_miss$pmid, pmid_add)
#' Missing dois
