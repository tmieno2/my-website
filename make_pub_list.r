library(bib2df)
library(yaml)
library(dplyr)
library(data.table)

#--- covert a bib file into a data.frame ---#
df <-
  bib2df::bib2df("/Users/tmieno2/Dropbox/PromotionTenure/Website/publications.bib") %>%
  dplyr::rename_with(stringr::str_to_lower) %>%
  dplyr::select(title, journal, year) %>%
  # this avoids having ".0" at the end of year
  dplyr::mutate(year = as.character(year)) %>%
  # this avoids error due to having ":" in the paper title
  dplyr::mutate(title = paste0('"', title, '"'))

#--- create a list of publications as yml ---#
pub_list_in_yaml <-
  lapply(
    1:nrow(df),
    function(x) {
      yaml::as.yaml(df[x, ])
    }
  ) %>%
  unlist() %>%
  #--- add - as an indication of the start of a paper ---#
  gsub("title", "- paper-title", .) %>%
  #--- add two spaces in front ---#
  gsub("year", "  year", .) %>%
  #--- add two spaces in front ---#
  gsub("journal", "  journal", .) %>%
  gsub("\'", "", .)

#--- write as a yml file ---#
writeLines(pub_list_in_yaml, "Website/publications.yml")


# !===========================================================
# !
# !===========================================================
library("scholar")
id <- "PxS7aU0AAAAJ"

pub_data <-
  get_publications(id) %>%
  data.table()

#--- list of peer-reviewed journals ---#
pr_journal_ls <-
  pub_data$journal %>%
  unique() %>%
  .[!stringr::str_detect(., "CSSA|Working|Chapman|HORIZON|Japanese|OSF|tmieno|SSRN|Illinois|Cornhusker|Abstract|SocArXiv|Proceeding|Unjournal")] %>%
  .[. != ""]

pub_urls <-
  lapply(
    1:nrow(pub_data),
    function(x) get_publication_url(id, pub_data$pubid[x])
  )

pub_urls[sapply(pub_urls, function(x) length(x) == 0L)] <- NA

pub_data$url <- unlist(pub_urls)

pr_pub_data <-
  pub_data[journal %in% pr_journal_ls, ] %>%
  .[, title_with_link := paste0("[", title, "](", url, ")")] %>%
  .[, .(title_with_link, year, journal)] %>%
  .[, year := as.character(year)] %>%
  .[, title_with_link := paste0('"', title_with_link, '"')]

pub_list_in_yaml <-
  lapply(
    1:nrow(pr_pub_data),
    function(x) {
      yaml::as.yaml(pr_pub_data[x, ])
    }
  ) %>%
  unlist() %>%
  #--- add - as an indication of the start of a paper ---#
  gsub("title_with_link", "- title_with_link", .) %>%
  #--- add two spaces in front ---#
  gsub("year", "  year", .) %>%
  #--- add two spaces in front ---#
  gsub("journal", "  journal", .) %>%
  gsub("url", "  url", .) %>%
  gsub("\'", "", .)

writeLines(pub_list_in_yaml, "Website/publications.yml")
