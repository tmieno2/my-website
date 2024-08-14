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

#++++++++++++++++++++++++++++++++++++
#+ Github repository
#++++++++++++++++++++++++++++++++++++
#--- list of GitHub repository ---#
# You need to build this yourself

repository_list <-
  tribble(
    ~keyword, ~repo_link,
    "Bias in economic", "https://github.com/tmieno2/GWR_value",
    "Aquifer depletion", "https://github.com/tmieno2/Drought-Production-Risk-Aquifer",
    "different trial designs", "https://github.com/tmieno2/Econ_Trial_Design_PA",
    "Causal forest approach", "https://github.com/tmieno2/CF_for_VRA"
  ) %>%
  data.table()

#---------------------
#- attach github repo link
#---------------------
attach_repo_link <- function(pub_data, repository_list) {
  keyword_ls <- repository_list$keyword
  repolink_ls <- repository_list$repo_link
  pub_data_copy <- copy(pub_data)

  for (i in 1:length(keyword_ls)) {
    pub_data_copy[grepl(keyword_ls[i], title), repo_link := repolink_ls[i]]
  }

  num_link <- pub_data_copy[, sum(!is.na(repo_link))]

  if (num_link < nrow(repository_list)){
    print(paste0("After the match, you have only ", num_link, "repository links in the publication data after the match, while you provided ", nrow(repository_list), "links. Take a look at the keyword variable for typos and other forms of errors."))
  }
  
  return(pub_data_copy)
}

#++++++++++++++++++++++++++++++++++++
#+ Select journals
#++++++++++++++++++++++++++++++++++++
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

#++++++++++++++++++++++++++++++++++++
#+
#++++++++++++++++++++++++++++++++++++
pr_pub_data <-
  pub_data[journal %in% pr_journal_ls, ] %>%
  attach_repo_link(., repository_list) %>%
  .[, title_with_link := ifelse(
    !is.na(repo_link),
    paste0(title, " ([Paper](", url, ")", ", [GitHub Repository](", repo_link, "))"),
    paste0(title, " ([Paper](", url, "))")
  )] %>%
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

writeLines(pub_list_in_yaml, "Website/publications/publications.yml")
