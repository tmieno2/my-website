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
  scholar::get_publications(id) %>%
  data.table() %>%
  .[, category := "article"]

bib2df::df2bib(pub_data, "CV/publications.bib")

mybib <- RefManageR::ReadBib("CV/publications.bib")

RefManageR::PrintBibliography(mybib, .opts = list(bib.style = "apa", sorting = ""))



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

  if (num_link < nrow(repository_list)) {
    print(paste0("After the match, you have only ", num_link, "repository links in the publication data after the match, while you provided ", nrow(repository_list), "links. Take a look at the keyword variable for typos and other forms of errors."))
  }

  return(pub_data_copy)
}

keywords <- c("Input use under", "Aquifer depletion")

data <- pr_pub_data

remove_papers <- function(data, keywords) {
  if (!is.null(keywords)) {
    drop_or_not <-
      lapply(
        1:length(keywords),
        \(x) {
          grepl(keywords[x], data[, title])
        }
      ) %>%
      do.call("+", .)

    return_data <-
      data.frame(data)[!drop_or_not, ] %>%
      data.table()
  } else {
    return_data <- data
  }

  return(return_data)
}


#++++++++++++++++++++++++++++++++++++
#+ Select journals
#++++++++++++++++++++++++++++++++++++
#--- list of peer-reviewed journals ---#
pr_journal_ls <-
  pub_data$journal %>%
  unique() %>%
  .[!stringr::str_detect(., "CSSA|Working|Chapman|HORIZON|Japanese|OSF|tmieno|SSRN|Illinois|Cornhusker|Abstract|SocArXiv|Proceeding|Unjournal|agriculture'")] %>%
  .[. != ""]

#++++++++++++++++++++++++++++++++++++
#+ Get urls of the paper
#++++++++++++++++++++++++++++++++++++
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
make_pub_yml <- function(pub_data, journal_list, exclude = NULL) {
  pr_pub_data <-
    pub_data[journal %in% journal_list, ] %>%
    attach_repo_link(., repository_list) %>%
    remove_papers(., keywords = exclude) %>%
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

  return(pub_list_in_yaml)
}

pub_list_in_yaml <- make_pub_yml(pub_data, pr_journal_ls)

writeLines(pub_list_in_yaml, "Website/publications/publications.yml")


#++++++++++++++++++++++++++++++++++++
#+ Precision-Ag papers
#++++++++++++++++++++++++++++++++++++
PA_journal_ls <-
  c(
    "Frontiers in Agronomy",
    "Computers and Electronics in Agriculture",
    "Agricultural Systems",
    "Precision Agriculture"
  )

PA_pub_list_in_yaml <- make_pub_yml(pub_data, PA_journal_ls)

writeLines(PA_pub_list_in_yaml, "Website/projects/precision-ag/publications.yml")

#++++++++++++++++++++++++++++++++++++
#+ Water management
#++++++++++++++++++++++++++++++++++++
WM_journal_ls <-
  c(
    "Water Resources Research",
    "American Journal of Agricultural Economics",
    "Resource and Energy Economics",
    "Advances in Water Resources",
    "Land Economics",
    "Nature Water",
    "Agricultural Economics",
    "Environmental and Resource Economics"
  )

WM_pub_list_in_yaml <-
  make_pub_yml(
    pub_data = pub_data,
    journal_list = WM_journal_ls,
    exclude = c("Input use under")
  )

writeLines(WM_pub_list_in_yaml, "Website/projects/water-economics/publications.yml")


# !===========================================================
# !
# !===========================================================
format(bref)

bref <- c(
  bibentry(
    bibtype = "Manual",
    title = "boot: Bootstrap R (S-PLUS) Functions",
    author = c(
      person("Angelo", "Canty",
        role = "aut",
        comment = "S original"
      ),
      person(c("Brian", "D."), "Ripley",
        role = c("aut", "trl", "cre"),
        comment = "R port, author of parallel support",
        email = "ripley@stats.ox.ac.uk"
      )
    ),
    year = "2012",
    note = "R package version 1.3-4",
    url = "https://CRAN.R-project.org/package=boot",
    key = "boot-package"
  ),
  bibentry(
    bibtype = "Book",
    title = "Bootstrap Methods and Their Applications",
    author = as.person("Anthony C. Davison [aut], David V. Hinkley [aut]"),
    year = "1997",
    publisher = "Cambridge University Press",
    address = "Cambridge",
    isbn = "0-521-57391-2",
    url = "http://statwww.epfl.ch/davison/BMA/",
    key = "boot-book"
  )
)


biblio <- bibtex::read.bib("CV/publications.bib")

format(biblio, style = "text")


mybib <-
  RefManageR::ReadBib("CV/publications.bib")

RefManageR::BibOptions(sorting = "ydnt")

temp <-
  data.table(
    title = c("fer", "efrfe"),
    year = c(2003, 2005)
  )

library(jsonlite)

read_json("CV/test.json")
fromJSON("CV/test.json")

exportJSON <- toJSON(temp, pretty = TRUE)


write_json(exportJSON, "CV/test.json")

