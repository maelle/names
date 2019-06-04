base_dir <- "/home/maelle/Documents/ropensci/roweb2/content/authors/"

authors <- fs::dir_ls(base_dir,
                      recurse = TRUE)
authors <- authors[grepl("\\_index", authors)]

read_author <- function(path){
  df <- tibble::as_tibble(
    yaml::read_yaml(path))
  df$slug <- gsub(base_dir, "", path)
  df$slug <- gsub("\\/_index\\.md", "", df$slug)
  df
}

authors <- purrr::map_df(authors, read_author)

####

registry <- jsonlite::read_json("https://raw.githubusercontent.com/ropensci/roregistry/gh-pages/raw_cm.json")

tibblify <- function(x){
  df <- tibble::as_tibble(x)
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  if (!"email" %in% names(df)){
    df$email <- NA
  }
  
  df$givenName <- glue::glue_collapse(df$givenName, sep = " ")
  df$familyName <- glue::glue_collapse(df$familyName, sep = " ")
  
  df
}

rectangle_folks <- function(entry){
  print(entry$identifier)
  #if(entry$identifier == "bikedata") browser()
  name <- entry$name
  maintainer <- tibblify(entry$maintainer[[1]])
  maintainer$role <- "maintainer"
  
  authors <- purrr::map_df(entry$author, tibblify)
  if(nrow(authors) > 0){
    authors$role <- "author"
    if (nrow(authors[authors$email != maintainer$email |
                     is.na(authors$email),]) > 0){
      authors <- authors[authors$email != maintainer$email |
                           is.na(authors$email),]
    } else {
      authors <- NULL
    }
  }
  
  if(any(purrr::map_chr(entry$contributor, "@type") == "Person")){
    contributors <- purrr::map_df(entry$contributor, tibblify)
    if (nrow(contributors > 0)){
      contributors$role <- "contributor"
      contributors <- contributors[!contributors$email %in% c(
        maintainer$email, authors$email[!is.na(authors$email)]) |
          is.na(authors$email),]
    }
  } else{
    contributors <- NULL
  }
  
  
  dplyr::bind_rows(list(maintainer, authors, contributors))
}

package_folks <- purrr::map_df(registry, rectangle_folks)
