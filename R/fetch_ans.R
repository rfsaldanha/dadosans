#' Fetch data from the Brazilian National Agency of Supplementary Health
#'
#' @param states character. One UF acronym or a vector.
#' @param years numeric. One year or a vector.
#'
#' @return A tibble.
#' @export
fetch_ans <- function(states, years){
  # Check assertions
  checkmate::assert_choice(states, c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO"))
  checkmate::assert_numeric(as.integer(years), lower = 2015)

  # Temp dir
  temp_dir <- normalizePath(paste0(tempdir(),"/fetch_ans"))
  if(dir.exists(temp_dir)){
    unlink(
      paste0(temp_dir, "/", dir(temp_dir)),
      recursive = TRUE
    )
  }
  dir.create(temp_dir)

  # For each state and year...
  for(state in states){
    for(year in years){
      # Download URL
      base_url <- glue::glue("{hospitalar_url}/{year}/{state}/")

      # Page links
      web_page <- rvest::read_html(base_url)
      links <- web_page %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")

      # Filter files
      desired_links <- grep("HOSP_CONS.zip$", links, value = TRUE)

      # Baixando e extraindo os arquivos ZIP

      for(link in desired_links){
        usethis::ui_info("Downloading {link}")

        file_name <- basename(link)
        dest_file <- glue::glue("{temp_dir}/{file_name}")

        # Download file
        curl::curl_download(url = paste0(base_url, link), destfile = dest_file)

        # Extract file
        utils::unzip(dest_file, exdir = temp_dir)
      }
    }
  }

  # List extracted files
  extracted_files <- list.files(
    path = temp_dir,
    pattern = "*.csv",
    full.names = TRUE
  )

  # Read and stack files
  usethis::ui_info("Reading files...")
  res <- purrr::map(
    .x = extracted_files,
    .f = readr::read_delim,
    .progress = TRUE,
    delim = ";",
    col_types = hosp_cons_schema
  )

  # Clean temp files
  unlink(
    paste0(temp_dir, "/", dir(temp_dir)),
    recursive = TRUE
  )

  # List to data frame
  res <- purrr::list_rbind(res)

  # Return
  usethis::ui_done("Done!")
  return(res)
}
