#' importing the raw data from the XRF machine (.txt file)

importdata <- function(datapath){

  datafile.df <- readr::read_delim(datapath, delim = "\t", locale = readr::locale(decimal_mark = ","))
  datafile.df <- datafile.df %>%
    dplyr::select(-dplyr::ends_with("(PPM)")) %>%
    dplyr::select(-dplyr::ends_with("(%)")) %>%
    dplyr::select(-c("S", "P")) %>%
    dplyr::select(dplyr::starts_with("X")) %>%
    dplyr::rename_all(stringr::str_remove, pattern = " .*")

  return(datafile.df)

}
