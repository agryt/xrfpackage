# import and modify datapath
#' @importFrom readr read_delim locale
#' @importFrom dplyr select ends_with starts_with rename_all
#' @importFrom stringr str_remove


importdata <- function(datapath){

  datafile.df <- read_delim(datapath, delim = "\t", locale = locale(decimal_mark = ","))
  datafile.df <- datafile.df %>%
    select(-ends_with("(PPM)")) %>%
    select(-ends_with("(%)")) %>%
    select(-c("S", "P")) %>%
    select(starts_with("X")) %>%
    rename_all(str_remove, pattern = " .*")

  return(datafile.df)

}
