#code

# readr: read_delim
# readxl: read_excel
# assertr: verify




importxrf <- function(datapath, infopath){

  # import and modify datapath
  #' @importFrom readr read_delim locale
  #' @importFrom dplyr select ends_with starts_with rename_all
  #' @importFrom stringr str_remove
  datafile.df <- read_delim(datapath, delim = "\t", locale = locale(decimal_mark = ","))
  datafile.df <- datafile.df %>%
    select(-ends_with("(PPM)")) %>%
    select(-ends_with("(%)")) %>%
    select(-c("S", "P")) %>%
    select(starts_with("X")) %>%
    rename_all(str_remove, pattern = " .*")
  return(datafile.df)

  # import and check infopath
  #' @importFrom readxl read_excel
  #' @importFrom assertr verify has_all_names
  infofile.df <- read_excel(infopath) %>%
    verify(has_all_names("Filter_box_nr", "Filter_type", "Filter_size", "Filter_blank"))
  return(infofile.df)

}
