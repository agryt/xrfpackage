# import and check infopath
#' @importFrom readxl read_excel
#' @importFrom assertr verify has_all_names


importinfo <- function(infopath) {

  infofile.df <- read_excel(infopath) %>%
  verify(has_all_names("Filter_box_nr", "Filter_type", "Filter_size", "Filter_blank"))

  return(infofile.df)

}
