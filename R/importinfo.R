# importing the Excel file with information about the samples

importinfo <- function(infopath) {

  infofile.df <- readxl::read_excel(infopath) %>%
  assertr::verify(assertr::has_all_names("Filter_box_nr", "Filter_type", "Filter_size", "Filter_blank"))

  return(infofile.df)

}
