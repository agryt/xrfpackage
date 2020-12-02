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


#' importing the Excel file with information about the samples

importinfo <- function(infopath) {

  infofile.df <- readxl::read_excel(infopath) %>%
    assertr::verify(assertr::has_all_names("Filter_box_nr", "Filter_type", "Filter_size", "Filter_blank"))

  return(infofile.df)

}


#' importing the Excel file containing detection limits, crystal drift, molar weights, and calibration constants

importsetup <- function(setuppath) {
  setupfile.df <- readxl::read_excel(setuppath)
  setupfile.df <- setupfile.df %>%
    tidyr::pivot_longer(.data$PC : .data$GFF,
                        names_to = "Filter_type",
                        values_to = "Cal_const")

}
