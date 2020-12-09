#' Importing the raw data from the XRF machine (.txt file)
#'
#' @param raw.data The name of your .txt file with raw data from the XRF machine.
#'
#' @importFrom readr read_delim locale
#' @importFrom dplyr select contains rename_all
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#'
#' @export

importdata <- function(raw.data){

  datafile.df <- readr::read_delim(raw.data, delim = "\t", locale = readr::locale(decimal_mark = ","))

  datafile.df <- datafile.df %>%
    dplyr::select(c(Sample, Date, dplyr::contains("Int"))) %>%
    dplyr::rename_all(stringr::str_remove, pattern = " .*")

  return(datafile.df)

}


#' Importing the Excel file with information about the samples
#'
#' @param project.info The name of your Excel file with necessary information about the samples.
#'
#' @importFrom readxl read_excel
#' @importFrom assertr verify has_all_names
#' @importFrom magrittr %>%
#'
#' @export

importinfo <- function(project.info) {

  infofile.df <- readxl::read_excel(project.info) %>%
    assertr::verify(assertr::has_all_names("Filter_box_nr", "Filter_type", "Filter_size", "Filter_blank"))

  return(infofile.df)

}


#' Importing the Excel file containing detection limits, crystal drift, molar weights, and calibration constants
#'
#' @param setup The name of the file containing detection limits, crystal drift, molar weights, and calibration constants.
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr relocate
#'
#' @export

importsetup <- function(setup) {

  setupfile.df <- readxl::read_excel(setup)

  setupfile.df <- setupfile.df %>%
    tidyr::pivot_longer(.data$PC : .data$GFF,
                        names_to = "Filter_type",
                        values_to = "Cal_const") %>%
    dplyr::relocate(Filter_type)

}
