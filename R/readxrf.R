#' Reading and joining data file and info file
#'
#' @description This function reads your two data frames and combines them into one. It will warn you if there are any samples that do not match between your datasets (the raw data and the information file).
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function creates a data frame where your raw data and project information has been merged.
#'
#' @param raw_data The name of your data frame with raw data from the XRF machine.
#' @param project_info The name of your data frame with necessary information about the samples.
#'
#' @importFrom dplyr inner_join anti_join select contains rename_all
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#'
#' projectfile.df <- readxrf(raw_data = rawdata.df, project_info = projectinfo.df)
#' }
#'
#' @export


readxrf <- function(raw_data, project_info) {

  # import data file
  datafile.df <- as.data.frame(raw_data)
  datafile.df <- datafile.df %>%
    dplyr::select(c(Sample, Date, dplyr::contains("Int"))) %>%
    dplyr::rename_all(stringr::str_remove, pattern = " .*")

  # import info file
  infofile.df <- as.data.frame(project_info)

  # assigning error messages for if not all necessary columns are included
  if(!"Filter_type" %in% names(infofile.df)) {
    stop("ERROR! Your project information file is missing one or more of the following columns: Filter_type, Filter_size, Filter_box_nr, Filter_blank, and Volume.")
  }
  if(!"Filter_size" %in% names(infofile.df)) {
    stop("ERROR! Your project information file is missing one or more of the following columns: Filter_type, Filter_size, Filter_box_nr, Filter_blank, and Volume.")
  }
  if(!"Filter_box_nr" %in% names(infofile.df)) {
    stop("ERROR! Your project information file is missing one or more of the following columns: Filter_type, Filter_size, Filter_box_nr, Filter_blank, and Volume.")
  }
  if(!"Filter_blank" %in% names(infofile.df)) {
    stop("ERROR! Your project information file is missing one or more of the following columns: Filter_type, Filter_size, Filter_box_nr, Filter_blank, and Volume.")
  }
  if(!"Volume" %in% names(infofile.df)) {
    stop("ERROR! Your project information file is missing one or more of the following columns: Filter_type, Filter_size, Filter_box_nr, Filter_blank, and Volume.")
  }
  if(is.numeric(infofile.df$Volume) == FALSE) {
    stop("ERROR! The column Volume in your project information file is not numeric. Make sure this column only contains numerical digits.")
  }

  # joining them into one data frame
  projectfile.df <- dplyr::inner_join(datafile.df, infofile.df, by = "Sample")

  # making a data frame of samples that did not match, should be 0 rows here
  notinprojectfile.df <- dplyr::anti_join(datafile.df, infofile.df, by = "Sample")

  # assigning warning message for if not all samples match
  if(nrow(notinprojectfile.df) > 0) {
    warning("WARNING! There are samples that do not match between your raw data file and information file.")
  }

  return(projectfile.df)

}
