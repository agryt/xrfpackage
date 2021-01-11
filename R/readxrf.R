#' Reading and joining data file and info file
#'
#' @description This function reads your two dataframes and combines them into one. It will warn you if there are any samples that do not match between your datasets (the raw data and the information file).
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function creates a dataframe where your raw data and project information has been merged.
#'
#' @param raw.data The name of your dataframe with raw data from the XRF machine.
#' @param project.info The name of your dataframe with necessary information about the samples.
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
#' projectfile.df <- readxrf(raw.data = rawdata.df, project.info = projectinfo.df)
#' }
#'
#' @export


readxrf <- function(raw.data, project.info) {

  # import data file
  datafile.df <- as.data.frame(raw.data)
  datafile.df <- datafile.df %>%
    dplyr::select(c(Sample, Date, dplyr::contains("Int"))) %>%
    dplyr::rename_all(stringr::str_remove, pattern = " .*")

  # import info file
  infofile.df <- as.data.frame(project.info)

  # assigning error messages for if not all necessary columns are included
  # THESE SHOULD BE GATHERED INTO ONE LINE
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

  # joining them into one dataframe
  projectfile.df <- dplyr::inner_join(datafile.df, infofile.df, by = "Sample")

  # making a dataframe of samples that did not match, should be 0 rows here
  notinprojectfile.df <- dplyr::anti_join(datafile.df, infofile.df, by = "Sample")

  # assigning warning message for if not all samples match
  if(nrow(notinprojectfile.df) > 0) {
    warning("WARNING! There are samples that do not match between your raw data file and information file.")
  }

  return(projectfile.df)

}
