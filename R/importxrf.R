#' Importing and joining data file and info file
#'
#' @description This function imports your files using specific utility functions from this package, and combines them into one dataframe. You will also get a dataframe showing any samples that did not match between your datasets (the raw data and the information file).
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
#' df <- importxrf(raw.data = "xrf_rawdata.txt", project.info = "xrf_projectinfo.xlsx")
#' }
#'
#' system.file("extdata", "xrf_rawdata.txt", package = "xrfr")
#' system.file("extdata", "xrf_projectinfo.xlsx", package = "xrfr")
#' df <- importxrf(raw.data = "xrf_rawdata.txt", project.info = "xrf_projectinfo.xlsx")
#'
#'
#' @export


importxrf <- function(raw.data, project.info) {

  # import data file
  # datafile.df <- importdata(raw.data = raw.data)
  datafile.df <- as.data.frame(raw.data)
  datafile.df <- datafile.df %>%
    dplyr::select(c(Sample, Date, dplyr::contains("Int"))) %>%
    dplyr::rename_all(stringr::str_remove, pattern = " .*")

  # import info file
  # infofile.df <- importinfo(project.info = project.info)
  infofile.df <- as.data.frame(project.info)


  # joining them into one dataframe
  projectfile.df <- dplyr::inner_join(datafile.df, infofile.df, by = "Sample")

  # making a dataframe of samples that did not match, should be 0 rows here
  notinprojectfile.df <- dplyr::anti_join(datafile.df, infofile.df, by = "Sample")

  # assigning warning messages
  if(nrow(notinprojectfile.df) > 0) {
    warning("WARNING! There are samples that do not match between your raw data file and information file.")
  }

  return(projectfile.df)

}
