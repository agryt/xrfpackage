#' Importing and joining data file and info file
#'
#' @description This function imports your files using specific utility functions from this package,  and combines them into one dataframe. You will also get a dataframe showing any samples that did    not match between your datasets (the raw data and the information file).
#'
#' @return description The function creates a dataframe where your raw data and project information has been merged.
#'
#' @param datapath The name of your .txt file with raw data from the XRF machine.
#' @param infopath The name of your Excel file with necessary information about the samples.
#'
#' @importFrom dplyr inner_join anti_join
#'
#' @examples
#' \dontrun{
#' importxrf(datapath = "/inst/extdata/xrf_rawdata.txt", infopath = "/inst/extdata/xrf_projectinfo.xlsx")
#' # this generates a dataframe that shows up in your environment
#' }
#'
#' @export


importxrf <- function(datapath, infopath) {

  # import data file
  datafile.df <- importdata(datapath = datapath)

  # import info file
  infofile.df <- importinfo(infopath = infopath)

  # joining them into one dataframe
  projectfile.df <- dplyr::inner_join(datafile.df, infofile.df, by = "Sample")

  # making a dataframe of samples that did not match, should be 0 rows here
  notinprojectfile.df <- dplyr::anti_join(datafile.df, infofile.df, by = "Sample")

  # assigning warning messages
  if(nrow(notinprojectfile.df) > 0) {
    warning("WARNING! There are samples that do not match between your raw data file and information file.")
  }
  if(ncol(projectfile.df) < 3) {
    warning("WARNING! Your data was not imported correctly. Check that you have saved the files as correct file types. Note that your raw data .txt file must use comma as a decimal mark and tabs to separate columns.")
  }

  return(projectfile.df)

}
