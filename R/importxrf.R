# import data file and info file and combining them to one dataframe

#' @param datapath name of your .txt file with raw data from the XRF machine
#' @param infopath name of your Excel file with necessary information about the samples

#' @importFrom readr read_delim locale
#' @importFrom dplyr select ends_with starts_with rename_all inner_join anti_join %>%
#' @importFrom stringr str_remove
#' @importFrom readxl read_excel
#' @importFrom assertr verify has_all_names
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data


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
    warning("WARNING! There are samples that do not match between your raw data file and information file. See the dataframe named notinprojectfile.df to identify them.")
  }
  if(ncol(projectfile.df) < 3) {
    warning("WARNING! Your data was not imported correctly. Check that you have saved the files as correct file types. Note that your raw data .txt file must use comma as a decimal mark and tabs to separate columns.")
  }

  return(projectfile.df)
  return(notinprojectfile.df)
}
