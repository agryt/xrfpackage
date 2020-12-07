#' Converting XRF data from kcal to micromolar
#'
#' @description This function takes your generated project dataframe created with "importxrf()" and the file containing information from the XRF machine, and converts your raw data from kcal to micromolar.
#'
#' @return description The function creates a dataframe in the long format with the new columns "Concentration" and "Detection_limit" showing the calculated concentration and the respective detection limit.
#'
#' @param setuppath The name of the file containing detection limits, crystal drift, molar weights, and calibration constants.
#' @param year The year the drift was measured closest to when your samples were analysed.
#' @param first_element The name of the first column containing kcps values in the generated project dataframe.
#' @param last_element The name of the last column containing kcps values in the generated project  dataframe.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter group_by summarise left_join mutate select distinct
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' df <- convertxrf(setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")
#' }
#'
#' @export


convertxrf <- function(setuppath, year, first_element, last_element) {

  filter_area <- 9.078935

  # BIG WEAKNESS THAT DATAFRAME M U S T BE CALLED "PROJECTFILE.DF"! FIND A WAY AROUND THIS
  projectfile.df <- as.data.frame(projectfile.df)

  # making the dataframe longer
  pivotproject.df <- projectfile.df %>%
    tidyr::pivot_longer(first_element : last_element,
                 names_to = "Element",
                 values_to = "Count")

  # making a dataframe with the means of the blank samples
  mean.blanks.df <- pivotproject.df %>%
    dplyr::filter(.data$Filter_blank == "blank") %>%
    dplyr::group_by(.data$Filter_type, .data$Filter_size, .data$Filter_box_nr, .data$Element) %>%
    dplyr::summarise(mean_blank = mean(.data$Count))

  # joining the mean blanks dataframe with the project file
  adjusted.for.blanks.df <- dplyr::left_join(pivotproject.df, mean.blanks.df, by = c("Filter_type", "Filter_size", "Filter_box_nr", "Element")) %>%
    dplyr::mutate(Net_count = .data$Count - .data$mean_blank)

  # joining setup file with the project file
  setupfile.df <- importsetup(setuppath = setuppath)

  joined.df <- dplyr::left_join(adjusted.for.blanks.df, setupfile.df, by = c("Filter_type", "Element"))

  # performing the calculation to convert from kcps to µM
  calculations.df <- joined.df %>%
    dplyr::mutate(Concentration = ((.data$Net_count * .data$Cal_const) * filter_area * (1000 / .data$Volume) * 1000 * (.data$Drift_2008 / .data[[paste0("Drift_", year)]])) / .data$MolarW)

  # making a dataframe showing the detection limits of each element
  detectionlimits.df <- setupfile.df %>%
    dplyr::select(.data$DL_PC : .data$DL_GFF, .data$Element) %>%
    tidyr::pivot_longer(.data$DL_PC : .data$DL_GFF,
                        names_to = "Filter_type",
                        values_to = "Detection_limit") %>%
    dplyr::mutate(Filter_type = stringr::str_remove(.data$Filter_type, "DL_"))

  # joining detection limit dataframe with the project file
  project.detectionlim.df <- dplyr::left_join(calculations.df, detectionlimits.df, by = c("Filter_type", "Element"))

  # removing the columns we don't need anymore
  project.df <- project.detectionlim.df %>%
    dplyr::distinct() %>%
    dplyr::select(.data$Sample : .data$Element, .data$Concentration, .data$Detection_limit)

  return(project.df)

}
