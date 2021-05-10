#' Widening the dataframe
#'
#' @description This function will widen the dataframe and thus return the data to where each element is in a column.
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf().
#'
#' @param project_data The name of the dataframe created with convertxrf().
#'
#' @importFrom dplyr select filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' baseinfo.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- readxrf(raw_data = rawdata.df, project_info = projectinfo.df)
#' project.df <- convertxrf(imported_data = projectfile.df, base_info = baseinfo.df, year = "2019", first_element = "C", last_element = "As")
#'
#' wideproject.df <- widen(project_data = project.df)
#' }
#'
#' @export

widen <- function(project_data) {

  project.df <- as.data.frame(project_data)

  projectwide.df <- project.df %>%
    dplyr::filter(!.data$Filter_blank %in% "blank") %>%
    dplyr::select(-.data$Adjusted_detection_limit) %>%
    tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration)

  return(projectwide.df)

}


#' Widening the dataframe and removing concentrations below the detection limits
#'
#' @description This function will both widen the dataframe so each element has its own column, and remove concentrations that are below the detection limits.
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf(), and where values below the detection limits are removed.
#'
#' @param project_data The name of the dataframe created with convertxrf().
#'
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' baseinfo.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- readxrf(raw_data = rawdata.df, project_info = projectinfo.df)
#' project.df <- convertxrf(imported_data = projectfile.df, base_info = baseinfo.df, year = "2019", first_element = "C", last_element = "As")
#'
#' abovedetlim.df <- widen_above(project_data = project.df)
#' }
#'
#' @export

widen_above <- function(project_data) {

  project.df <- as.data.frame(project_data)

  projectabove.df <- project.df %>%
    dplyr::filter(.data$Concentration > .data$Adjusted_detection_limit) %>%
    dplyr::filter(!.data$Filter_blank %in% "blank") %>%
    dplyr::select(-.data$Adjusted_detection_limit) %>%
    tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration)

}


#' Widening the dataframe and calculating the means based on two factors
#'
#' @description This function will widen your data like widen() but also calculate the mean concentrations based on two factors, for example location and depth.
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function creates a dataframe where the columns available are your two factors (for example location and depth) and each element.
#'
#' @param project_data The name of the dataframe created with convertxrf().
#' @param first_factor The name of the column that shows the first or only factor you want to calculate means based on, for example depth.
#' @param second_factor The name of the column that shows a potential second factor you want to calculate means based on, for example location.
#'
#' @importFrom dplyr filter select group_by summarise_if
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' baseinfo.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- readxrf(raw_data = rawdata.df, project_info = projectinfo.df)
#' project.df <- convertxrf(imported_data = projectfile.df, base_info = baseinfo.df, year = "2019", first_element = "C", last_element = "As")
#'
#' means.df <- widen_means(project_data = project.df, first_factor = "Treatment", second_factor = "Day")
#' }
#'
#' @export

widen_means <- function(project_data, first_factor, second_factor = NULL) {

  project.df <- as.data.frame(project_data)

  if(is.null(second_factor)) {

    projectaverage.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(-.data$Adjusted_detection_limit) %>%
      dplyr::select(.data[[first_factor]], .data$Volume, .data$Element, .data$Concentration) %>%
      tidyr::pivot_wider(names_from = .data$Element,
                         values_from = .data$Concentration)

  } else {

    projectaverageabove.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data[[second_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(-.data$Adjusted_detection_limit) %>%
      dplyr::select(.data[[first_factor]], .data[[second_factor]], .data$Volume, .data$Element, .data$Concentration) %>%
      tidyr::pivot_wider(names_from = .data$Element,
                         values_from = .data$Concentration)

  }

}


#' Widening the dataframe, filtering out concentrations below detection limit, and calculating the means based on two factors
#'
#' @description This function will widen your data and exclude concentrations below the detection limits like widen_above(), and will also calculate the means based on your two factors like widen_means().
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function returns a dataframe that shows the mean concentrations calculated from the concentrations above the detection limits for each element based on one or two factors.
#'
#' @param project_data The name of the dataframe created with convertxrf().
#' @param first_factor The name of the column that shows the first or only factor you want to calculate means based on, for example depth.
#' @param second_factor The name of the column that shows a potential second factor you want to calculate means based on, for example location.
#'
#' @importFrom dplyr filter select group_by summarise_if
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' baseinfo.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- readxrf(raw_data = rawdata.df, project_info = projectinfo.df)
#' project.df <- convertxrf(imported_data = projectfile.df, base_info = baseinfo.df, year = "2019", first_element = "C", last_element = "As")
#'
#' meansabovedetlim.df <- widen_means_above(project_data = project.df, first_factor = "Treatment", second_factor = "Day")
#' }
#'
#' @export

widen_means_above <- function(project_data, first_factor, second_factor = NULL) {

  project.df <- as.data.frame(project_data)

  if(is.null(second_factor)) {

    projectaverageabove.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::filter(.data$Concentration > .data$Adjusted_detection_limit) %>%
      dplyr::select(-.data$Adjusted_detection_limit) %>%
      dplyr::select(.data[[first_factor]], .data$Volume, .data$Element, .data$Concentration) %>%
      tidyr::pivot_wider(names_from = .data$Element,
                         values_from = .data$Concentration)

  } else {

    projectaverageabove.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data[[second_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::filter(.data$Concentration > .data$Adjusted_detection_limit) %>%
      dplyr::select(-.data$Adjusted_detection_limit) %>%
      dplyr::select(.data[[first_factor]], .data[[second_factor]], .data$Volume, .data$Element, .data$Concentration) %>%
      tidyr::pivot_wider(names_from = .data$Element,
                         values_from = .data$Concentration)

  }

}


#' Showing the mean blanks
#'
#' @description This function will extract only the mean blank kcps values calculated for each filter type, size, and box number, and present them as a dataframe.
#'
#' @return description The function creates a dataframe showing the mean blank values in kcps per filter type, size, and filter box number.
#'
#' @param imported_data The name of the dataframe created with readxrf()
#' @param first_element The name of the first column containing kcps values in the project dataframe.
#' @param last_element The name of the last column containing kcps values in the project dataframe.
#'
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr filter group_by summarise
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#'
#'
#' @export

show_blanks <- function(imported_data, first_element, last_element) {

  projectfile.df <- as.data.frame(imported_data)

  # transforming the data to long format
  pivotproject.df <- projectfile.df %>%
    tidyr::pivot_longer(first_element : last_element,
                        names_to = "Element",
                        values_to = "Count")

  # making a dataframe with the means of the blank samples
  mean.blanks.df <- pivotproject.df %>%
    dplyr::filter(.data$Filter_blank == "blank") %>%
    dplyr::group_by(.data$Filter_type, .data$Filter_size, .data$Filter_box_nr, .data$Element) %>%
    dplyr::summarise(mean_blank = mean(.data$Count))

  # transforming the data back to wide format
  wide.mean.blanks.df <- mean.blanks.df %>%
    tidyr::pivot_wider(names_from = .data$Element, values_from = .data$mean_blank)

  return(wide.mean.blanks.df)

}
