#' Widening the dataframe
#'
#' @description This function will widen the dataframe and thus return the data to where each element is in a column.
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf().
#'
#' @param project.data The name of the dataframe created with convertxrf().
#'
#' @importFrom dplyr select filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = readr::locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' setup.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- importxrf(raw.data = rawdata.df, project.info = projectinfo.df)
#' project.df <- convertxrf(imported.data = projectfile.df, setup = setup.df, year = "2019", first_element = "C", last_element = "As")
#'
#' wideproject.df <- widen(project.data = project.df)
#' }
#'
#' @export

widen <- function(project.data) {

  project.df <- as.data.frame(project.data)

  projectwide.df <- project.df %>%
    dplyr::filter(!.data$Filter_blank %in% "blank") %>%
    dplyr::select(-.data$Detection_limit) %>%
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
#' @param project.data The name of the dataframe created with convertxrf().
#'
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = readr::locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' setup.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- importxrf(raw.data = rawdata.df, project.info = projectinfo.df)
#' project.df <- convertxrf(imported.data = projectfile.df, setup = setup.df, year = "2019", first_element = "C", last_element = "As")
#'
#' abovedetlim.df <- widen_above(project.data = project.df)
#' }
#'
#' @export

widen_above <- function(project.data) {

  project.df <- as.data.frame(project.data)

  projectabove.df <- project.df %>%
    dplyr::filter(.data$Concentration > .data$Detection_limit) %>%
    dplyr::filter(!.data$Filter_blank %in% "blank") %>%
    dplyr::select(-.data$Detection_limit) %>%
    tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration, id_cols = .data$Sample)

}


#' Widening the dataframe and calculating the means based on two factors
#'
#' @description This function will widen your data like widen() but also calculate the mean concentrations based on two factors, for example location and depth.
#'
#' See vignette("xrfr") for more information.
#'
#' @return description The function creates a dataframe where the columns available are your two factors (for example location and depth) and each element.
#'
#' @param project.data The name of the dataframe created with convertxrf().
#' @param first_factor The name of the column that shows the first or only factor you want to calculate means based on, for example depth.
#' @param second_factor The name of the column that shows a potential second factor you want to calculate means based on, for example location.
#'
#' @importFrom dplyr filter select group_by summarise_if
#' @importFrom tidyr pivot_wider
#' @importFrom rlang is_missing
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = readr::locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' setup.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- importxrf(raw.data = rawdata.df, project.info = projectinfo.df)
#' project.df <- convertxrf(imported.data = projectfile.df, setup = setup.df, year = "2019", first_element = "C", last_element = "As")
#'
#' means.df <- widen_means(project.data = project.df, first_factor = "Treatment", second_factor = "Day")
#' }
#'
#' @export

widen_means <- function(project.data, first_factor, second_factor) {

  project.df <- as.data.frame(project.data)

  if(rlang::is_missing(.data[[second_facor]])) {

    projectaverage.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(-.data$Detection_limit) %>%
      dplyr::select(.data[[first_factor]], .data$Volume, .data$Element, .data$Concentration) %>%
      tidyr::pivot_wider(names_from = .data$Element,
                         values_from = .data$Concentration)

  } else {

    projectaverageabove.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data[[second_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(-.data$Detection_limit) %>%
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
#' @param project.data The name of the dataframe created with convertxrf().
#' @param first_factor The name of the column that shows the first or only factor you want to calculate means based on, for example depth.
#' @param second_factor The name of the column that shows a potential second factor you want to calculate means based on, for example location.
#'
#' @importFrom dplyr filter select group_by summarise_if
#' @importFrom tidyr pivot_wider
#' @importFrom rlang is_missing
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' rawdata.df <- read_delim("xrf_rawdata.txt", delim = "\t", locale = readr::locale(decimal_mark = ","))
#' projectinfo.df <- read_excel("xrf_projectinfo.xlsx")
#' setup.df <- read_excel("xrf_setup.xlsx")
#'
#' projectfile.df <- importxrf(raw.data = rawdata.df, project.info = projectinfo.df)
#' project.df <- convertxrf(imported.data = projectfile.df, setup = setup.df, year = "2019", first_element = "C", last_element = "As")
#'
#' meansabovedetlim.df <- widen_means_above(project.data = project.df, first_factor = "Treatment", second_factor = "Day")
#' }
#'
#' @export

widen_means_above <- function(project.data, first_factor, second_factor) {

  project.df <- as.data.frame(project.data)

  if(rlang::is_missing(.data[[second_factor]])) {

    projectaverageabove.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::filter(.data$Concentration > .data$Detection_limit) %>%
      dplyr::select(-.data$Detection_limit) %>%
      dplyr::select(.data[[first_factor]], .data$Volume, .data$Element, .data$Concentration) %>%
      tidyr::pivot_wider(names_from = .data$Element,
                         values_from = .data$Concentration)

  } else {

    projectaverageabove.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::group_by(.data[[first_factor]], .data[[second_factor]], .data$Element) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::filter(.data$Concentration > .data$Detection_limit) %>%
      dplyr::select(-.data$Detection_limit) %>%
      dplyr::select(.data[[first_factor]], .data[[second_factor]], .data$Volume, .data$Element, .data$Concentration) %>%
      tidyr::pivot_wider(names_from = .data$Element,
                         values_from = .data$Concentration)

  }

}

