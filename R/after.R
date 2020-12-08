#' Widening the dataframe
#'
#' @description This function will widen the dataframe and thus return the data to where each element is in a column.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf().
#'
#' @param projectpath The CSV file created with convertxrf().
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select filter
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#'
#' @examples
#'
#' @export

widen <- function(projectpath) {

  project.df <- readr::read_csv(file = projectpath)

  projectwide.df <- project.df %>%
    dplyr::filter(!.data$Filter_blank %in% "blank") %>%
    dplyr::select(-c(1, .data$Detection_limit)) %>%
    tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration)

  return(projectwide.df)

}


#' Widening the dataframe and removing concentrations below the detection limits
#'
#' @description This function will both widen the dataframe so each element has its own column, and remove concentrations that are below the detection limits.
#'
#' @return description The function creates a dataframe that is a wider version of the one created with convertxrf(), and where values below the detection limits are removed.
#'
#' @param projectpath The CSV file created with convertxrf().
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' @export

widen_above <- function(projectpath) {

  project.df <- readr::read_csv(file = projectpath)

  projectabove.df <- project.df %>%
    dplyr::filter(.data$Concentration > .data$Detection_limit) %>%
    dplyr::select(-.data$Detection_limit) %>%
    tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration, id_cols = .data$Sample)

}


#' Widening the dataframe and calculating the means based on two factors
#'
#' @description This function will widen your data like widen() but also calculate the mean concentrations based on two factors, for example location and depth.
#'
#' @return description The function creates a dataframe where the columns available are your two factors (for example location and depth) and each element.
#'
#' @param projectpath The CSV file created with convertxrf().
#' @param first_factor The name of the column that shows the first or only factor you want to calculate means based on, for example depth.
#' @param second_factor The name of the column that shows a potential second factor you want to calculate means based on, for example location.
#' @param first_element The name of the first column containing concentration values in the generated project dataframe.
#' @param last_element The name of the last column containing concentration values in the generated project  dataframe.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter select group_by summarise_if
#' @importFrom tidyr pivot_wider
#' @importFrom rlang is_missing
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' @export

widen_means <- function(projectpath, first_factor, second_factor, first_element, last_element) {

  if(rlang::is_missing(.data[[second_facor]])) {

    project.df <- readr::read_csv(file = projectpath)

    projectwide.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::select(-c(1, .data$Detection_limit)) %>%
      tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration)

    projectaverage.df <- projectwide.df %>%
      dplyr::group_by(.data[[first_factor]]) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(.data[[first_factor]], .data[[first_element]] : .data[[last_element]])

  } else {

    project.df <- readr::read_csv(file = projectpath)

    projectwide.df <- project.df %>%
      dplyr::filter(!.data$Filter_blank %in% "blank") %>%
      dplyr::select(-c(1, .data$Detection_limit)) %>%
      tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration)

    projectaverage.df <- projectwide.df %>%
      dplyr::group_by(.data[[first_factor]], .data[[second_factor]]) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(.data[[first_factor]], .data[[second_factor]], .data[[first_element]] : .data[[last_element]])

  }

}


#' Widening the dataframe, filtering out concentrations below detection limit, and calculating the means based on two factors
#'
#' @description This function will widen your data and exclude concentrations below the detection limits like widen_above(), and will also calculate the means based on your two factors like widen_means().
#'
#' @return description The function returns a dataframe that shows the mean concentrations calculated from the concentrations above the detection limits for each element based on one or two factors.
#'
#' #' @param projectpath The CSV file created with convertxrf().
#' @param first_factor The name of the column that shows the first or only factor you want to calculate means based on, for example depth.
#' @param second_factor The name of the column that shows a potential second factor you want to calculate means based on, for example location.
#' @param first_element The name of the first column containing concentration values in the generated project dataframe.
#' @param last_element The name of the last column containing concentration values in the generated project  dataframe.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter select group_by summarise_if
#' @importFrom tidyr pivot_wider
#' @importFrom rlang is_missing
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' @export

widen_means_above <- function(projectpath, first_factor, second_factor, first_element, last_element) {

  project.df <- readr::read_csv(file = projectpath)

  projectabove.df <- project.df %>%
    dplyr::filter(.data$Concentration > .data$Detection_limit) %>%
    dplyr::filter(!.data$Filter_blank %in% "blank") %>%
    dplyr::select(-.data$Detection_limit) %>%
    tidyr::pivot_wider(names_from = .data$Element, values_from = .data$Concentration, id_cols = .data$Sample)

  if(rlang::is_missing(.data[[second_facor]])) {

    projectaverage.df <- projectabove.df %>%
      dplyr::group_by(.data[[first_factor]]) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(.data[[first_factor]], .data[[first_element]] : .data[[last_element]])

  } else {

    projectaverage.df <- projectabove.df %>%
      dplyr::group_by(.data[[first_factor]], .data[[second_factor]]) %>%
      dplyr::summarise_if(is.numeric, mean) %>%
      dplyr::select(.data[[first_factor]], .data[[second_factor]], .data[[first_element]] : .data[[last_element]])

  }

}
