
projectfile.df <- importxrf(datapath = "xrf_rawdata.txt", infopath = "xrf_projectinfo.xlsx")

project.df <- convertxrf(setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")

widen(projectpath = "project_long.csv")




project2.df <- read_csv("project_long.csv")

projectwide.df <- project2.df %>%
  dplyr::select(-Detection_limit, -1) %>%
  tidyr::pivot_wider(names_from = Element, values_from = Concentration)

projecmeans.df <- projectwide.df %>%
  dplyr::group_by(Treatment, Day) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::select(Day, Treatment, C:As)
