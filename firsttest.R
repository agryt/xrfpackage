
projectfile.df <- importxrf(datapath = "xrf_rawdata.txt", infopath = "xrf_projectinfo.xlsx")

project.df <- convertxrf(setuppath = "xrf_setup1.xlsx", year = "2019", first_element = "C", last_element = "As")

widen()




projectwide.df <- project.df %>%
  dplyr::select(-(.data$Detection_limit)) %>%
  group_by(Treatment) %>%
  tidyr::pivot_wider(names_from = Element, values_from = Concentration)
