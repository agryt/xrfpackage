
projectfile.df <- importxrf(datapath = "xrf_rawdata.txt", infopath = "xrf_projectinfo.xlsx")

project.df <- convertxrf(setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")

wide.df <- widen(projectpath = "project_long.csv")

wideabove.df <- widen_above(projectpath = "project_long.csv")




projectfile.df <- importxrf(datapath = "2ANS&2AEN_merged.txt", infopath = "Infofile_forR.xlsx")

project1.df <- convertxrf(setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")






projecmeans.df <- projectwide.df %>%
  dplyr::group_by(Treatment, Day) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  dplyr::select(Day, Treatment, C:As)


averagesaug2.df <- project.aug.df %>%
  filter(Value > Detection_lim) %>%
  select(-Detection_lim) %>%
  pivot_wider(names_from = Element, values_from = Value)
