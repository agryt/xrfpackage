
projectfile.df <- importxrf(datapath = "xrf_rawdata.txt", infopath = "xrf_projectinfo.xlsx")

project.df <- convertxrf(setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")






