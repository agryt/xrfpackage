data.df <- importdata(datapath = "xrf_rawdata.txt")

info.df <- importinfo("xrf_projectinfo.xlsx")

setup.df <- importsetup("xrf_setup.xlsx")

projectfile.df <- importxrf(datapath = "xrf_rawdata.txt", infopath = "xrf_projectinfo.xlsx")

project.df <- convertxrf(projectpath = "projectdata.csv", setuppath = "xrf_setup.xlsx", year = "2019", first_element = "C", last_element = "As")



