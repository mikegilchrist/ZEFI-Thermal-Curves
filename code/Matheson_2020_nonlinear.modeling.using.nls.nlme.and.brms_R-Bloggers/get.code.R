# install_github(repo = "stevecondylios/rawr")

library(rawr)
# This works
# rawr("https://stackoverflow.com/questions/58248102/date-input-dt-r-shiny", file_path = "tmp.Rmd")

#This doesn't retrieve anything
#rawr("https://www.r-bloggers.com/2020/03/nonlinear-modelling-using-nls-nlme-and-brms",  file_path="matheson_2020.Rmd")
# This, however, does work
rawr("https://www.granvillematheson.com/post/nonlinear-modelling-using-nls-nlme-and-brms/", file_path="matheson_2020.Rmd") 
