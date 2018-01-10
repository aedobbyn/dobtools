
library(feather)
starwars <- read_feather("./data/starwars.feather")
droids <- starwars[starwars$species == "Droid", ]

