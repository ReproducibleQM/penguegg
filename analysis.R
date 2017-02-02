#dummy file for code
eggSize <- read.csv("https://raw.githubusercontent.com/ReproducibleQM/penguegg/master/NRPE_eggSize.csv")
head(eggSize)
plot(data = eggSize, length~year)
