#1 manu,popul
data("USairpollution", package = "HSAUR2")
panel.hist <- function(x,...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="gray")
}
USairpollution$negtemp <- USairpollution$temp*(-1)
USairpollution$temp <- NULL
pairs(USairpollution[,-1], diag.panel = panel.hist,pch=".",cex = 1.5)

cor(USairpollution[,-1])


usair_pca <- princomp(USairpollution[,-1], cor = TRUE)
summary(usair_pca)





install.packages("MVA")
library(MVA)
pairs(usair_pca$scores[,1:3], ylim = c(-4, 7), xlim = c(-6, 4),
panel = function(x,y,...) {
text(x, y, abbreviate(row.names(USairpollution)),cex = 0.6)
    bvbox(cbind(x,y), add = TRUE)
      })
bioenv <- read.table('https://github.com/Bio723-class/example-datasets/raw/master/bioenv.txt')
names(bioenv)

eigen(cov(abundance))
prcomp(abundance)
