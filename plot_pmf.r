library(ggplot2)

datfile <- c('wt_CypD_SFGPDL_fep.500.out', 'pCypD_SFGPDL_fep.500.out', 'SFGPDL_fep.500.out')
#datfile <- list.files('.', pattern='^fep\\..*\\.out$', recursive=TRUE, full.names=TRUE)
#datfile <- datfile[grep("wham", datfile)]
#nmaxrun <- max(as.numeric(sub("^fep.([0-9]+).out$", "\\1", basename(datfile))))
#datfile <- list.files('.', pattern=paste0('^fep\\.', nmaxrun, '\\.out$'), recursive=TRUE, full.names=TRUE)
#datfile <- datfile[grep("wham", datfile)]

dat <- NULL
for(i in datfile) {
   tdat <- read.table(i)
   tdat <- tdat[tdat[, 1]<=240, ]
#   fref <- sum(tdat[tdat[, 1]%in% c(178.75, 181.25), 2])/2 # reference to trans
   fref <- mean(tdat[tdat[, 1]%in% c(178.5,181.5), 2]) # reference to trans
   fref2 <- mean(tdat[tdat[, 1]%in% c(-1.5, 1.5), 2]) # reference to cis 
   #tdat <- cbind(tdat, as.numeric(sub(".*\\/(.*)\\/wham.*", "\\1", i))/14.0)
   tdat <- cbind(tdat, label = i) 
   #tdat[, 2] <- tdat[, 2] - (fref + fref2)/2
   tdat[, 2] <- tdat[, 2] - fref2
   dat <- rbind(dat, tdat)
}
colnames(dat) <- c("Angle", "Energy", "EnergySD", "label")
#colnames(dat) <- c("Angle", "Energy", "EnergySD", "Alpha")
#dat$Alpha <- factor(dat$Alpha, levels=as.character(seq(0, 1, 0.05)))
#dat$Alpha2 <- 1-as.numeric(as.character(dat$Alpha))

#ggplot(dat, aes(x=Angle, y=Energy, col=Alpha)) + geom_line()

#ggplot(dat, aes(x=1-as.numeric(as.character(Alpha)), y=Energy)) + 
#   geom_point() + 
#   geom_smooth(method="lm", se=FALSE, size=0.5) + 
#   facet_wrap(~Angle)

#coefs <- tapply(1:nrow(dat), dat$Angle, function(i) {
#   coef(lm(Energy~Alpha2, data=dat[i, ]))
#    summary(lm(Energy~Alpha2, data=dat[i, ], weights=1/(dat$EnergySD[i]^2)))$coefficients
#})

#dat.pred <- data.frame(Angle=as.numeric(names(coefs)), 
#   Energy=unname(sapply(coefs, "[", "(Intercept)", 1)), 
#   EnergySD=unname(sapply(coefs, "[", "(Intercept)", 2)),
#   Alpha=1.0, Alpha2=0.0, predict=TRUE)
#
#dat2 <- cbind(dat, predict=FALSE)
#dat2 <- rbind(dat2, dat.pred)

dat2 <- dat
p <- ggplot(dat2, aes(x=Angle, y=Energy, col=label)) + 
   geom_errorbar(aes(ymin=Energy-EnergySD, ymax=Energy+EnergySD), width=3, size=0.3) +
#   geom_line(aes(linetype=predict)) +
   geom_line() #+
#   ylim(-2, 26)

pdf(height=4, width=6, file="Glycine.pdf")
print(p)
dev.off()
