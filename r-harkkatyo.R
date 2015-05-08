aineisto <- read.csv(file='asty3.csv', sep=';', stringsAsFactors=FALSE)

#Tehtävä 1
#a)
summa <- subset(aineisto, select=paste('K2A', 1:6, sep=''))
x <- rowSums(is.na(summa))
ka <- rowMeans(summa, na.rm=TRUE)

ka[x>=3] <- NA

#b)
ika <- 2014 - aineisto$T7 #Vastaajan ika 2014 vuoden lopussa.

#c)
ikaluokat <- cut(ika, breaks=c(0, 20, 40, 60, 100), include.lowest=FALSE)
levels(ikaluokat) <- c('0-20', '21-40', '41-60', '61-100')

#d)
tapply(ka, ikaluokat, mean, na.rm=TRUE) #summamuuttujan otoskeskiarvot ikäluokittain.

#Tehtävä 2
n = sum(1 - is.na(aineisto$K3A9)) #vastausten jotka eivät ole puuttuvat lkm

ttest <- t.test(aineisto$K3A9, mu=4.3)

q <- qt(0.975, df=1966)


#data:  aineisto$K3A9
#t = 3.228, df = 1966, p-value = 0.001267
#alternative hypothesis: true mean is not equal to 4.3
#97.5 percent confidence interval:
#  4.314410 4.380049
#sample estimates:
#  mean of x 
#4.347229 

ttest2 <- t.test(aineisto$K3A9, mu=4.3, conf.level=0.9)

#data:  aineisto$K3A9
#t = 3.228, df = 1966, p-value = 0.001267
#alternative hypothesis: true mean is not equal to 4.3
#90 percent confidence interval:
#  4.323152 4.371307
#sample estimates:
#  mean of x 
#4.347229 

#Odotusarvo ei kuulu luottamusvälille.


#Tehtävä 5
#a)
pallot <- function(n) {
  valkoisia_korissa <- sample(1:6, n, replace=TRUE)
  valkoisia_nostettu <- rbinom(n, 3, valkoisia_korissa/6)
  otokset <- valkoisia_korissa[valkoisia_nostettu == 1]
  fotokset <- ftable(otokset)
  jakauma <- fotokset/length(otokset)
  jakauma
}

#b)
pallot2 <- function(n) {
  valkoisia_korissa <- rbinom(n, 6, 0.5)
  valkoisia_nostettu <- rbinom(n, 3, valkoisia_korissa/6)
  otokset <- valkoisia_korissa[valkoisia_nostettu == 1]
  fotokset <- ftable(otokset)
  jakauma <- fotokset/length(otokset)
  jakauma
}

#c)
#> pallot2(100000)
#otokset          1          2          3          4          5
#
#0.10718887 0.32755733 0.37612729 0.16799794 0.02112858

#Todennäköisin thetan arvo on 2 tn:llä 0.37612729


#> pallot(100000)
#otokset          1          2          3          4          5
#
#0.23315679 0.30664696 0.25754403 0.15424724 0.04840498

#Todennäköisin thetan arvo on 1 tn:llä 0.30664696