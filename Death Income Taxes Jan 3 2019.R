
remove(list=ls())
library(readxl)

# Read in Mortality 2006-2015 data from .txt file
Mortality <- read.csv("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\Mortality 2006-2015.txt", header = TRUE, sep = '\t')
Mortality <- Mortality[, c(1,3,5,7,9,10)]

# Read in Median Household Income Data from Census
# Table H-8.  Median Household Income by State: 1984 to 2015																																																																		
# Households as of March of the following year. Income in current and 2015 CPI-U-RS adjusted dollars.																																																																		
medIncome <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\Median Household Income by State.xlsx")

# Per Capita Income
# Source: U.S. Department of Commerce, Bureau of Economic Analysis, Survey of Current Business. Web: http://www.bea.gov/newsreleases/relsarchivespi.htm 
StatePerCapita <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\StatePerCapitaIncome.xlsx")

# Education level by state 2005; proportions among those 25+
# https://census.gov/data/tables/2005/demo/educational-attainment/cps-detailed-tables.html
StateEduc <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\State Education Level 2005.xlsx")

# Read in Tax Data
StateTaxBurden <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\Tax Foundation Burden of Taxes.xlsx")

# Read in State Income Taxation Metrics from TPC 2011
# http://www.taxpolicycenter.org/sites/default/files/publication/131621/2000847-federal-state-income-tax-progressivity.pdf
StateTPC <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\TPC State Income Progressivity 2011.xlsx")
StateTPC <- StateTPC[, c(1,6,2,5)] # Drops the two measures of progressivity of the state tax 

# Read in State Income Rates by Income Brackets
# https://www.cga.ct.gov/2005/rpt/2005-r-0472.htm, manually abstracted using tables
StateRates <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\State Income 2005 Rates Bracket 50 100 200 500K.xlsx")
Inc <- c(25,50,100,200,500)/1000
SITR.Slope <- as.matrix(StateRates[,2:6]) %*% (Inc - mean(Inc)) / sum((Inc - mean(Inc))^2)
StateRates <- cbind(StateRates, SITR.Slope)

# Read in ITEP table of tax burden by state and income level from 2003
StateTB.By.Income.2003 <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\StateTaxBurdenByIncome2003ITEP.xlsx")
Q5.TB <- as.matrix(StateTB.By.Income.2003[, c("P8095.TB", "P9599.TB", "P99.TB")]) %*% (I <- c(126.2, 280.4, 1593.4))/sum(I)
Q.Ctr <- c(10,30,50,70,87,97,99.5)/100
TB.Slope <- as.matrix(StateTB.By.Income.2003[,2:8]) %*% (Q.Ctr - mean(Q.Ctr)) / sum((Q.Ctr - mean(Q.Ctr))^2)
StateTB.By.Income.2003 <- cbind(StateTB.By.Income.2003, Q5.TB, TB.Slope)

# Read in ITEP table of Income Tax rate by state and income level from 2003 
StateRate.By.Income.2003 <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\StateTaxIncomeRateByIncomeITEP.xlsx")
Q5.StateRate <- as.matrix(StateRate.By.Income.2003[, c("P8095.IncomeRate", "P9599.IncomeRate", "P99.IncomeRate")]) %*% (I <- c(126.2, 280.4, 1593.4))/sum(I)
wt <- (c(9.3,19.7,31.9,52.5,95.3,202.3,1080.9)*c(0.2,0.2,0.2,0.2,0.15,0.04,0.01))
Overall.IncomeRate <- as.matrix(StateRate.By.Income.2003[,2:8]) %*% wt/sum(wt)
Q.Ctr <- c(10,30,50,70,87,97,99.5)/100
Q.IncomeRate.Slope <- as.matrix(StateRate.By.Income.2003[,2:8]) %*% (Q.Ctr - mean(Q.Ctr)) / sum((Q.Ctr - mean(Q.Ctr))^2)
StateRate.By.Income.2003 <- cbind(StateRate.By.Income.2003, Q5.StateRate, Overall.IncomeRate, Q.IncomeRate.Slope)

# Read state sales tax from: www.taxpolicycenter.org/file/62201/download?token=l5U7d74Y
# I used food exempt to mean no tax on food 
StateSalesTax2005 <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\State Sales Tax 2005.xlsx")
SalesTaxFood <- StateSalesTax2005$StateSalesTax *  (1-StateSalesTax2005$FoodExempt)
StateSalesTax2005 <- cbind(StateSalesTax2005, SalesTaxFood)


# Read in corporate tax data
StateCorporateTax2005 <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\State Corporate Taxes 2005.xlsx")
AveCorp <- rowMeans(StateCorporateTax2005[,2:5])
StateCorporateTax2005 <- cbind(StateCorporateTax2005, AveCorp)

# Expendituresy State in 2005
# https://www.taxpolicycenter.org/statistics/state-and-local-general-expenditures-capita
StateExpenditures2005 <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\State and Local Expenditures Per Capita 2005.xlsx")
StateExpenditures2005[, -1] <- (1/1000) * StateExpenditures2005[, -1] # In Thousands of $

# Examination of per capita income over time
pairs(StatePerCapita[, -1])
Inc.Cov <- cor(StatePerCapita[, -1])
n.yr <- dim(StatePerCapita)[2]-1
rho <- rep(NA, n.yr-1)
for (i in 1:(n.yr-1))
  rho[i] <- mean(diag(Inc.Cov[1:(n.yr-1),(1+i):n.yr]))
rho
plot(c(1980,2015), range( StatePerCapita[, 1 + (1:n.yr)]), type="n", xlab="Year", ylab="Per Capital Income")
for (i in 1:51) lines(as.numeric(names(StatePerCapita)[-1]), StatePerCapita[i, -1])

# Examination of median income over time
pairs(medIncome[,seq(from=2,to=10,by=2)])
cols <- seq(from=2,to=62,by=2)
Inc.Cov <- cor(medIncome[, cols])
rho <- rep(NA, 30)
for (i in 1:30)
  rho[i] <- mean(diag(Inc.Cov[1:(31-i), (1+i):31]))
plot(c(1985,2015), range(medIncome[, cols]), type="n", xlab="Year", ylab="Median Income")
for (i in 1:51) lines(2015:1985, medIncome[i, cols])
 

# Examination of tax burden over time
# DC had a big cut 2008-2009; note Kansas cut occurred in 2012 after this timeframe
pairs( StateTaxBurden[,-c(1,16)])
Inc.Cov <- cor(StateTaxBurden[, -c(1,16)])
n.yr <- dim(StateTaxBurden)[2]-2
rho <- rep(NA, n.yr-1)
for (i in 1:(n.yr-1))
  rho[i] <- mean(diag(Inc.Cov[1:(n.yr-1),(1+i):n.yr]))
plot(c(2005,2012), range( StateTaxBurden[, 1 + (1:n.yr)]), type="n", xlab="Year", ylab="State Tax Burden %")
for (i in 1:51) lines(as.numeric(names(StateTaxBurden)[-c(1,16)]), StateTaxBurden[i, -c(1,16)])

# Figure of Tax Burden by Income
par(mfrow=c(1,2))
plot(c(1,7), range(StateTB.By.Income.2003[,2:8]), axes=FALSE, type="n", xlab="Percentile of Income", ylab="State Tax Burden %", main="State Tax Burden by Income")
axis(1, 1:7, c("0-19", "20-39", "40-59", "60-79", "80-94","95-98","Top 1%"))
axis(2)
for (i in 1:51) lines(1:7, StateTB.By.Income.2003[i, 2:8])

# main factors of Tax Burden by Income 
(Eig <- eigen(cov(StateTB.By.Income.2003[,2:8])))
#(Eig <- eigen(cov(StateTB.By.Income.2003[,c(2:5,9)])))
Prop <- Eig$values/sum(Eig$values)
cumsum(Prop)
Eig$vectors[,1] <- -Eig$vectors[,1]
plot(c(1,7), range(Eig$vectors), axes=FALSE, type="n", xlab="Percentile of Income", ylab="", main="Principal Components", sub="Line Widths Proportion to Variation Explained")
axis(1, 1:7, c("0-19", "20-39", "40-59", "60-79", "80-94","95-98","Top 1%"))
axis(2)
for (i in 1:7) lines(1:7, Eig$vectors[,i], lwd=round(100*Prop[i]))


# Figure of Income Tax Rate by Income
par(mfrow=c(1,2))
plot(c(1,5), range(StateRates[,2:6]), axes=FALSE, type="n", xlab="Income in 2005", ylab="Income Tax Rate (%)", main="State Income Tax Rate by Income")
axis(1, 1:5, c("$25K", "$50K", "$100K", "$200K", "$500K"))
axis(2)
rand.budge <- runif(51)*0.2
for (i in 1:51) lines(1:5, StateRates[i, 2:6] + rand.budge[i])

# main factors of Income Tax Rate by Income 

########## For class demo
Y <- as.matrix(StateRates[,2:6])
factanal(~as.matrix(Five.Year.Age.Groups), factors=2)
summary(lm(c(as.matrix(Y)) ~ -1 + rep(1, 5*51) + rep(1:5, each=51)))
sum(Y^2)
sum((Y - mean(Y))^2)
sum((Y - mean(Y))^2)
#Intercept <- 
#Slope <- (Y-rowMeans(Y)) %*% (1:5)/(5*sqrt(var(1:5)))
Predicted <- t(lm(t(Y) ~ xx)$coef) %*% t(cbind(1,1:5))
sum((Y - Predicted)^2) 
################################################

(Eig <- eigen(cov(StateRates[,2:6])))
Prop <- Eig$values/sum(Eig$values)
cumsum(Prop)
Eig$vectors[,1] <- -Eig$vectors[,1]
plot(c(1,5), range(Eig$vectors), axes=FALSE, type="n", xlab="Percentile of Income", ylab="", main="Principal Components", sub="Line Widths Proportion to Variation Explained")
axis(1, 1:5, c("$25K", "$50K", "$100K", "$200K", "$500K"))
axis(2)
for (i in 1:5) lines(1:5, Eig$vectors[,i], lwd=round(100*Prop[i]))

# Figure of Corporate Income Tax Rate by Income
par(mfrow=c(1,2))
plot(c(1,4), range(StateCorporateTax2005[,2:5]), axes=FALSE, type="n", xlab="Income in 2005", ylab="Income Tax Rate (%)", main="State Income Tax Rate by Income")
axis(1, 1:4, c("$50K", "$100K", "$200K", "$500K"))
axis(2)
rand.budge <- runif(51)*0.2
for (i in 1:51) lines(1:4, StateRates[i, 2:5] + rand.budge[i])

# main factors of Crporate Income Tax Rate by Income 
(Eig <- eigen(cov(StateCorporateTax2005[,2:5])))
Prop <- Eig$values/sum(Eig$values)
cumsum(Prop)
Eig$vectors[,1] <- -Eig$vectors[,1]
plot(c(1,4), range(Eig$vectors), axes=FALSE, type="n", xlab="Percentile of Income", ylab="", main="Principal Components", sub="Line Widths Proportion to Variation Explained")
axis(1, 1:4, c("$50K", "$100K", "$200K", "$500K"))
axis(2)
for (i in 1:4) lines(1:4, Eig$vectors[,i], lwd=round(100*Prop[i]))



# Merge all datasets
names(StatePerCapita)[-1] <- paste("PCI.", names(StatePerCapita)[-1], sep="")
names(StateTaxBurden)[-c(1,16)] <- paste("TB.", names(StateTaxBurden)[-c(1,16)], sep="")

DFs <- merge(medIncome[,c("State","Median2005")], StatePerCapita[, c("State","PCI.2003")], by="State", all.x=TRUE)
DFs <- merge(DFs, StateEduc, by="State", all.x=TRUE)
DFs <- merge(DFs, StateTaxBurden[, c("State","TB.2005","State2L")], by="State", all.x=TRUE)
DFs <- merge(DFs, StateTB.By.Income.2003, by="State", all.x=TRUE)
DFs <- merge(DFs, StateRate.By.Income.2003, by="State", all.x=TRUE)
DFs <- merge(DFs, StateTPC, by="State", all.x=TRUE)
DFs <- merge(DFs, StateRates, by="State", all.x=TRUE)
DFs <- merge(DFs, StateSalesTax2005, by="State", all.x=TRUE)
DFs <- merge(DFs, StateCorporateTax2005, by="State", all.x=TRUE)
State.Measures <- merge(DFs, StateExpenditures2005, by="State", all.x=TRUE)
# End of Tax and Expend Data being read in
NoIncomeTax <- is.element(State.Measures$State, c("Alaska","Florida","Nevada","New Hampshire","South Dakota","Texas","Washington", "Wyoming"))
State.Measures <- cbind(State.Measures, NoIncomeTax)
State.Measures <- State.Measures[, c(1,7,2:6,8:dim(State.Measures)[2])]
State.Measures[,c("Median2005", "PCI.2003")] <- (1/1000) * State.Measures[,c("Median2005", "PCI.2003")]


##############################################
# Finalization of the analytic dataset
DF <- merge(Mortality, State.Measures, by="State", all.x=TRUE)


StateAbbrev <- unique(DF[order(DF[,"State"]), "State2L"])
StatePop <- tapply(DF[,"Population"], DF[,"State"], sum)

Male <- ifelse(DF[,"Gender"] == "Male", 1, ifelse(DF[,"Gender"] == "Female", 0, NA))
Black <- DF[,"Race"] == "Black or African American "
Asian.PI <- DF[,"Race"] == "Asian or Pacific Islander"
Other.Race <- DF[,"Race"] == "American Indian or Alaska Native"
Race_ <- factor(c(4,3,2,1)[as.numeric(DF[,"Race"])], labels=c("White","Black","Asian or PI","FirstNation")) 
Age.Dummies <- matrix(nrow=dim(DF)[1], ncol=(n.cat <- length(lev <- levels(DF$Five.Year.Age.Groups))))
for (i in 1:n.cat) Age.Dummies[,i] <- DF$Five.Year.Age.Groups == lev[i]
dimnames(Age.Dummies)[[2]] <- paste("Age", substr(lev,1,2), sep=".")
DF <- cbind(DF, Age.Dummies)

###############################################
# All code below uses this analytic dataset
###############################################
write.table(DF, "c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\PLOS One Accepted Documents\\State_Tax_vs_Mortality_Data.txt", sep="\t", row.names=TRUE)



# Demographic Aggregates to add to State Measures
Total <- tapply(DF[,"Population"], DF[,"State"], sum)
Race.Perc <- 100 * tapply(DF[,"Population"], list(DF[,"State"],DF[,"Race"]), sum) / as.vector(Total)
Gender.Perc <- 100* tapply(DF[,"Population"], list(DF[,"State"],DF[,"Gender"]), sum) / as.vector(Total)
Age.40.Perc <- 100 * tapply(DF[,"Population"], list(DF[,"State"],DF[,"Age.40"]), sum)[,"TRUE"] / as.vector(Total)
Age.45.Perc <- 100 * tapply(DF[,"Population"], list(DF[,"State"],DF[,"Age.45"]), sum)[,"TRUE"] / as.vector(Total)
Age.50.Perc <- 100 * tapply(DF[,"Population"], list(DF[,"State"],DF[,"Age.50"]), sum)[,"TRUE"] / as.vector(Total)
Age.55.Perc <- 100 * tapply(DF[,"Population"], list(DF[,"State"],DF[,"Age.55"]), sum)[,"TRUE"] / as.vector(Total)
Age.60.Perc <- 100 * tapply(DF[,"Population"], list(DF[,"State"],DF[,"Age.60"]), sum)[,"TRUE"] / as.vector(Total)
State.Demo <- cbind(Race.Perc, Gender.Perc, Age.40.Perc, Age.45.Perc, Age.50.Perc, Age.55.Perc, Age.60.Perc)

State.Measures <- merge(State.Measures, State.Demo, by.x="State", by.y="row.names")
########################################################################################

MedInc <- DF$Median2015 / 10000

pairs(DF[,7:12])

# Some analyses may exclude HI and AK
Not.HI.AK <- DF$State != "Hawaii" & DF$State != "Alaska"

###################################################
# Decriptive Results
###################################################

### Table of mortality by demographics
Mort.Rate <- 1000 * tapply(DF$Deaths, by<-list(DF$Five.Year.Age.Groups, DF$Race, DF$Gender), sum) /  tapply(DF$Population, by, sum)
M <- rbind(Mort.Rate[,,1], Mort.Rate[,,2])
dimnames(M)[[1]] <- paste(c("Female", rep("",4), "Male", rep("",4)), dimnames(M)[[1]])
write.table(M, "c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\Results\\Mortality by Demo.txt", sep="\t",row.names=TRUE)


###########################################################################
### Table of Quantiles of the exposures; variation across states
Quantiles <- matrix(nrow=dim(State.Measures)[2]-2, ncol=5)
for (i in 1:(dim(State.Measures)[2]-2)) {
  Quantiles[i,] <- quantile(State.Measures[,i+2], na.rm=TRUE) 	
}
dimnames(Quantiles)[[1]] <- names(State.Measures)[-(1:2)]
dimnames(Quantiles)[[2]] <- c("Minimum", "1st Quartile", "Median", "2nd Quartile", "Maximum")
Quantiles
write.table(Quantiles, "c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\Results\\Quantiles.txt", sep="\t",row.names=TRUE)

################################
# Outcome Results
################################

# Number of deaths
sum(DF$Deaths)
# Proportion of cells provided (i.e. # deaths>=10)
dim(Mortality)[1]/(50*5*4*2)
50*5*4*2 - dim(Mortality)[1] # number of missing cells
# should we impute 5?
# Upper limit on proportion of deaths missed
(169*9)/sum(DF$Deaths)


# Model with sex, race and age group, and their interactions
NullModel <- glm(cbind(Deaths, Population - Deaths) ~ Male*Race_*Five.Year.Age.Groups, data=DF, family=binomial)
df.null <- NullModel$rank

OoERagg <- tapply(DF$Deaths, DF$State, sum) / tapply(DF$Population * NullModel$fit, DF$State, sum)
OoERagg.SE <- sqrt(tapply(DF$Deaths, DF$State, sum)) / tapply(DF$Population * NullModel$fit, DF$State, sum)
log.scale.SE <- OoERagg.SE / OoERagg 

#############################################################
### Figure: State mortality adjusted for sex/race/age by state 
#############################################################
CI <- OoERagg * exp(data.frame(Lo=-1.96 * log.scale.SE, Up=+1.96 * log.scale.SE))
Ord <- order(OoERagg)
plot(sort(OoERagg), 1:51, ylab="", axes=FALSE, xlab="Decreased or Excess Mortality (%)")
axis(1, x.grid <- (7:14)/10, 100*(x.grid-1))
axis(2, 1:51, names(OoERagg)[Ord], cex.axis=0.5, las=2) 
for (i in 1:51) lines(CI[Ord,][i,], rep(i,2))

# Model with state income and state education, sex, race and age group, and their interactions
IncEducModel <- glm(cbind(Deaths, Population - Deaths) ~ Median2005 + PCI.2003 + HSPerc + BachPerc + Male*Race_*Five.Year.Age.Groups, data=DF, family=binomial)

OoER.IE.agg <- tapply(DF$Deaths, DF$State, sum) / tapply(DF$Population * IncEducModel$fit, DF$State, sum)
OoER.IE.agg.SE <- sqrt(tapply(DF$Deaths, DF$State, sum)) / tapply(DF$Population * IncEducModel$fit, DF$State, sum)
log.scale.SE <- OoER.IE.agg.SE / OoER.IE.agg 

### Figure mortality adjusted for ex/race/age and state income and education by state 
CI <- OoER.IE.agg * exp(data.frame(Lo=-1.96 * log.scale.SE, Up=+1.96 * log.scale.SE))
Ord <- order(OoER.IE.agg)
plot(sort(OoER.IE.agg), 1:51, axes=FALSE, xlab="Excess or Decreased Mortality (%)")
axis(1, x.grid <- (7:14)/10, 100*(x.grid-1))
axis(2, 1:51, names(OoER.IE.agg)[Ord], cex.axis=0.5, las=2) 
for (i in 1:51) lines(CI[Ord,][i,], rep(i,2))

#
Outcome <- cbind(DF$Deaths, DF$Population - DF$Deaths)

#############################################################
# Associations of mortality with demo, income and education
Covar <- data.frame(Male, DF[, c("Median2005", "PCI.2003", "HSPerc", "BachPerc")])
Coef <- matrix(nrow=dim(Covar)[2], ncol=4)
for (i in 1:dim(Coef)[1]) {
  Coef[i,] <- summary(glm(as.formula(paste("Outcome ~", names(Covar)[i], "+ Male*Race_*Five.Year.Age.Groups")), family=binomial, data=DF))$coef[2, ]
}
dimnames(Coef)[[1]] <- names(Covar)
Coef <- rbind(Coef, summary(glm(Outcome ~ Race_ + Male*Five.Year.Age.Groups, family=binomial, data=DF))$coef[2:4, ])
Coef <- rbind(Coef, summary(glm(Outcome ~ Five.Year.Age.Groups + Male*Race_, family=binomial, data=DF))$coef[2:5, ])
Ratio <- exp(Coef[,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2)))
write.table(Ratio, "c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\Results\\Covariate Effects.txt", sep="\t", row.names=TRUE)

par(mfrow=c(1,1))
n.var <- dim(Coef)[1]
plot(c(-0.5,1), c(1, n.var), axes=FALSE, type="n", xlab="Odds Ratio", ylab="")
abline(v=0.5)
Range <-c(1/5,5)
Scale <- diff(log(Range))
Ticks <- c(1/4,1/2,1,2,4)
axis(1, 0.5+log(Ticks)/Scale, Ticks)
text(rep(0, n.var), n.var:1, dimnames(Coef)[[1]], adj=1)
points(0.5+Coef[,1]/Scale, n.var:1)
for (i in 1:n.var) {lines(0.5+(Coef[i,1]+Coef[i,2]*c(-2,2))/Scale, rep(n.var+1-i,2))}

###########################################
# Effects of State Taxation levels
###########################################

# Reduce list
Redux.State.Measures <- State.Measures[, c("TB.2005", "TB.Slope", "Overall.IncomeRate", "Q.IncomeRate.Slope", "AveCorp", "StateSalesTax", "SalesTaxFood")]

# Univariate coefficients adjusted for sex,race, age 
tax.names <- names(Redux.State.Measures)[]
Coef <- matrix(nrow=length(tax.names), ncol=4)
for (i in 1:dim(Coef)[1]) {
  Coef[i,] <- summary(glm(as.formula(paste("Outcome ~", tax.names[i], "+ Male*Race_*Five.Year.Age.Groups")), family=binomial, data=DF))$coef[2, ]
}
dimnames(Coef)[[1]] <- tax.names
Coef.1 <- Coef
Ratio.1 <- exp(Coef.1[,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2)))

# Univariate coefficients adjusted for sex, race, age and income and education - sensitive model (high dimensional fit)
Coef <- matrix(nrow=length(tax.names), ncol=4)
for (i in 1:dim(Coef)[1]) {
  Coef[i,] <- summary(glm(as.formula(paste("Outcome ~", tax.names[i], " + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups")), family=binomial, data=DF))$coef[2, ]
}
dimnames(Coef)[[1]] <- tax.names
Coef.2 <- Coef
Ratio.2 <- exp(Coef.2[,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2)))

# Is Tax Burden significant with random effect for state
summary(o <- glmer(Outcome ~ TB.2005 + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=DF))

Tbl.Tax <- cbind(Ratio.1, Ratio.2)
Tbl.Tax

summary(glm(Outcome ~ TB.2005 + TB.Slope+ cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, family=binomial, data=DF))$coef[2:3, ]
summary(glm(Outcome ~ TB.2005 + TB.Slope+ cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, subset=!NoIncomeTax, family=binomial, data=DF))$coef[2:3, ]


# Effect of Income Tax in States with Income Tax
summary(glm(Outcome ~ Overall.IncomeRate + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, subset=!NoIncomeTax, family=binomial, data=DF))$coef[2:3, ]
summary(glm(Outcome ~ Overall.IncomeRate + SITR.Slope + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, subset=!NoIncomeTax, family=binomial, data=DF))$coef[2:3, ]


# Model incorporating rather distinct parts
summary(glm(Outcome ~ Overall.IncomeRate + StateSalesTax + AveCorp + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, family=binomial, data=DF))$coef[2:4, ]

# Ignoring population size
summary(lm(OoERagg ~ State.Measures$TB.2005))$coef[2,]
# Using lm but weighted
summary(lm(OoERagg ~ State.Measures$TB.2005, weight=StatePop))$coef[2,]


#Effect of tax burden and income tax in states with income tax
summary(glm(Outcome ~ TB.2005 + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, subset=!NoIncomeTax, family=binomial, data=DF))$coef[2, ]
summary(glm(Outcome ~ Q5StateIncomePerc + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, subset=!NoIncomeTax, family=binomial, data=DF))$coef[2, ]
summary(glm(Outcome ~ Overall.IncomeRate + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, subset=!NoIncomeTax, family=binomial, data=DF))$coef[2:3, ]
summary(glm(Outcome ~ Overall.IncomeRate  + Q.IncomeRate.Slope + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, subset=!NoIncomeTax, family=binomial, data=DF))$coef[2:3, ]

summary(glm(Outcome ~ TB.2005 + Q5StateIncomePerc + RateAt25 + FoodExempt + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, family=binomial, data=DF))$coef[2:5,]
  
#summary(glm(Outcome ~ TB.2005 + Q5StateIncomePerc + RateAt25 + FoodExempt + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, family=binomial, data=DF))$coef[2:5,]

#summary(glm(Outcome ~ TB.2005 + RateAt200 + RateAt25 + FoodExempt + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, family=binomial, data=DF))$coef[2:5,]

# Restrict to states with income tax
Coef <- summary(glm(Outcome ~ RateAt25 + RateAt50 + RateAt100 + RateAt200 + RateAt500 + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups, family=binomial, data=DF, subset=!NoIncomeTax))$coef[2:6,]
par(mfrow=c(1,1))
plot(1:5, exp(Coef[,"Estimate"]), pch=16, log="y", xlab="Income ($1000)", ylab="Mortality Change % per 1% Income Tax", ylim=c(0.85,1.15), axes=FALSE)
for (i in 1:5) lines(c(i,i), exp(Coef[i,"Estimate"]+c(-2,2)*Coef[i,"Std. Error"]))
abline(h=1)
axis(1, 1:5, c("25","50","100","200","500"))
axis(2, x.grid<-c(0.90, 1, 1.1), 100*(x.grid-1))

#########################################################
#########################################################
# PLOS One's request for expenditures
#########################################################
#########################################################

# Association of Tax Burden with Spending 
# It affects PrimSec, Welfare and Police but not HealthHosp, PostSec, Highways
Pop <- tapply(DF$Population, DF$State, sum)
summary(lm(State.Measures$EducationPrimSec ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$EducationPostSec ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$Welfare ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$HealthHospitals ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$Highways ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$Police ~ State.Measures$TB.2005, weight=Pop))$coef[2,]

par(mfrow=c(2,2))
plot(State.Measures$TB.2005, State.Measures$EducationPrimSec, cex=sqrt(Pop/10^7))
plot(State.Measures$TB.2005, State.Measures$HealthHospitals, cex=sqrt(Pop/10^7))
plot(State.Measures$TB.2005, State.Measures$Welfare, cex=sqrt(Pop/10^7))
plot(State.Measures$TB.2005, State.Measures$Police, cex=sqrt(Pop/10^7))

#########################################################
# Mortality by Expenditures
#########################################################

# Figures of Adjusted Mortality by Expenditures


# log base 1.1 will allow us to assess effect of a 10% increase
log.1.1.Expenditures <- log(DF[, names(StateExpenditures2005)[-1]])/log(1.1)
names(log.1.1.Expenditures) <- paste("log.1.1", names(StateExpenditures2005)[-1], sep="")

# Univariate coefficients adjusted for sex,race, age 
expend.names <- names(log.1.1.Expenditures)[]
Coef <- matrix(nrow=length(expend.names), ncol=4)
for (i in 1:dim(Coef)[1]) {
  Coef[i,] <- summary(glm(as.formula(paste("Outcome ~", expend.names[i], "+ Male*Race_*Five.Year.Age.Groups")), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef[2, ]
}
dimnames(Coef)[[1]] <- expend.names
Coef.Expend <- Coef
(Ratio.Expend.1 <- exp(Coef.Expend[,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2))))

# Univariate coefficients adjusted for sex,race, age 
expend.names <- names(log.1.1.Expenditures)[]
Coef <- matrix(nrow=length(expend.names), ncol=4)
for (i in 1:dim(Coef)[1]) {
  Coef[i,] <- summary(glm(as.formula(paste("Outcome ~", expend.names[i], "+ cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups")), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef[2, ]
}
dimnames(Coef)[[1]] <- expend.names
Coef.Expend.2 <- Coef
(Ratio.Expend.2 <- exp(Coef.Expend.2[,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2))))

Tbl.Expend <- cbind(Ratio.Expend.1, Ratio.Expend.2)

Tbl.Main <- rbind(Tbl.Tax, Tbl.Expend)
dimnames(Tbl.Main)[[2]] <- c("Odds Ratio 1","Low 1", "Up 1", "Odds Ratio 2","Low 2", "Up 2")
write.table(Tbl.Main, "c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\Results\\Table Main.txt", sep="\t", row.names=TRUE)
round(100*(Tbl.Main-1),1)


# P-values
for (i in 1:dim(Coef)[1]) {
  cat(expend.names[i], "\n")
  os <- summary(o <- glm(as.formula(paste("Outcome ~", expend.names[i], "*Race_ + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Race_*Five.Year.Age.Groups")), family=binomial, data=cbind(DF,log.1.1.Expenditures)))
  Kp <- regexpr(":Race_", names(o$coef)) > 0
  P.v <- 1 - pchisq(t(o$coef[Kp]) %*% solve(os$cov.unscaled[Kp,Kp]) %*% o$coef[Kp], df=3)
  print(P.v)
}


# glme
summary(o <- glmer(Outcome ~ log.1.1EducationPrimSec + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef
summary(o <- glmer(Outcome ~ log.1.1EducationPostSec + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef
summary(o <- glmer(Outcome ~ log.1.1Welfare + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef
summary(o <- glmer(Outcome ~ log.1.1HealthHospitals + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef
summary(o <- glmer(Outcome ~ log.1.1Police + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef
summary(o <- glmer(Outcome ~ log.1.1Highways + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef

# lm on adhusted mortality per state
summary(lm(OoERagg ~ EducationPrimSec, data=State.Measures, weight=StatePop))$coef[2,]
summary(lm(OoERagg ~ EducationPrimSec + Median2005, data=State.Measures, weight=StatePop))$coef[2,]


#########################################################
# Instrument
#########################################################

# First Stage associations
summary(lm(State.Measures$EducationPostSec ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$EducationPrimSec ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$Welfare ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$HealthHospitals ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$Highways ~ State.Measures$TB.2005, weight=Pop))$coef[2,]
summary(lm(State.Measures$Police ~ State.Measures$TB.2005, weight=Pop))$coef[2,]

expend.names <- names(StateExpenditures2005)[-1]
log.expend.names <- names(log.1.1.Expenditures)
Coef <- matrix(nrow=length(expend.names), ncol=4)
for (i in 1:length(expend.names)) {
	o.nm <- expend.names[i+1]
	cat("----------------------------------------\n", o.nm, "\n")
	print(summary(lm(State.Measures[, o.nm] ~ TB.2005, data=State.Measures, weight=Pop))$coef[-1,])
	log.1.1.Y <- log(State.Measures[, o.nm]) / log(1.1)
	o.lm <- lm(log.1.1.Y ~ TB.2005 + PCI.2003 + Median2005 + HSPerc + BachPerc, data=State.Measures, weight=Pop)
	print(summary(o.lm)$coef[-1,])
	Res <- merge(DF, cbind(State=State.Measures$State, Res=o.lm$residuals), by = "State")[,"Res"]
	Coef[i,] <- summary(glm(as.formula(paste("Outcome ~", log.expend.names[i], "+ Res + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male*Race_*Five.Year.Age.Groups")), family=binomial, data=cbind(DF,log.1.1.Expenditures)))$coef[2, ]
}
dimnames(Coef)[[1]] <- expend.names
Coef.Expend <- Coef
(IV.Ratio.Expend <- exp(Coef.Expend[,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2))))
write.table(IV.Ratio.Expend, "c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\Results\\Table IV.txt", sep="\t", row.names=TRUE)


#########################################################

#########################################################
# Allowing for random effect of state
summary(o <- glmer(Outcome ~ EducationPrimSec + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=DF))$coef[2,]
summary(o <- glmer(Outcome ~ EducationPostSec + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=DF))$coef[2,]
summary(o <- glmer(Outcome ~ Welfare + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=DF))$coef[2,]
summary(o <- glmer(Outcome ~ HealthHospitals + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=DF))$coef[2,]
summary(o <- glmer(Outcome ~ Police + cbind(PCI.2003, Median2005, HSPerc, BachPerc) + Male + Race_ + Five.Year.Age.Groups + (1 | State), family=binomial, data=DF))$coef[2,]
#########################################################




########################################################
# 
########################################################



########################################################
# End of expenditures
########################################################


##########################################
# Figures 
##########################################

# Manuscript Figure 1
tiff("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\plos one accepted documents\\Figure 1.tiff")
par(mfrow=c(1,1))
plot(State.Measures$TB.2005, OoERagg, cex=sqrt(StatePop/(2*10^6)),  log="y", xlab="State Tax Burden in 2005:\nPercentage of State Income Paid in State and Local Taxes", ylab="Mortality Ratio Adjusted for Sex, Age & Race")
text(State.Measures$TB.2005, OoERagg, StateAbbrev, cex=0.75, col=2)
dev.off()

# Manuscript Figure 2
tiff("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\plos one accepted documents\\Figure 2.tiff")
par(mfrow=c(2,2), mar=c(4.0,3.8,1.5,1.5))
plot(State.Measures$EducationPrimSec, OoERagg, cex=sqrt(StatePop/(2*10^6)),  log="xy", xlab="Primary/Secondary Education\n $1000 per capita", ylab="Adjusted Mortality Ratio")
text(State.Measures$EducationPrimSec, OoERagg, StateAbbrev, cex=0.75, col=2)
plot(State.Measures$HealthHospitals, OoERagg, cex=sqrt(StatePop/(2*10^6)),  log="xy", xlab="Health and Hospitals\n $1000 per capita", ylab="Adjusted Mortality Ratio")
text(State.Measures$HealthHospitals, OoERagg, StateAbbrev, cex=0.75, col=2)
plot(State.Measures$Welfare, OoERagg, cex=sqrt(StatePop/(2*10^6)),  log="xy", xlab="Welfare\n $1000 per capita", ylab="Adjusted Mortality Ratio")
text(State.Measures$Welfare, OoERagg, StateAbbrev, cex=0.75, col=2)
plot(State.Measures$Police, OoERagg, cex=sqrt(StatePop/(2*10^6)),  log="xy", xlab="Police\n $1000 per capita", ylab="Adjusted Mortality Ratio")
text(State.Measures$Police, OoERagg, StateAbbrev, cex=0.75, col=2)
dev.off()



plot(State.Measures$Q5StateIncomePerc, OoERagg, cex=sqrt(StatePop/(2*10^6)), main="Income Tax Rate for Highest Earning 20%")
text(State.Measures$Q5StateIncomePerc, OoERagg, StateAbbrev, cex=0.75, col=2)

plot(State.Measures$RateAt25, OoERagg, cex=sqrt(StatePop/(2*10^6)), main="By State Income Tax Rate for Those Under 25K")
text(State.Measures$RateAt25, OoERagg, StateAbbrev, cex=0.75, col=2)

plot(State.Measures$FoodExempt, OoERagg, cex=sqrt(StatePop/(2*10^6)), xlim=c(-0.5,1.7), axes=FALSE, main="Food Exempt from Sales Tax")
axis(2)
axis(1, c(0,1), c("Not Exempt","Exempt"))
text(State.Measures$FoodExempt, OoERagg, State.Measures$State, cex=0.5, col=2, adj=0)


M <- matrix(nrow=0, ncol=4)
for (rc in levels(DF$Race)) {
  est <- summary(glm(Outcome ~ TB.2005 + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Five.Year.Age.Groups, subset=DF$Race==rc, family=binomial, data=DF))$coef[2,]
  M <- rbind(M, est)
}
dimnames(M)[[1]] <- levels(DF$Race)
dimnames(M)[[2]] <- names(est)
M

M <- matrix(nrow=0, ncol=4)
for (sx in 0:1) {
  est <- summary(glm(Outcome ~ TB.2005 + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Race_*Five.Year.Age.Groups, subset=Male==sx, family=binomial, data=DF))$coef[2,]
  M <- rbind(M, est)
}
dimnames(M)[[1]] <- c("Female","Male")
dimnames(M)[[2]] <- names(est)
M

M <- matrix(nrow=0, ncol=4)
for (ag in levels(DF$Five.Year.Age.Groups)) {
  est <- summary(glm(Outcome ~ TB.2005 + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male * Race_, subset = DF$Five.Year.Age.Groups==ag, family=binomial, data=DF))$coef[2,]
  M <- rbind(M, est)
}
dimnames(M)[[1]] <- levels(DF$Five.Year.Age.Groups)
dimnames(M)[[2]] <- names(est)
M

# Food Exempt

M <- matrix(nrow=0, ncol=4)
for (rc in levels(DF$Race)) {
  est <- summary(glm(Outcome ~ FoodExempt + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male*Five.Year.Age.Groups, subset=DF$Race==rc, family=binomial, data=DF))$coef[2,]
  M <- rbind(M, est)
}
dimnames(M)[[1]] <- levels(DF$Race)
dimnames(M)[[2]] <- names(est)
M

M <- matrix(nrow=0, ncol=4)
for (sx in 0:1) {
  est <- summary(glm(Outcome ~ FoodExempt + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Race_*Five.Year.Age.Groups, subset=Male==sx, family=binomial, data=DF))$coef[2,]
  M <- rbind(M, est)
}
dimnames(M)[[1]] <- c("Female","Male")
dimnames(M)[[2]] <- names(est)
M

M <- matrix(nrow=0, ncol=4)
for (ag in levels(DF$Five.Year.Age.Groups)) {
  est <- summary(glm(Outcome ~ FoodExempt + cbind(PCI.2003, Median2005, HSPerc, BachPerc) * Male * Race_, subset = DF$Five.Year.Age.Groups==ag, family=binomial, data=DF))$coef[2,]
  M <- rbind(M, est)
}
dimnames(M)[[1]] <- levels(DF$Five.Year.Age.Groups)
dimnames(M)[[2]] <- names(est)
M







