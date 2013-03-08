
loansData <- read.csv("C:\\Users\\DanDye\\Google Drive\\DataAnalysis\\loansData_clean.csv")


mytree <- rpart(loansData$Interest.Rate ~ 
      loansData$Home.Ownership + 
      loansData$Revolving.CREDIT.Balance + 
      loansData$Inquiries.in.the.Last.6.Months +
      loansData$FICO.Range +
      loansData$Debt.To.Income.Ratio +
      loansData$Employment.Length +
      loansData$Loan.Purpose +
      as.factor(loansData$Loan.Length)+
      loansData$Amount.Requested +
      loansData$State +
      loansData$Amount.Funded.By.Investors +
      loansData$Monthly.Income+Open.CREDIT.Lines
      , data=loansData)



lm1 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range + 
            as.factor(loansData$Loan.Length) + loansData$Amount.Requested)

lm1.res <- residuals(lm1)

par( mfrow = c( 1, 2 ) )
plot(loansData$Interest.Rate, loansData$FICO.Range, 
     main="Interest vs. FICO\nred=36mo\nblue=60mo",
     xlab="Interest Rate %", 
     ylab="FICO Score",
     pch=19,
     cex=.5,
     col=c("red","blue")[as.factor(loansData$Loan.Length)],
     legend.plot=T
     )
     

plot(loansData$Interest.Rate, lm1.res, 
     ylab="Residuals", xlab="Interest Rate %", 
     main="Residuals\nlm(Rate ~ FICO Score + 
            Loan Lngth + Amt. Request)",
     pch=19,
     cex=.5
     ) 

