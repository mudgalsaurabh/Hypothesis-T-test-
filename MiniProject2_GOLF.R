#MINI PROJECT 2 FO

golf=read.csv(file.choose())
str(golf)
attach(golf)
summary(ï..Current)
## 2 smaple test 
t.test(ï..Current,New)  
## it shows that p-value is .188 but as it is 2 tail test then we need 
## to divide by 2 .188/2 = .095 ,which is greater then .05 
## hence we can't reject the null hypothesis hence we need more analysis 

# we will do 1 tail test for each sample 

t.test(ï..Current)##95 percent confidence interval that ball lies in :
                  ##267.4757 273.0743
t.test(New)  ##95 percent confidence interval that ball lies in :
            ##264.3348 270.6652

## yes we can say that there is slight diff in performane of new and current 
##ball 
## need to calculate power of test to come to conclusion 

delta= round(mean(ï..Current) - mean(New),3)
poolsd = ((40-1)*(8.75^2) +(40-1)*(9.92)/(40+40-2))^.05
power.t.test(n=40,delta=2.75,sd=9.3,sig.level = .05,type="two.sample",alternative="two.sided")

## power is .25 i.e that 25% of chance that null hypothesis can't be rejected when H0 is 
##false , hence we should revisit the # of sample to increease the power of test .

## if we replace n with power then we will get hte sample size for 95% power - 
power.t.test(power=.95,delta=2.75,sd=9.3,sig.level =.188,type="two.sample",alternative="two.sided")
