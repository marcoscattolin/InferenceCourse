###Q1
mn <- 12; s <- 4; z <- qnorm(.05)
mu0 <- mn + z * s / sqrt(100)
mu0

###Q2
baseline <- c(140,138,150,148,135)
w2 <- c(132,135,151,146,130)
t.test(baseline,w2,var.equal=FALSE, paired=TRUE,alternative="two.sided")


##Q3
mu <- 1100
s <- 30
n <- 9
mu +c(-1,1)*qt(.975,n-1)*s/sqrt(n)


##Q4
choose(4,4)*.75^4*.25^0   #all choose coke

##Q5
ppois(10,lambda=(1/100)*1787)


##Q6
m1 <- -3
m2 <- 1
s1 <- 1.5
s2 <- 1.8
mixprob <- (9 - 1) / (9+9 - 2)
s <- sqrt(mixprob * s1 ^ 2  +  (1 - mixprob) * s2 ^ 2)
z <- (m1 - m2) / (s * sqrt(1 / 9 + 1 / 9))
2 * pt(-abs(z), df = 9+9 - 2)


##Q8
power.t.test(n=100,sd=.04,sig.level=.05,delta=.01,type="one.sample",alternative="one.sided")$power

##Q9
ceiling(power.t.test(power=.9,sd=.04,sig.level=.05,delta=.01,type="one.sample",alternative="one.sided")$n)

##q11
m1 <- 44; m2 <- 42.04
n1 <- n2 <- 288
s <- 12
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))

##q12
.05/10
