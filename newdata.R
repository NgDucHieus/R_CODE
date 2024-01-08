data<-read.csv("C:\\Users\\Admin\\Desktop\\SXTK\\Big_data\\diem_thi_thpt_2023.csv")
toan<- data$toan

toan <-toan[!is.na(toan)]
mean(toan)
hist(toan,breaks =50,freq = F)

sd(toan)^2


pnorm()




A00 <- data[!is.na(data$toan) & !is.na(data$ngu_van) & !is.na(data$gdcd),]

SUMA00<-(A00$toan+A00$ngu_van+A00$gdcd)
barplot(SUMA00)
hist(SUMA00,breaks = 100,freq = F)
plot(density(SUMA00),lwd = 2,col='red')
lines(density(SUMA00),lwd =2 ,col='red')

SUMA00
data$gdcd
A00

nn<-data$ma_ngoai_ngu =="N1"
nn

mean<-sum(SUMA00)/length(SUMA00)
mean(SUMA00)
var <-sum((SUMA00-mean)^2)/length(SUMA00)
var
var(SUMA00)

if ( length(SUMA00) %% 2 ==0){
  mean <-
}
hist(x1,probability = T,breaks = 1000)


dnorm()
pnorm() 

?rnorm()


toss_results <- sample(c(0, 1), 1e6, replace = TRUE)
hist(toss_results,probability = T)



H1_BCSE <- c(172,181,171,170,174,177,178,165,169,168,171,180,170,172,170,178,163,169,170,175,170,176,176)
H2_BJS <- c(160,175,157,153,175,160,155,163,173,165,156,155,162,162,163,168,162,163,157,159,158,173,158,163,167,168,165,169,149,163)
H <- c(H1_BCSE, H2_BJS)
mean(H1_BCSE)
sd(H1_BCSE)
p <- 2*pnorm(mean(H1_BCSE), mean = 170, sd = sd(H1_BCSE)/sqrt(length(H1_BCSE)), lower.tail = F)
p
aH = c()
for (k in 1:1e6)
{
  B <- sample(H, replace = TRUE)
  aH[k] <- mean(B)
}
sd(aH)
mean(aH)
hist(aH,breaks =100)


rnorm(10,mean = 1,sd =2)
?rnorm


# Fixing the seed for reproducibility of the result
set.seed(10)

# Drawing 10000 samples from a geometric distribution
size <- 10000
sample <- rgeom(size, prob = 0.5)
bins <- seq(0, 10, by = 1)

# Plotting the histogram
hist(sample, breaks = bins, col = "blue", border = "blue", main = "Geometric Distribution")

z = (9900-10000)/(120/sqrt(30))
alpha = 0.05
z.alpha = qnorm(alpha)
z.alpha

