#Simulation of the Exponential Distribution and the Central Limit Theorem
##Author. Arturo L. S.

In this project we will investigate the exponential distribution in R and compare it with the Central Limit 
Theorem. The exponential distribution is simulated in R with rexp(n, lambda) where lambda is the rate 
parameter, the mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. 
lambda is set to be 0.2 for all of the simulations. An averages of 40 exponentials is investigated and 
to do so a 1000 simulations will be needed.

To illustrate the data simulated versus the real data, the following measures needs to be calculated:

Show the sample mean and compare it to the theoretical mean of the distribution.
Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
Show that the distribution is approximately normal.

```{r, echo=FALSE}
# load libraries
library(ggplot2)

# set constants
lambda <- 0.2 
n <- 40 
sims <- 1000 

# reproducability
set.seed(820)

# run the simulation test 
exp_dist <- matrix(rexp(n * sims, lambda), sims,n)
exp_dist_mean <- rowMeans(exp_dist)
head(exp_dist_mean)

# Draw Histogram
hist(exp_dist_mean, col = "orange")
```
![plot1](https://cloud.githubusercontent.com/assets/10600024/8270469/56afe8a6-17d0-11e5-9644-c65800381f2d.png)
##Results

The averange, the standar deviation and variance are calculated for both simulated and real data

##Simulated data

```{r, echo=FALSE}
#Averange
avng_sim <- mean(exp_dist_mean)

#Variance
var_sim <- sd(exp_dist_mean)

#Standar deviation
desvest_sim <- var(exp_dist_mean)

Real data
#Averange
mean_theory<-1/lambda

#Variance
var_theory<-((1/lambda)/sqrt(40))^2

#Standar deviation
desvest_theory<-(1/lambda)/sqrt(40)
```

##Comparison between the results
Mean

```{r, echo=FALSE}
avng_sim
mean_theory
```

The expected value of the theorical mean is 5, however the sample of 40 exponential mean of this distribution
converges to the population mean of the initial exponential distribution

Variance
```{r, echo=FALSE}
var_sim 
var_theory
```

The theoretical value for the variance of the distribution of averages is given by the variance of the
original population, divided by the number of samples n used to compute the averages. It can be noted
tha the values are close to one another


##Comparison with the Normal Distribution

```{r, echo=FALSE}
graph_data<-data.frame(exp_dist_mean)
ggplot(graph_data, aes(x = exp_dist_mean)) + 
geom_histogram(aes(y=..density..), fill = I('#00e6fa'), 
                   binwidth = 0.20, color = I('black')) +
geom_vline(xintercept=avng_sim, size = 1, color = 'green') +
geom_vline(xintercept=mean_theory, size = 1, color = 'red') +
stat_function(fun=dnorm,args=list( mean=avng_sim, sd=sqrt(var_sim)),color = "green", size = 1.0)+
stat_function(fun = dnorm, arg = list(mean = 5, sd = sqrt(var_theory)))

The histogram can be aproximated by the normal distribution and it is due to the central limit theorem,
the sample mean is the yellow dotted line, and the theorical mean is the green dotted line.

Variance and Confidence Intervals
Conf_Int_sim <- round (avng_sim + c(-1,1)*1.96*sd(exp_dist_mean)/sqrt(n),3)
Conf_Int_theory <- mean_theory + c(-1,1)*1.96*sqrt(var_theory)/sqrt(n);
```
![plot2](https://cloud.githubusercontent.com/assets/10600024/8270541/17230450-17d2-11e5-8787-9f7e4bdea6f1.png)


## 95% Confidence Interval simulated and real
```{r, echo=FALSE}
Conf_Int_sim
Conf_Int_theory
```

The 95% confidence intervals are very similar, for the simulated data 4.762,5.235 and the real ones 4.755,5.245


#Drawing the quantiles
```{r, echo=FALSE}
qqnorm(exp_dist_mean); qqline(exp_dist_mean)
```
![plot3](https://cloud.githubusercontent.com/assets/10600024/8270542/20dec7a4-17d2-11e5-982d-e5903ef986c8.png)


