Keith McNulty’s Chapter 3, Statistics Foundations
================

Link to Keith’s Book
<https://peopleanalytics-regression-book.org/found-stats.html>

Download and set up data

``` r
url<-"http://peopleanalytics-regression-book.org/data/salespeople.csv"
salespeople<-read.csv(url)
```

CLT and Sampling

``` r
set.seed(123)
custrate<-na.omit(salespeople$customer_rate)
n<-100
sample_custrate=sample(custrate,n)
```

Because we have taken 100 samples of the data, we have a sampling
distribution, meaning we must calculate sample mean and variability in
standard error. The standard error is the sample standard deviation
divide by the square root of the sample size.

``` r
sample_mean<-mean(sample_custrate)
sample_mean
```

    ## [1] 3.6485

``` r
se<-(sd(sample_custrate/sqrt(n)))
se
```

    ## [1] 0.08494328

Now that we have the standard error we can determine our critical values
for a 95% confidence interval by looking at the outer 2.5% of the
distribution. This gives us 95% “confidence” that the population mean is
contained in our CI. We use the t-distribution here becuase we are
approximating the normal distribution, but are doing so with a sample
estimate of the mean.

We first need to calculate the critical values for a=0.05 using the
t-distribution that matches with our sample size. To do this we need to
find the degrees of freedom of our distribution which is n-1 or 100-1.
Then we can see that we are looking at the outer 2.5% of the
distributions as we are using a “two-tailed” distribution.

``` r
t<-qt(p=0.975, df=n-1)
t
```

    ## [1] 1.984217

1.98 standard errors on either side will give us 95% CI. We take the
mean and add or subtract the product of the t-value and the standard
error to produce our confidence interval.

``` r
lower_bound<-sample_mean-t*se
upper_bound<-sample_mean+t*se
cat(paste0('[',lower_bound,',',upper_bound,']'))
```

    ## [3.47995410046738,3.81704589953262]

T-test basics-the t-test lets us compare means between two groups,
looking for significant differences. In the book’s example, the
comparison is between those with high and low sales performance ratings.
To do this we are creating two new samples of our data, based on the
performance metric of the salesperson.

``` r
perf1<-subset(salespeople, subset =performance ==1)
mean(perf1$sales)
```

    ## [1] 464.9167

``` r
perf4<-subset(salespeople,subset=performance==4)
mean(perf4$sales)
```

    ## [1] 619.8909

``` r
(diff<-mean(perf4$sales)-mean(perf1$sales))
```

    ## [1] 154.9742

Here we can see the difference between the means of the highest
performing group (4) and the lowest performing group (1). The question
now is whether or not these means are *significantly* different from one
another, that is, the probability of (not) finding these sample
statistics due to chance alone. To start this NHST process, we need to
calculate a CI around these means which allows us to determine whether
or not 0 is present.

First we calculate the standard error to help us find the critical
t-value.

``` r
se<-sqrt(sd(perf1$sales)^2/length(perf1$sales) 
            +sd(perf4$sales)^2/length(perf4$sales))
se
```

    ## [1] 33.47554

We then calculate the t-value using the grouped degrees of freedom using
the [Welch-Satterthwaite
approximation](https://statisticaloddsandends.wordpress.com/2020/07/03/welchs-t-test-and-the-welch-satterthwaite-equation/).
We use this approximation because we are using Welch’s t-test opposed to
student’s t-test. Typically this is done because student’s t assumes
that the populations have equal variances while Welch’s does not.

![t=\\frac{\\bar{X\_{1}}-\\bar{X\_{2}}}{\\sqrt{\\frac{s\_{1}^2}{n\_{1}}}+{\\frac{s\_{2}^2}{n\_{2}}}}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t%3D%5Cfrac%7B%5Cbar%7BX_%7B1%7D%7D-%5Cbar%7BX_%7B2%7D%7D%7D%7B%5Csqrt%7B%5Cfrac%7Bs_%7B1%7D%5E2%7D%7Bn_%7B1%7D%7D%7D%2B%7B%5Cfrac%7Bs_%7B2%7D%5E2%7D%7Bn_%7B2%7D%7D%7D%7D "t=\frac{\bar{X_{1}}-\bar{X_{2}}}{\sqrt{\frac{s_{1}^2}{n_{1}}}+{\frac{s_{2}^2}{n_{2}}}}")

``` r
t<-qt(p=0.975, df=100.98)
t
```

    ## [1] 1.983736

``` r
(upper_bound<-diff+t*se)
```

    ## [1] 221.3809

``` r
(lower_bound<-diff-t*se)
```

    ## [1] 88.56763

Now that we have the upper and lower bound estimates of our 95% CI, we
can test to see if 0 is present in the confidence interval. If 0 is
within our confidence interval, there is a chance that there is 0
difference between our samples which would cause us to retain the null
hypothesis. If 0 is not present, then we can say that with 95% certainty
the difference between the populations is statistically significantly
different from 0, i.e., there is a real difference between sales of top
and bottom performers.

``` r
(0<=upper_bound)&(0>=lower_bound)
```

    ## [1] FALSE

Alternatively, we can calculate the t-statistic manually by dividing the
difference by the standard error. Then doubling it to find the
two-tailed p-value.

``` r
t_statistic<-diff/se
t_statistic
```

    ## [1] 4.629478

``` r
2*pt(t_statistic, df=100.98, lower=F)
```

    ## [1] 1.093212e-05

lower=F here because we want to know the probability of seeing the score
occur using the right hand of the distribution.

Last way to do this, a simple function! The t-test function:

``` r
t.test(perf4$sales, perf1$sales)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  perf4$sales and perf1$sales
    ## t = 4.6295, df = 100.98, p-value = 1.093e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   88.5676 221.3809
    ## sample estimates:
    ## mean of x mean of y 
    ##  619.8909  464.9167
