
#Nonlinear-regression-analysisfor-summarizing-disease-progress

## Input the data that include the variables time,
#    plant ID, and severity
time <- c(seq(0,10),seq(0,10),seq(0,10))
plant <- c(rep(1,11),rep(2,11),rep(3,11))

## Severity represents the number of
## lesions on the leaf surface, standardized
## as a proportion of the maximum
severity <- c(
        42,51,59,64,76,93,106,125,149,171,199,
        40,49,58,72,84,103,122,138,162,187,209,
        41,49,57,71,89,112,146,174,218,250,288)/288
data1 <- data.frame(
        cbind(
                time,
                plant,
                severity
        )
)

## Plot severity versus time 
## to see the relationship between
## the two variables for each plant
plot(
        data1$time,
        data1$severity,
        xlab="Time",
        ylab="Severity",
        type="n"
)
text(
        data1$time,
        data1$severity,
        data1$plant
)
title(main="Graph of severity vs time")


getInitial(
        severity ~ SSlogis(time, alpha, xmid, scale),
        data = data1
)


## Using the initial parameters above,
## fit the data with a logistic curve.
para0.st <- c(
        alpha=2.212,
        beta=12.507/4.572, # beta in our model is xmid/scale
        gamma=1/4.572 # gamma (or r) is 1/scale
)

fit0 <- nls(
        severity~alpha/(1+exp(beta-gamma*time)),
        data1,
        start=para0.st,
        trace=T
)


## Plot to see how the model fits the data; plot the
## logistic curve on a scatter plot
plot(
        data1$time,
        data1$severity,
        type="n"
)

text(
        data1$time,
        data1$severity,
        data1$plant
)

title(main="Graph of severity vs time")

curve(
        2.21/(1+exp(2.74-0.22*x)),
        from=time[1],
        to=time[11],
        add=TRUE
)

#https://www.apsnet.org/edcenter/disimpactmngmnt/topc/EcologyAndEpidemiologyInR/DiseaseProgress/Pages/NonlinearRegression.aspx
