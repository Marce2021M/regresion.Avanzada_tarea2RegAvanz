library(methods)

# Define the class
jagsModel <- setClass(
  "jagsModel",
  slots = list(
    data = "list",
    inits = "function",
    parms = "character",
    model.fileText = "character",
    n.iter = "numeric",
    n.chains = "numeric",
    n.burnin = "numeric",
    n.thin = "numeric",
    modelSim = "rjags",
    outModelSim = "list",
    graphModelSim = "array",
    summModel = "array" ,
    dicModel = "numeric"
  ),
  prototype = list(
    data = NULL,
    inits = NULL,
    parms = NULL,
    model.fileText = NULL,
    n.iter = NULL,
    n.chains = NULL,
    n.burnin = NULL,
    n.thin = NULL,
    modelSim = NULL,
    outModelSim = NULL,
    graphModelSim = NULL,
    summModel = NULL,
    dicModel = NULL
  )
)

# Define the constructor method
setMethod(
  "initialize",
  "jagsModel",
  function(.Object, data, inits, parms, model.fileText, n.iter=5000, n.chains=2, n.burnin=500, n.thin=1) {
    .Object@data <- data
    .Object@inits <- inits
    .Object@parms <- parms
    .Object@model.fileText <- model.fileText
    .Object@n.iter <- n.iter
    .Object@n.chains <- n.chains
    .Object@n.burnin <- n.burnin
    .Object@n.thin <- n.thin
    return(.Object)
  }
)


# Define the runJagsModel method

setGeneric("runJagsModel", function(.Object, ...) {
  standardGeneric("runJagsModel")
})

setMethod(
  f="runJagsModel",
  signature = "jagsModel",
  function(.Object) {
    require('R2jags')

    .Object@modelSim <- jags(model.file=textConnection(.Object@model.fileText) , data = .Object@data, parameters.to.sav=.Object@parms, inits = .Object@inits, n.chains = .Object@n.chains, n.burnin=.Object@n.burnin, n.thin=.Object@n.thin)
    
    .Object@outModelSim <- .Object@modelSim$BUGSoutput$sims.list
    .Object@graphModelSim <- .Object@modelSim$BUGSoutput$sims.array
    .Object@summModel <- .Object@modelSim$BUGSoutput$summary
    .Object@dicModel <- .Object@modelSim$BUGSoutput$DIC
    
    #print("Ha terminado de crear la cadena")
  }
)

# Define the jagsTraceplot method

setGeneric("jagsTraceplot", function(object, ...) {
  standardGeneric("jagsTraceplot")
})

setMethod(
  "jagsTraceplot",
  "jagsModel",
  function(object) {
    return(traceplot(object@modelSim))
  }
)

# Define the viewSpace method

setGeneric("viewSpace", function(object, ...) {
  standardGeneric("viewSpace")
})

setMethod(
  "viewSpace",
  "jagsModel",
  function(object) {
    z <- object@outModelSim$beta
    par(mfrow=c(1,1))
    plot(z)
  }
)

# Define the graphConvergence method

setGeneric("graphConvergence", function(object, ...) {
  standardGeneric("graphConvergence")
})

setMethod(
  "graphConvergence",
  "jagsModel",
  function(object, parama = 1) {
    z1 <- object@graphModelSim[,1,parama]
    z2 <- object@graphModelSim[,2,parama]
    par(mfrow=c(3,2))
    plot(z1,type="l",col="grey50")
    lines(z2,col="firebrick2")
    y1<-cumsum(z1)/(1:length(z1))
    y2<-cumsum(z2)/(1:length(z2))
    ymin<-min(y1,y2)
    ymax<-max(y1,y2)
    plot(y1,type="l",col="grey50",ylim=c(ymin,ymax))
    lines(y2,col="firebrick2",ylim=c(ymin,ymax))
    hist(z1,freq=FALSE,col="grey50")
    hist(z2,freq=FALSE,col="firebrick2")
    acf(z1)
    acf(z2)
  }
)
# Define the method to summary

setGeneric("summaryModel", function(object, ...) {
  standardGeneric("summaryModel")
})

setMethod(
    "summaryModel",
    "jagsModel",
    function(object){
      print(object@summModel)
    }
)
#Method to know DIC

setGeneric("dicModel", function(object, ...) {
  standardGeneric("dicModel")
})

setMethod(
    "dicModel",
    "jagsModel",
    function(object){
      print(object@dicModel)

    }
)



