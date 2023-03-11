library(R6)

jagsModel <- R6Class("jagsModel",
  public = list(
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
    dicModel = NULL,
    
    initialize = function(data, inits, parms, model.fileText, n.iter=5000, n.chains=2, n.burnin=500, n.thin=1) {
      self$data <- data
      self$inits <- inits
      self$parms <- parms
      self$model.fileText <- model.fileText
      self$n.iter <- n.iter
      self$n.chains <- n.chains
      self$n.burnin <- n.burnin
      self$n.thin <- n.thin
      self$modelSim <- NULL
      self$outModelSim <- NULL
      self$graphModelSim <- NULL
      self$summModel <- NULL
      self$dicModel <- NULL
    },
    
    runJagsModel = function() {
      require(R2jags)
      self$modelSim <- jags(model.file = textConnection(self$model.fileText),
      data = self$data,
      parameters.to.sav = self$parms,
      inits = self$inits,
      n.iter=self$n.iter,
      n.chains = self$n.chains,
      n.burnin = self$n.burnin,
      n.thin = self$n.thin)
      
      self$outModelSim <- self$modelSim$BUGSoutput$sims.list
      self$graphModelSim <- self$modelSim$BUGSoutput$sims.array
      self$summModel <- self$modelSim$BUGSoutput$summary
      self$dicModel <- self$modelSim$BUGSoutput$DIC
      
    },
    
    jagsTraceplot = function() {
      return(traceplot(self$modelSim))
    },
    
    viewSpace = function() {
      z <- self$outModelSim$beta
      par(mfrow=c(1,1))
      plot(z)
    },
    
    graphConvergence = function(parama = 1) {
      z1 <- self$graphModelSim[,1,parama]
      z2 <- self$graphModelSim[,2,parama]
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
    },
    
    summaryModel = function() {
      print(self$summModel)
    },
    
    dicModelfunc = function() {
      print(self$dicModel)
    }
  )
) 

canvaMortalidad <- function(){
  #mortality<-read.table("http://gente.itam.mx/lnieto/index_archivos/mortality.txt",header=TRUE)
  out.yf1<-ej5a.sim$summModel[grep("yf1",rownames(ej5a.sim$summModel)),]  #especial
  or<-order(mortality$x)
  ymin<-min(mortality$y,out.yf1[,c(1,3,7)])
  ymax<-max(mortality$y,out.yf1[,c(1,3,7)])

  par(mfrow=c(1,1))
  plot(mortality$x,mortality$y,ylim=c(ymin,ymax))

}

mortalidadGraf <- function(modelo, col=1){
  or<-order(mortality$x)
  out.yf1<-modelo$summModel[grep("yf1",rownames(modelo$summModel)),]  #especial

  lines(mortality$x[or],out.yf1[or,1],lwd=2,col=col)
  lines(mortality$x[or],out.yf1[or,3],lty=2,col=col)
  lines(mortality$x[or],out.yf1[or,7],lty=2,col=col)
}
