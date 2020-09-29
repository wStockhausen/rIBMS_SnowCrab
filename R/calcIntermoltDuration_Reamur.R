#'
#' @title Calculate the intermolt duration using Reaumur's Law from heat summation theory
#'
#' @description Function to calculate the intermolt duration using Reaumur's Law from heat summation theory.
#'
#' @param a - the "thermal constant" (Yamamoto et al., 2014. This is K in Yamamoto et al., 2015.)
#' @param b - the "threshold temperature constant" (Yamamoto et al., 2014. This is \alpha in Yamamoto et al., 2015.)
#' @param T - temperature (either a single number or a time series)
#' @param dt - time step (same time units as a), if T is a time series
#'
#' @return The intermolt duration, in the time units of \code{a}.
#'
#' @details The intermolt duration using Reaumur's Law is D = a/(T-b) for development at constant T.
#' Note that Reamur's law => 1/D = (T-b)/a => S (1/D) dt = S (T-b)/a dt => 1 = S (T-b)/a dt, where S is the
#' time integral from 0 to D. For variable T, then, the intermolt duration is assumed to be
#' given by the time at which the integral S (T-b)/a dt = 1. Since a is constant, this condition is equivalent
#' to S (T-b) dt = a.
#'
#' @export
#'
calcIntermoltDuration<-function(a,b,T,dt=1){
  D <- NA;
  if (length(T)==1){
    D <- a/(T-b);
    if (D<=0) D<-NA;
  } else {
    #find time at which integral of (T-b) = a
    i<-0; t<-0; j<-1;
    while ((i < a)|(j>length(T))){
      i <- i + (T[j]-b)*dt;
      t <- t + dt;
    }
    if (i>=a) D <- t;
  }
  return(D);
}

#'
#' @title Calculate the intermolt duration using Belehradek equation
#'
#' @description Function to calculate the intermolt duration using Belehradek equation.
#'
#' @param a - the "thermal constant" (Ouellet and Ste. Marie, 2018)
#' @param b - the "threshold temperature constant" (Ouellet and Ste. Marie, 2018)
#' @param c - exponent for Belehradek equation (Ouellet and Ste. Marie, 2018)
#' @param T - temperature (either a single number or a time series)
#' @param dt - time step (same time units as a), if T is a time series
#'
#' @return The intermolt duration, in the time units of \code{a}.
#'
#' @details The intermolt duration using Belehradek equation is D = a/(T-b)^c for development at constant T.
#' Note that this => 1/D = [(T-b)^c]/a => S (1/D) dt = S [(T-b)^c]/a dt => 1 = S [(T-b)^c]/a dt, where S is the
#' time integral from 0 to D. For variable T, then, the intermolt duration is assumed to be
#' given by the time at which the integral S [(T-b)^c]/a dt = 1. Since a is constant, this condition is equivalent
#' to S [(T-b)^c] dt = a.
#'
#' @export
#'
calcIntermoltDuration_Belehradek<-function(a,b,c,T,dt=1){
  D <- NA;
  if (length(T)==1){
    D <- a*(T-b)^c;
    if (D<=0) D<-NA;
  } else {
    #find time at which integral of (T-b)^c = a
    i<-0; t<-0; j<-1;
    while ((i < a)|(j>length(T))){
      i <- i + ((T[j]-b)^c)*dt;
      t <- t + dt;
    }
    if (i>=a) D <- t;
  }
  return(D);
}

#'
#' @title Calculate the temperature corresponding to an intermolt duration using Reaumur's Law from heat summation theory
#'
#' @description Function to calculate the temperature corresponding to an intermolt duration using Reaumur's Law from heat summation theory.
#'
#' @param a - the "thermal constant" (Yamamoto et al., 2014. This is K in Yamamoto et al., 2015.)
#' @param b - the "threshold temperature constant" (Yamamoto et al., 2014. This is \alpha in Yamamoto et al., 2015.)
#' @param D - the intermolt duration (either a single number or a time series)
#' @param dt - time step (same time units as a), if T is a time series
#'
#' @return The intermolt duration, in the time units of \code{a}.
#'
#' @details The intermolt duration using Reaumur's Law is D = a/(T-b) for development at constant T, so
#' T = a/D+b for a given intermolt duration..
#'
#' @export
#'
calcTempForIMD<-function(a,b,D){
  T <- a/D+b;
  return(T);
}

#'
#' @title Get Reamur's Law constants for a snow crab life stage
#' 
#' @description A function to get Reamur's Law constants for a snow crab life stage.
#' 
#' @param stage - stage ("Z1","Z1-M","M","C1"-"C7") to retrieve constants for
#' 
#' @return A one-row tibble
#' 
#' @details Constants from Yamamoto et al. ().
#' 
#' @import tibble
#' 
#' @export
getReaumursLawConstants<-function(stage="Z1"){
  dfr<-rbind(
        tibble(stage="Z1",a= 229.5, se_a= 11.05,b= 0.63,se_b=0.26),
        tibble(stage="Z1-M",a= 530.5, se_a= 22.38,b=-0.02,se_b=0.26),
        tibble(stage="M", a= 417.3, se_a=  0.09,b=-2.24,se_b=0.09),
        tibble(stage="C1",a= 276.07,se_a= 15.93,b=-1.29,se_b=0.14),
        tibble(stage="C2",a= 429.06,se_a= 18.99,b=-2.54,se_b=0.19),
        tibble(stage="C3",a= 511.44,se_a= 33.06,b=-2.63,se_b=0.28),
        tibble(stage="C4",a= 762.62,se_a= 74.77,b=-3.68,se_b=0.58),
        tibble(stage="C5",a= 801.73,se_a= 98.33,b=-3.28,se_b=0.67),
        tibble(stage="C6",a= 996.53,se_a=133.14,b=-4.87,se_b=0.95),
        tibble(stage="C7",a=1115.32,se_a=177.68,b=-4.02,se_b=0.10)
       );
  return(dfr[dfr$stage==stage,]);
}

#'
#' @title Get Belehradek's Law constants for a snow crab life stage
#' 
#' @description A function to get Belehradek's Law constants for a snow crab life stage.
#' 
#' @param stage - stage ("Z1","Z2","M") to retrieve constants for
#' 
#' @return A one-row tibble
#' 
#' @details Values from Ouellet and Ste. Marie, 2018.
#' 
#' @import tibble
#' 
#' @export
getBelehradekConstants<-function(stage="Z1"){
  dfr<-rbind(
        tibble(stage="Z1",a=4903.33, b=-6.1 , c=1.67),
        tibble(stage="Z2",a=2461.31, b=-4.73 ,c=1.67),
        tibble(stage="M" ,a= 260.39 ,b=-1.37, c=0.82)
       );
  return(dfr[dfr$stage==stage,]);
}

#' 
#' 
calcIMforStage<-function(stage,T,dt=1,addVariability=NULL){
  cs<-getReaumursLawConstants(stage);
  a<-cs$a; b<-cs$b;
  if (!is.null(addVariability)){
    if (addVariability$a) a <- rnorm(1,mean=a,sd=cs$se_a);
    if (addVariability$b) b <- rnorm(1,mean=b,sd=cs$se_b);
  }

  D<-calcIntermoltDuration(a,b,T,dt);
  return(D);
}

calcTforStageAndIMD<-function(stage,D,addVariability=NULL){
  cs<-getReaumursLawConstants(stage);
  a<-cs$a; b<-cs$b;
  if (!is.null(addVariability)){
    if (addVariability$a) a <- rnorm(1,mean=a,sd=cs$se_a);
    if (addVariability$b) b <- rnorm(1,mean=b,sd=cs$se_b);
  }

  T<-calcTempForIMD(a,b,D);
  return(T);
}

testIMD<-function(Ts,
                 stages=c("Z1","Z2","M","C1","C2","C3","C4","C5","C6","C7"),
                 addVariability=NULL,
                 nreps=ifelse(is.null(addVariability),1,100),
                 lims=NULL){
  dfr<-NULL;
  for (stage in stages){
    for (T in Ts){
      D <- calcIMforStage(stage,T);
      dfr<-rbind(dfr,tibble(stage=stage,type="mean",T=T,D=D));
      if (!is.null(addVariability)){
        for (rep in 1:nreps){
          D <- calcIMforStage(stage,T,addVariability=addVariability);
          dfr<-rbind(dfr,tibble(stage=stage,type="var",T=T,D=D));
        }#--rep loop
      }#--
    }#--T loop
  }#--stage loop
  require(ggplot2);
  p <- ggplot(mapping=aes_string(x="T",y="D",colour="stage",shape="type"));
  p <- p + geom_point(data=dfr[dfr$type=="var",],position=position_jitter(0.1));
  p <- p + geom_line(data=dfr[dfr$type=="mean",])+ geom_point(data=dfr[dfr$type=="mean",]);
  if (!is.null(lims))
    p <- p + coord_cartesian(xlim=lims$x,ylim=lims$y);
  p <- p + labs(x="temperature",y="intermolt duration");
  print(p)

  return(list(dfr=dfr,p=p));
}

getTforIMD<-function(Ds,
                 stages=c("Z1","Z2","M","C1","C2","C3","C4","C5","C6","C7"),
                 addVariability=NULL,
                 nreps=ifelse(is.null(addVariability),1,100),
                 lims=NULL){
  dfr<-NULL;
  for (stage in stages){
    for (D in Ds){
      T <- calcTforStageAndIMD(stage,D);
      dfr<-rbind(dfr,tibble(stage=stage,type="mean",T=T,D=D));
      if (!is.null(addVariability)){
        for (rep in 1:nreps){
          T <- calcTforStageAndIMD(stage,D,addVariability=addVariability);
          dfr<-rbind(dfr,tibble(stage=stage,type="var",T=T,D=D));
        }#--rep loop
      }#--
    }#--D loop
  }#--stage loop
  require(ggplot2);
  p <- ggplot(mapping=aes_string(x="D",y="T",colour="stage",shape="type"));
  p <- p + geom_point(data=dfr[dfr$type=="var",],position=position_jitter(0.1));
  p <- p + geom_line(data=dfr[dfr$type=="mean",])+ geom_point(data=dfr[dfr$type=="mean",]);
  p <- p + xlim(c(0,NA));
  if (!is.null(lims))
    p <- p + coord_cartesian(xlim=lims$x,ylim=lims$y);
  p <- p + labs(x="intermolt duration",y="temperature");
  print(p)

  return(list(dfr=dfr,p=p));
}

#--the following won't run when file is sourced
if (FALSE){
  res<-testIMD(seq(from=1,to=10,by=0.25),
              stages=c("Z1","Z2","M"),
              lims=list(x=c(0,10),y=c(0,200)));
  res<-testIMD(1:10,
              stages=c("Z1","Z2","M"),
              addVariability=list(a=TRUE,b=TRUE),
              nreps=100,
              lims=list(x=c(0,10),y=c(0,100)));
  res<-getTforIMD(c(20:100),
              stages=c("Z1","Z2","M"),
              lims=list(x=c(20,100),y=c(0,15)));
  res<-getTforIMD(c(20:100),
              stages=c("Z1","Z2","M"),
              addVariability=list(a=TRUE,b=TRUE),
              nreps=100,
              lims=list(x=c(20,100),y=c(0,15)));
  res<-testIMD(1:8,
              stages=c("C1","C2","C3","C4","C5","C6","C7"),
              lims=list(x=c(0,10),y=c(0,300)));
  res<-testIMD(1:8,
              stages=c("C1","C2","C3","C4","C5","C6","C7"),
              addVariability=list(a=TRUE,b=TRUE),
              nreps=20,
              lims=list(x=c(0,10),y=c(0,300)));
}
