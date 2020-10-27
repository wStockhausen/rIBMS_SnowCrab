#'
#' @title Get the constants for the Belehradek equation by life stage and source
#'
#' @description Function to get the constants for the Belehradek equation by life stage and source.
#' 
#' @param sources - "all", "Ouellet and Ste Marie 2018", or "Yamamoto et al. 2014"
#' @param stages - "all" or character vector with required life stages
#' 
#' @return tibble with columns 'source', 'stage', 'a', 'se_a', 'b', 'se_b', and 'c'.
#' 
#' @details Note that Reamur's equation is equivalent to the Belehradek equation with
#' the constant \eqn{c = 1}.
#' 
#' @importFrom tibble tibble
#' 
#' @export
#'
getBelehradekConstants<-function(sources="all",
                                 stages="all"){
  dfr<-rbind(
        tibble(source="Yamamoto et al. 2014",      stage="Z1-M", a= 530.5, se_a= 22.38,b=-0.02,se_b=0.26, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="Z1",a= 229.5, se_a= 11.05,b= 0.63,se_b=0.26, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="M", a= 417.3, se_a=  0.09,b=-2.24,se_b=0.09, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="C1",a= 276.07,se_a= 15.93,b=-1.29,se_b=0.14, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="C2",a= 429.06,se_a= 18.99,b=-2.54,se_b=0.19, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="C3",a= 511.44,se_a= 33.06,b=-2.63,se_b=0.28, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="C4",a= 762.62,se_a= 74.77,b=-3.68,se_b=0.58, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="C5",a= 801.73,se_a= 98.33,b=-3.28,se_b=0.67, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="C6",a= 996.53,se_a=133.14,b=-4.87,se_b=0.95, c=1.0),
        tibble(source="Yamamoto et al. 2014",      stage="C7",a=1115.32,se_a=177.68,b=-4.02,se_b=0.10, c=1.0),
        tibble(source="Ouellet and Ste Marie 2018",stage="Z1",a=4903.33,se_a=0, b=-6.1 ,se_b=0, c=1.87),
        tibble(source="Ouellet and Ste Marie 2018",stage="Z2",a=2461.31,se_a=0, b=-4.73,se_b=0 ,c=1.67),
        tibble(source="Ouellet and Ste Marie 2018",stage="M" ,a= 260.39,se_a=0 ,b=-1.37,se_b=0, c=0.82)
       );
  if (sources=="all") sources<-unique(dfr$source);
  if (stages=="all") stages<-unique(dfr$stage);
  idx<-(dfr$source %in% sources)&(dfr$stage %in% stages);
  if (any(idx)) return(dfr[idx,]);
  return(NULL);
}

#'
#' @title Calculate the intermolt duration using the Belehradek equation
#'
#' @description Function to calculate the intermolt duration using the Belehradek equation.
#'
#' @param a - the "thermal constant" (Ouellet and Ste. Marie, 2018)
#' @param b - the "threshold temperature constant" (Ouellet and Ste. Marie, 2018)
#' @param c - exponent for Belehradek equation (negative of Ouellet and Ste. Marie, 2018 value)
#' @param T - temperature (either a single number or a time series)
#' @param dt - time step (same time units as a), if T is a time series
#'
#' @return The intermolt duration, in the time units of \code{a}.
#'
#' @details The intermolt duration using Belehradek equation is \eqn{D = a/(T-b)^c} for development at constant T.
#' Note that this => \eqn{1/D = [(T-b)^c]/a} => \eqn{S (1/D) dt = S [(T-b)^c]/a dt} => \eqn{1 = S [(T-b)^c]/a dt}, where S is the
#' time integral from 0 to D. For variable T, then, the intermolt duration is assumed to be
#' given by the time at which the integral \eqn{S [(T-b)^c]/a dt = 1}. Since a is constant, this condition is equivalent
#' to \eqn{S [(T-b)^c] dt = a}. Note that Reamur's Law can be obtained by setting the exponent "c" to 1.
#'
#' @export
#'
calcIntermoltDuration_Belehradek<-function(a,b,c,T,dt=1){
  D <- NA;
  if (length(T)==1){
    D <- a/(T-b)^c;
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
#' @title Calculate the intermolt duration using Belehradek equation
#'
#' @description Function to calculate the intermolt duration using Belehradek equation for given
#' stages based on given sources.
#'
#' @param sources - "all", "Ouellet and Ste Marie 2018", or "Yamamoto et al. 2014"
#' @param stages - "all" or character vector with required life stages
#' @param T - temperature (either a single number or a time series)
#' @param dt - time step (same time units as a), if T is a time series
#'
#' @return A tibble with columns 'source', 'stage', 'T', and 'D'.
#'
#' @details The intermolt duration using Belehradek equation is \eqn{D = a/(T-b)^c} for development at constant T.
#' Note that this => \eqn{1/D = [(T-b)^c]/a} => \eqn{S (1/D) dt = S [(T-b)^c]/a dt} => \eqn{1 = S [(T-b)^c]/a dt}, where S is the
#' time integral from 0 to D. For variable T, then, the intermolt duration is assumed to be
#' given by the time at which the integral \eqn{S [(T-b)^c]/a dt = 1}. Since a is constant, this condition is equivalent
#' to \eqn{S [(T-b)^c] dt = a}. Note that Reamur's Law can be obtained by setting the exponent "c" to 1.
#'
#'@importFrom tibble tibble
#'
#' @export
#'
calcIMDforStage_Belehradek<-function(sources="all",
                                     stages="all",
                                     T,
                                     dt=1){
  dfr<-NULL;
  cs<-getBelehradekConstants();#all sources, stages
  if (sources=="all") sources<-unique(cs$source);
  if (stages=="all") stages  <-unique(cs$stages);
  for (source in sources){
    for (stage in stages){
      cs<-getBelehradekConstants(source,stage);
      if (!is.null(cs)){
        a<-cs$a; b<-cs$b; c<-cs$c;
        D<-calcIntermoltDuration_Belehradek(a,b,c,T,dt);
        dfr<-rbind(dfr,tibble(source=source,stage=stage,T=T,D=D));
      }
    }
  }
  return(dfr);
}

#'
#' @title Calculate the IMD by stage for fixed T
#'
#' @description Function to calculate the IMD by stage for fixed T.
#'
#' @param T - vector of (fixed) temperatures
#' @param stages - character vector of stages to calculate IMD for
#' @param source - character value with source for constants
#'
#' @return a tibble with stage, temperature, and intermolt duration, in the time units of \code{a}.
#'
#' @details Temperatures are regarded as fixed, so this function is appropriate to
#' use if T represents temperatures spatially, for example.
#' See also \code{\link{calcIntermoltDuration_Belehradek}}.
#'
#' @importFrom tibble tibble
#' 
#' @export
#'
calcIMDbyStage_FixedT<-function(T,
                                 stages=c("Z1","Z2","M"),
                                 source="Ouellet and Ste Marie 2018"){
  dfr<-NULL;
  for (stage in stages){
    cs<-getBelehradekConstants(sources=source,
                               stages=stages);
    D <- cs$a/(T-cs$b)^cs$c;
    idx<-D<0; D[idx]<-NA;
    dfr<-rbind(dfr,tibble(stage=stage,T=T,D=D,stringsAsFactors=FALSE));
  }
  return(dfr);
}

#'
#' @title Calculate the temperature corresponding to an intermolt duration using Belehradek's function
#'
#' @description Function to calculate the temperature corresponding to an intermolt duration using Belehradek's function.
#'
#' @param a - the "thermal constant" (Ouellet and Ste. Marie, 2018)
#' @param b - the "threshold temperature constant" (Ouellet and Ste. Marie, 2018)
#' @param c - exponent for Belehradek equation (negative of Ouellet and Ste. Marie, 2018 value)
#' @param D - the intermolt duration (either a single number or a time series)
#'
#' @return The intermolt duration, in the time units of \code{a}.
#'
#' @details The intermolt duration using Belehradek's function is \eqn{D = a/(T-b)^c} for development at constant T, so
#' \eqn{T = (a/D)^(1/c)+b} for a given intermolt duration..
#'
#' @export
#'
calcTempForIMD_Belehradek<-function(a,b,c,D){
  T <- (a/D)^(1/c)+b;
  return(T);
}

#'
#' @title Calculate the temperature corresponding to an intermolt duration using Belehradek's function
#'
#' @description Function to calculate the temperature corresponding to an intermolt duration 
#' using Belehradek's function for given stages and sources.
#'
#' @param sources - character value with source for constants
#' @param stages - character vector of stages to calculate IMD for
#' @param D - the intermolt duration (either a single number or a time series)
#'
#' @return A tibble, with coloumns 'source', 'stage','T', and 'D'.
#'
#' @details The intermolt duration using Belehradek's function is \eqn{D = a/(T-b)^c} 
#' for development at constant T, so
#' \eqn{T = (a/D)^(1/c)+b} for a given intermolt duration.
#'
#' @export
#'
calcTforStageAndIMD_Belehradek<-function(sources="all",
                                         stages="all",
                                         D){
  dfr<-NULL;
  cs<-getBelehradekConstants();#all sources, stages
  if (sources=="all") sources<-unique(cs$source);
  if (stages=="all") stages  <-unique(cs$stages);
  for (source in sources){
    for (stage in stages){
      cs<-getBelehradekConstants(source,stage);
      if (!is.null(cs)){
        a<-cs$a; b<-cs$b; c<-cs$c
        T<-calcTempForIMD_Belehradek(a,b,c,D);
        dfr<-rbind(dfr,tibble(source=source,stage=stage,T=T,D=D));
      }
    }
  }
  return(dfr);
}

#'
#' @title Calculate and plot the IMD against T by stage for a range of fixed Ts
#'
#' @description Function to calculate and plot the IMD against T by stage for a range of fixed Ts.
#'
#' @param Ts - vector of (fixed) temperatures at which t
#' @param sources - character vector with sources for constants
#' @param stages - character vector of stages to calculate IMD for
#' @param lims - list with elements 'x' and 'y' with axis limits
#' @param showPlot - flag (T/F) to immediately show the plot
#'
#' @return list, with a tibble (stage, temperature, and intermolt duration, in the time units of \code{a})
#' and a \pkg{ggplot2} object.
#'
#' @details Temperatures are regarded as fixed, so this function is appropriate to
#' use if T represents temperatures spatially, for example.
#' See also \code{\link{calcIMDforStage_Belehradekk}}.
#'
#'@import ggplot2
#'@importFrom tibble tibble
#'
#' @export
#'
plotIMDbyT_Belehradek<-function(Ts,
                                sources="all",
                                stages=c("Z1","Z2","M"),
                                lims=NULL,
                                showPlot=FALSE){
  dfr<-NULL;
  for (T in Ts){
    dfrp<-calcIMDforStage_Belehradek(sources,stages,T);
    dfr<-rbind(dfr,dfrp);
  }#--T loop
  dfr$stage<-factor(dfr$stage,levels=stages)
  p <- ggplot(data=dfr,mapping=aes_string(x="T",y="D",colour="stage",linetype="source",shape="source"));
  p <- p + geom_line()+ geom_point();
  if (!is.null(lims))
    p <- p + coord_cartesian(xlim=lims$x,ylim=lims$y);
  p <- p + labs(x="temperature (deg C)",y="intermolt duration (days)");
  if (showPlot) print(p);

  return(list(dfr=dfr,p=p));
}

#'
#' @title Calculate and plot the necessary T against IMD by stage for a range of IMDs
#'
#' @description Function to calculate and plot the necessary T against IMD by stage for a range of IMDs.
#'
#' @param Ds - vector of IMDs
#' @param sources - character vector with sources for constants
#' @param stages - character vector of stages to calculate IMD for
#' @param lims - list with elemets 'x' and 'y' with axis limits
#' @param showPlot - flag (T/F) to immediately show the plot
#'
#' @return list, with a tibble (stage, temperature, and intermolt duration, in the time units of \code{a})
#' and a \pkg{ggplot2} object.
#'
#' @details Temperatures are regarded as fixed to determine the T corresponding to a
#' given IMD. See also \code{\link{calcTforStageAndIMD_Belehradek}}.
#'
#'@import ggplot2
#'@importFrom tibble tibble
#'
#' @export
#'
getTforIMD_Belehradek<-function(Ds,
                                sources="all",
                                stages=c("Z1","Z2","M"),
                                lims=NULL,
                                showPlot=FALSE){
  dfr<-NULL;
  for (D in Ds){
    dfrp <- calcTforStageAndIMD_Belehradek(sources,stages,D);
    dfr<-rbind(dfr,dfrp);
  }#--D loop
  dfr$stage<-factor(dfr$stage,levels=stages)
  p <- ggplot(data=dfr,mapping=aes_string(x="D",y="T",colour="stage",linetype="source",shape="source"));
  p <- p + geom_line()+ geom_point();
  p <- p + xlim(c(0,NA));
  if (!is.null(lims))
    p <- p + coord_cartesian(xlim=lims$x,ylim=lims$y);
  p <- p + labs(x="intermolt duration (days)",y="temperature (deg C)");
  if (showPlot) print(p)

  return(list(dfr=dfr,p=p));
}

# #--the following won't run when file is sourced
# if (FALSE){
#   res<-testIMD_Belehradek(seq(from=1,to=10,by=0.25),
#               stages=c("Z1","Z2","M"),
#               lims=list(x=c(0,10),y=c(0,200)));
#   res<-getTforIMD_Belehradek(c(20:100),
#               stages=c("Z1","Z2","M"),
#               lims=list(x=c(20,100),y=c(0,15)));
# }
