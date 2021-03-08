getPackageData<-function(name,
                         xyType="LL"){
  if (tolower(name)=="land") {
    fn = paste0("extdata/Land.",xyType,".RData");
    return(wtsUtilities::getObj(system.file(fn,package="rIBMsSnowCrab")));
  }
}