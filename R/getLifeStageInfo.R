#'
#'@title Get a list that defines life stage info for the DisMELS snow crab IBM.
#'
#'@description Function to get a list that defines life stage info for the DisMELS snow crab IBM.
#'
#'@param resType - results file type (i.e., 'NEW2.0SC')
#'
#'@return a list (see @link{@details})
#'
#'@details The returned list has the following elelments:
#' resType 
#' classInfo 
#' lifeStageType
#'
#'@import rDisMELS
#'@import tibble
#'
#'@export
#'
getLifeStageInfo<-function(resType='NEW2.0SC'){

    #get standard attributes dataframe
    dfrStdAtts <- rDisMELS::getStandardAttributes(resType);

    #java LHS class names
    classNames<-c('disMELS.IBMs.SnowCrab.Zooea.Zooea',
                  'disMELS.IBMs.SnowCrab.Megalopa.Megalopa',
                  'disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureMale',
                  'disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale');

    #information on 'additional attributes' for each life stage class
    ZooeaClassInfo<-rbind(tibble(short_name="molt indicator",   data_type="numeric",name="molt indicator"),
                          tibble(short_name="shellthickness",   data_type="numeric",name="shell thickness"),
                          tibble(short_name="temperature",      data_type="numeric",name="temperature"),
                          tibble(short_name="salinity",         data_type="numeric",name="salinity"),
                          tibble(short_name="pH",               data_type="numeric",name="pH"));
    ZooeaClassInfo  <-rbind(dfrStdAtts,ZooeaClassInfo);

    MegalopaClassInfo<-rbind(tibble(short_name="molt indicator",  data_type="numeric",name="molt indicator"),
                             tibble(short_name="shellthickness",  data_type="numeric",name="shell thickness"),
                             tibble(short_name="temperature",     data_type="numeric",name="temperature"),
                             tibble(short_name="salinity",        data_type="numeric",name="salinity"),
                             tibble(short_name="pH",              data_type="numeric",name="pH"));
    MegalopaClassInfo  <-rbind(dfrStdAtts,MegalopaClassInfo);


    ImmatureMaleClassInfo<-rbind(tibble(short_name="instar",         data_type="character",name="instar?"),
                                 tibble(short_name="ageInInstar",    data_type="numeric",  name="ageInInstar"),
                                 tibble(short_name="moltIndicator",  data_type="numeric",  name="moltindicator"),
                                 tibble(short_name="size",           data_type="numeric",  name="size (mm CW)"),
                                 tibble(short_name="weight",         data_type="numeric",  name="weight (g)"),
                                 tibble(short_name="shellcondition", data_type="numeric",  name="shell condition"),
                                 tibble(short_name="temperature",    data_type="numeric",name="temperature"),
                                 tibble(short_name="salinity",       data_type="numeric",name="salinity"),
                                 tibble(short_name="pH",             data_type="numeric",name="pH"));
    ImmatureMaleClassInfo  <-rbind(dfrStdAtts,ImmatureMaleClassInfo);

    ImmatureFemaleClassInfo <-ImmatureMaleClassInfo;

    #class info, by class
    classInfo<-list();
    classInfo[['disMELS.IBMs.SnowCrab.Zooea.Zooea']]                   <-list(info=ZooeaClassInfo,              typeNames=c("Z1"));
    classInfo[['disMELS.IBMs.SnowCrab.Zooea.Zooea']]                   <-list(info=ZooeaClassInfo,              typeNames=c("Z2"));
    classInfo[['disMELS.IBMs.SnowCrab.Megalopa.Megalopa']]             <-list(info=MegalopaClassInfo,           typeNames=c("M1"));
    classInfo[['disMELS.IBMs.SnowCrab.Megalopa.Megalopa']]             <-list(info=MegalopaClassInfo,           typeNames=c("M2"));
    classInfo[['disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureMale']]     <-list(info=ImmatureMaleClassInfo,       typeNames=c("C1M"));
    classInfo[['disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale']]   <-list(info=ImmatureFemaleClassInfo,     typeNames=c("C1F"));


    #map of defined life stage type names to class names
    lifeStageTypes<-rbind(tibble(typeName="Z1",  class='disMELS.IBMs.SnowCrab.Zooea.Zooea',                name="Zooea1",         nextType="Z1"),
                          tibble(typeName="Z2",  class='disMELS.IBMs.SnowCrab.Zooea.Zooea',                name="Zooea2",         nextType="M1"),
                          tibble(typeName="M1",  class='disMELS.IBMs.SnowCrab.Megalopa.Megalopa',          name="Megalopa1",      nextType="M2"),
                          tibble(typeName="M2",  class='disMELS.IBMs.SnowCrab.Megalopa.Megalopa',          name="Megalopa2",      nextType=c("C1M","C1F")),
                          tibble(typeName="C1M", class='disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureMale',  name="ImmatureMale",   nextType="C2M"),
                          tibble(typeName="C1F", class='disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale',name="ImmatureFemale", nextType="C2F"));

    return(invisible(list(resType=resType,classInfo=classInfo,lifeStageTypes=lifeStageTypes)));
}
