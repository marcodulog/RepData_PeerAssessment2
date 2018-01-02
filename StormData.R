require(sqldf)
require(lubridate)
setwd("c:/Users/Marco.Dulog/git/RepData_PeerAssessment2")

#don't load the result set if it is already there... takes too long
if (!exists("ds")){ 
  ds <- read.csv(file=bzfile('StormData.csv.bz2'),strip.white = TRUE)  
}

evtypeUnique<-sqldf("SELECT evtype as eventType, count(1) as Counter FROM ds GROUP BY evtype")

evtypeUnique$CatGroup <- 
  ifelse(grepl("TSTM|THUNDER|LIGHT|LIGN|STORM", evtypeUnique$eventType, ignore.case = TRUE),"STORM",
         ifelse(grepl("HAIL", evtypeUnique$eventType,ignore.case = TRUE),'HAIL',
                ifelse(grepl("HIGH WATER|FLOOD|FLOOOD|FLD", evtypeUnique$eventType,ignore.case = TRUE),'FLOOD',
                       ifelse(grepl("TYPHOON|TSUN|HURRICANE|TROPICAL DEPRESSION", evtypeUnique$eventType,ignore.case = TRUE),'HURRICANE',       
                              ifelse(grepl("TORNAD|TORND|FUNNEL|TNADO|BURST", evtypeUnique$eventType,ignore.case = TRUE),'TORNADO',       
                                     ifelse(grepl("RAIN|SHOWER|PRECIP|WET WE", evtypeUnique$eventType,ignore.case = TRUE),'RAIN',       
                                            ifelse(grepl("GLAZE|RECORD COLD|FREEZ|FROST|LOW TEMP|COLD|BLIZ|RECORD LOW|ICE|WINT|ICY|ICE|SNOW|SLEET", evtypeUnique$eventType,ignore.case = TRUE),'WINTER EVENTS',                
                                                   ifelse(grepl("Temperature record|WARM|HEAT|RECORD HIGH|HIGH TEMP|WARM TEMP|RECORD TEMP|HOT|DROUGHT|DRY|WARMTH", evtypeUnique$eventType,ignore.case = TRUE),'HEAT EVENTS',                
                                                          ifelse(grepl("WIND|DUST DEV", evtypeUnique$eventType,ignore.case = TRUE),'WIND',                
                                                                 ifelse(grepl("FIRE", evtypeUnique$eventType,ignore.case = TRUE),'FIRE',                
                                                                        ifelse(grepl("FOG", evtypeUnique$eventType,ignore.case = TRUE),'FOG', 
                                                                               ifelse(grepl("AVALA", evtypeUnique$eventType,ignore.case = TRUE),'AVALANCHE',
                                                                                      ifelse(grepl("MARINE|SWELL|SPOUT|TIDE|CURRENT|SURF|SEAS|COAST|WAVE", evtypeUnique$eventType,ignore.case = TRUE),'MARINE EVENTS',                
                                                                                             ifelse(grepl("LAND|BEACH|MUD", evtypeUnique$eventType,ignore.case = TRUE),'ERROSION',                
                                                                                                    "OTHER")
                                                                                      )))))))))))))



#
# Property damage: making a weighted (although subjective)
# assumption that injuries are 1/3 harmful than death to facilitate ranking
#
dsPopDmg<-sqldf("
  SELECT upper(evtype) evtype
  , SUM(fatalities) as fatalities
  , SUM(injuries) as injuries 
  , SUM((injuries * .3) + fatalities) as wgtdScore
  FROM ds 
  WHERE (IfNull(injuries,0) > 0 or IfNull(fatalities,0) > 0) 
  GROUP BY upper(evtype)")

#
# Crop damage: created a score value
# assumption, since crop damage and property damages
# are measured in dollars, this is totaled" 
#
dsPropDmg<-sqldf("
    SELECT evtype
    , (propDmgAmt * propdmgexp) as propertyDamage
    , (cropDmgAmt * cropDmgExp) as cropDamage
    FROM(
      SELECT upper(evtype) as evtype
      , IfNull(propdmg,0) as propDmgAmt
      , CASE 
          WHEN lower(propdmgexp) = '1' THEN 10.00
          WHEN lower(propdmgexp) = '2' THEN 100.00
          WHEN lower(propdmgexp) = '3' THEN 1000.00
          WHEN lower(propdmgexp) = '4' THEN 10000.00
          WHEN lower(propdmgexp) = '5' THEN 100000.00
          WHEN lower(propdmgexp) = '6' THEN 1000000.00
          WHEN lower(propdmgexp) = '7' THEN 10000000.00
          WHEN lower(propdmgexp) = 'h' then 100.00
          WHEN lower(propdmgexp) = 'k' then 1000.00
          WHEN lower(propdmgexp) = 'm' then 1000000.00
          WHEN lower(propdmgexp) = 'b' then 1000000000.00
        ELSE 0
        END as propdmgexp
      , IfNull(cropdmg,0) as cropDmgAmt
      , CASE 
          WHEN lower(cropdmgexp) = 'h' then 100.00
          WHEN lower(cropdmgexp) = 'k' then 1000.00
          WHEN lower(cropdmgexp) = 'm' then 1000000.00
          WHEN lower(cropdmgexp) = 'b' then 1000000000.00
        ELSE 0
        END as cropdmgexp
    FROM ds
      WHERE (IfNull(propdmg,0) > 0 or IfNull(cropdmg,0) > 0)
    ) v
  ")
#
# property damage totals
#
dsPropertyDamage<-sqldf("
  SELECT evtype
  , SUM(propertyDamage) as propertyDamage
  , SUM(cropDamage) as cropDamage
  , SUM(propertyDamage + cropDamage) as combinedDamage 
  FROM dsPropDmg 
  GROUP BY evtype ")

dsGroupPop<-sqldf("
SELECT evt.CatGroup
, SUM(ds.fatalities) as fatalities
, SUM(ds.injuries) as injuries
, SUM(ds.wgtdScore) as wgtdScore
FROM dsPopDmg ds
JOIN evtypeUnique evt
ON ds.evtype = evt.EventType
WHERE 1 = 1
GROUP BY evt.[CatGroup]")

dsGroupProp<-sqldf("
SELECT evt.CatGroup
, SUM(propertyDamage) propertyDamage
, SUM(cropDamage) cropDamage
, SUM(combinedDamage) combinedDamage
FROM dsPropertyDamage ds
JOIN evtypeUnique evt
on ds.evtype = evt.EventType
WHERE 1 = 1
GROUP BY evt.CatGroup
")


