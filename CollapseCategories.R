evtypeUnique<-sqldf("SELECT evtype as eventType, count(1) as Counter FROM ds GROUP BY evtype")

evtypeUnique$group <- 
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

