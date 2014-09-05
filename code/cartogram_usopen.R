#devtools::install_github("hadley/httr")

library(httr)
library(pipeR)
library(rlist)
library(dplyr)
library(tidyr)
library(countrycode)



#US Open Completed Match Scores from Kimono
us_data <- GET(paste0("https://www.kimonolabs.com/api/",apikey)) %>>%
  content( as = "parsed" )

us_data$results$collection1 %>>%
  list.filter(
    type == "Men's Singles" || type == "Women's Singles"
  ) %>>% 
  list.map(
    l -> rbind(
      data.frame(
        country = l$player1Country
        ,type = l$type
        ,gameswon = list.match( l, "player1S.*" ) %>>%
          unlist %>>%
          ( gsub( x = ., pattern = "[\\[\\]]",replacement = "") )  %>>% 
          substr(1,1) %>>%
          as.numeric %>>%
          na.omit %>>%
          sum
      )
      ,data.frame(
        country = l$player2Country
        ,type = l$type
        ,gameswon = list.match( l, "player2S.*" ) %>>%
          unlist %>>%
          ( gsub( x = ., pattern = "[\\[\\]]",replacement = "") )  %>>% 
          substr(1,1) %>>%
          as.numeric %>>%
          na.omit %>>%
          sum
      )
    ) 
  ) %>>%
  list.stack %>>%
  mutate ( isocountry = ifelse(country == "TPE", "TW", countrycode(country,"ioc","iso2c") ) ) %>>%
  group_by( country, isocountry, type ) %>>%
  summarise( gameswon = sum(gameswon) ) %>>%
  ( .[ order(.$gameswon, decreasing = T ), ] )  %>>%
  spread( type, gameswon ) -> countryGames
  


carto <- rCharts$new()

carto$setLib(".")
carto$lib = "carto"
carto$LIB$name = "carto"


carto$set(
  map = "worldcountries.topojson"  #url to the topojson source
)


#for now use the example from mayer cartogram to join
#eventually get all countries from the topojson
lang_pop <- read.csv( "./data/nrlangspop.csv", stringsAsFactors = F )

# do the join to make sure every country gets a value
lang_pop %>>%
  #handle Namibia where "NA" becomes R <NA>
  mutate(
    NAME = ifelse( is.na(NAME), "NA", NAME)
  ) %>>%
  merge( countryGames ,by.x="NAME",by.y="isocountry",all.x = T ) %>>%
  (~ joinedData = . ) %>>%
  carto$set (
    data = .
    , x = "Men's Singles"
  )



carto$setTemplate(
  chartDiv = "
  <{{container}} id = '{{ chartId }}' class = '{{ lib }}' style='height:100%;width:100%;'>
  <div id='map-container'>
  <!--img id='placeholder' alt='placeholder image for old browsers' src='placeholder.png'-->
  <svg id = 'map' style = 'height:100%; width:100%' viewbox = '0 0 {{params.width}} {{params.height}}'>
  </svg>
  </div>
  </{{ container}}>" 
  , script = "./layouts/chart.html"
)


carto$save("world_usopen.html",cdn=T)


carto <- rCharts$new()

carto$setLib(".")
carto$lib = "carto"
carto$LIB$name = "carto"


carto$params$data = joinedData

carto$set(
  map = "worldcountries.topojson"  #url to the topojson source
  ,title = "Cartogram of the US Open"
  ,fields = list(
    list(name= "(no scale)", id= "none"),
    list(name= "Men's Singles", id= "men", key= "Men's Singles", unit= " games won"),
    list(name= "Women's Singles", id= "women", key= "Women's Singles", unit= " games won")
  )
)

carto$setTemplate(
  chartDiv = "
  <{{container}} id = '{{ chartId }}' class = '{{ lib }}' style='height:100%;width:100%;'>
  <h1> {{ params.title }} </h1>
  <form>
  <p>
  <label>Scale by <select id='field'></select></label>
  <span id='status'></span><br>
  </p>
  </form>
  <div id='map-container'>
  <svg id = 'map' style = 'height:100%; width:100%' viewbox = '0 0 {{params.width}} {{params.height}}'>
  </svg>
  </div>
  </{{ container}}>" 
  , script = "./layouts/chart_mayer_dynamicfields.html"
)

carto$save("world_usopen_mayer.html",cdn=T)
