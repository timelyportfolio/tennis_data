library(pipeR)
library(igraph)
library(vcd)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(htmltools)
library(XML)
library(gridSVG)
library(SVGAnnotation)
library(rCharts)

read.csv("./atp/match_results_odds_2000_2014/all_atp_matches_2000_2014_odds.csv/all_atp_matches_2000_2014_odds.csv"
  ,stringsAsFactors = F
) -> atp 


atp %>>%
  filter( tournament == "US Open" ) %>>%
  { .[,c("winner","loser")] } %>>%
  na.omit %>>% 
  as.matrix() %>>%
  graph.edgelist  %>>% #-> atp_g
  (~
    betweenness(.) %>>%
    sort(decreasing=T) %>>%
    head(20) %>>% 
    barplot
  ) %>>%
  (~
     authority.score(.) %>>%
     { sort(.$vector,decreasing=T ) } %>>%
     head(20) %>>%
     barplot
  ) -> atp_g



unique(cut(as.numeric(atp$lrank),breaks=c(seq(0,50,5)),right=F))

#with(atp,table(winner,surface,court)) %>>% mosaic
atp %>>%
  (~
    filter ( ., winner == "Federer R." | loser == "Federer R.") %>>%
    mutate ( outcome = ifelse(loser == "Federer R.","lost","won" ) ) %>>%
    mutate ( lrank_c = cut(as.numeric(ifelse(winner == "Federer R.",lrank,wrank)),breaks=c(0,5,20),right=F) ) %>>%
    with(table(outcome,round,series)) %>>% assoc( shade = T )
  ) %>>%
  (~
     
  )  



atp %>>%
  mutate( year = format(as.Date(date),"%Y") ) %>>%
  ggplot() %>>%
  + geom_density(aes(x=as.numeric(lrank),colour="red")) %>>%
  + geom_density(aes(x=as.numeric(wrank),colour="green")) %>>%
  + xlim(c(0,50)) %>>%
  (~ print( . + facet_wrap( ~round, ncol = 1 ) ) ) %>>%
  (~ print( . + facet_wrap( ~series, ncol = 1 ) ) ) %>>%
  (~ print( . + facet_wrap( ~surface, ncol = 1 ) ) ) %>>%
  (~ print( . + facet_grid( round~year ) ) ) 



atp %>>%
  filter( tournament == "US Open" ) %>>%
  select( winner_last_name, loser_last_name ) %>>%
  rCharts::to_json( orient = "records" ) %>>%
  cat( file = "usopendata.json")


# try to get total sets won for network weight
atp %>>%
  filter( tournament == "US Open" ) %>>%
  group_by ( date, winner, loser ) %>>%
  do( data.frame(
    wongames = with( ., sum( na.omit( w1, w2, w3, w4, w5) ) )
    ,lostgames = with( ., sum( na.omit( l1, l2, l3, l4, l5) ) )
  ) ) %>>%
  mutate ( year = as.numeric( format( as.Date(date), "%Y" ) ) ) %>>%
  na.omit %>>%
  as.data.frame( stringsAsFactors = F ) %>>%
  filter( year == 2013) %>>%
  select( winner, loser) %>>% #, wongames, lostgames, year ) %>>%
  as.matrix() %>>%
  graph.edgelist -> atp_us_g



unique(cut(as.numeric(atp$lrank),breaks=c(seq(0,50,5)),right=F))

atp %>>%
  #filter( tournament == "US Open" ) %>>%
  select( wrank, lrank, round ) %>>%
  mutate( wrank = as.numeric(wrank), lrank = as.numeric(lrank) ) %>>%
  gather( outcome, rank, -round ) %>>%
  na.omit() %>>% 
  mutate( outcome = substr(outcome,1,1) ) %>>%
  (~ print( 
    ggplot( data=., aes( x = round, y= as.numeric(rank) ) ) %>>%
      (. + geom_boxplot( aes(colour=outcome)  )  ) + ylim( 0, 100 ) )
   ) %>>%
  group_by( round, outcome ) %>>%
  summarise( avg_rank = mean( as.numeric(rank) ) ) %>>% 
  ggplot( aes( x = round, y=avg_rank ) ) + geom_point(  )


"./wta/ranking_history/wta_rankings_1983_2014.csv" %>>%
  read.csv( stringsAsFactors = F ) -> wta_data

wta_data %>>%
  filter( last_name == "Williams", first_name == "Serena" ) %>>%
  select( rank_date, current_rank ) %>>%
  (~
     print(
       ggplot( data = ., aes( x= as.Date(rank_date), y = current_rank ) ) + geom_point()
     )
  )

"./wta/serena_williams_data_08_25_14.csv" %>>%
  read.csv( stringsAsFactors = F ) -> serena

serena %>>%
  (
    dPlot(
      data = .
      , y = "percentage"
      , x = c( "type", "tour" )
      , groups = c( "result" )
      , type = "line"
    )
  ) %>>%
  (~ .$publish() ) 




"./atp/mens_top_game_data_with_updated_metadata.csv" %>>% 
  read.csv( stringsAsFactors = F) -> us_men

us_men %>>%
  select(player,singles_rank,srv_rank,rtn_rank,serve_games_won_pct, return_games_won_pct,career_win_pct,age) %>>%
  { .[order(.$singles_rank),] }%>>%
  jsonlite::toJSON() %>>% cat(file = "./atp/us_men/us_men.json")

readHTMLList("http://www.atpworldtour.com/Tennis/Players/Top-Players/Novak-Djokovic.aspx?t=mf")[28:29] -> djokovic

data.frame(
  player="Novak Djokovic"
  ,singles_rank=1
  ,serve_rank=7
  ,return_rank=5
  ,serve_games_won_pct=88
  ,return_games_won_pct=30
  ,career_win_pct=81
  ,age=27
) %>>% jsonlite::toJSON()







# serve data August 27, 2014
"./atp/mens_serving_data_melt.csv" %>>%
  read.csv(stringsAsFactors = F) -> top_men
"./atp/womens_serving_data_melt.csv" %>>%
  read.csv(stringsAsFactors = F) -> top_women

rbind(top_men,top_women) %>>%
  #select ( -(c(2:4,9:10) ) ) %>>%
  #gather ( player, value, -position ) %>>%
  # %>>%
  (
    dPlot(
      player ~ position
      ,z = "game_win_pct"
      ,data = .
      ,type = "bar"
      ,height = 500
      ,width = 500
      ,yAxis = list(
        type = "addCategoryAxis"
      )
      ,zAxis = list(
        type = "addMeasureAxis"
        ,overrideMin = 0
        ,overrideMax = 1
      )
      ,colorAxis = list(
        type = "addColorAxis"
        ,colorSeries = 'game_win_pct'
        ,palette = RColorBrewer::brewer.pal(9,"Blues")[c(2,6)]
        ,outputFormat = "0.2%"
      )
      ,bounds = list( x = 150, y = 50, width = 330, height = 200 )
    )
  ) %>>%
  (~ .$show() ) %>>% 
  ( ~ .$set( facet = list(y = "draw" , removeAxes=T) ))





#%>>%
    (~
       #with the bar let's play with tooltips
       #first tooltip will just be the number in the center of the bar
       .$setTemplate(
          script = "http://timelyportfolio.github.io/rCharts_dimple/chart_tooltip_flexible.html"
          #this template is designed to let us define onHover and onLeave
          #with thought afterScript but does not work since separate <script> block
          #so use chartDiv instead          
          ,chartDiv = 
            '
       <div id = "{{chartId}}"></div>
          <script>
          function onHover(e){
          //ugly but it works; if tooltip exists then select otherwise append
          //whole d3 enter, update, exit makes things difficult here
          var custTool = (
          d3.select(e.selectedShape[0][0].parentNode).select("#chartTooltip")[0][0] ? 
          d3.select(e.selectedShape[0][0].parentNode).select("#chartTooltip") :
          d3.select(e.selectedShape[0][0].parentNode).append("text").attr("id","chartTooltip")
          )
          
          custTool
          //reads outputFormat and assumes it exists
          .text(d3.format(opts.colorAxis.outputFormat)(e.selectedShape.data()[0].cValue))
          //turn dispay on with css since none from leave
          .style("display",null)
          .style("pointer-events","none")
          //use x and y from selected rectangle to position with transform for center
          .attr("x",e.selectedShape.attr("x"))
          .attr("y",e.selectedShape.attr("y"))
          //move to center
          .attr("transform",
          "translate(" + e.selectedShape.attr("width") / 2 + "," + e.selectedShape.attr("height")/2 + ")"
          )
          .attr("dy",6)
          .style("text-anchor","middle")
          }
          function onLeave(e){
          myChart.svg.select("#chartTooltip")
          .style("display","none");
          }
          </script>
          '
          ,afterScript = '<script></script>'
       )
    ) 

base_size <- 9
rbind(top_men,top_women) %>>%
ggplot( aes(player, position)) %>>%
 + geom_tile(aes(fill = game_win_pct),colour = "white") %>>%
  + scale_fill_gradient(low = "white",  high = "steelblue") %>>%
  + theme_grey(base_size = base_size) %>>%
  + labs(x = "", y = "") %>>%
  + scale_x_discrete(expand = c(0, 0)) %>>%
  + scale_y_discrete(expand = c(0, 0)) %>>%
  + opts(
    legend.position = "none"
    , axis.ticks = theme_blank()
    , axis.text.x = theme_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50")
  )