library(plyr);library(XML);library(pipeR)
men_bracket = data.frame()
url1 = 'http://www.usopen.org/en_US/scores/draws/ms/r1s1.html'
url2 = 'http://www.usopen.org/en_US/scores/draws/ms/r1s2.html'
url3 = 'http://www.usopen.org/en_US/scores/draws/ms/r1s3.html'
url4 = 'http://www.usopen.org/en_US/scores/draws/ms/r1s4.html'
tables1 = readHTMLTable(url1,header = F)
tables2 = readHTMLTable(url2,header = F)
tables3 = readHTMLTable(url3,header = F)
tables4 = readHTMLTable(url4,header = F)

#URL 1

for(i in 1:length(tables1)){
	table = data.frame(tables1[i])
	player = table[c(1,3),1]
	match = 
		data.frame(player = player)
	p = colsplit(match$player,pattern = '\\(',c('player','country_code_rank'))
	p = cbind(p$player,colsplit(p$country_code_rank,pattern = '\\[',c('country_code','rank')))
	names(p)[1] = 'player'
	p$player = Trim(p$player)
	p[,2:3] = apply(p[,2:3],2, function(x) Trim(gsub(pattern = '\\)|\\]','',x)))
	p$match = i
	p$tour = 'ATP'
	p$round = 1
	men_bracket = rbind.fill(men_bracket,p)
}

#URL 2
for(i in 1:length(tables2)){
table = data.frame(tables2[i])
player = table[c(1,3),1]
match = 
	data.frame(player = player)
p = colsplit(match$player,pattern = '\\(',c('player','country_code_rank'))
p = cbind(p$player,colsplit(p$country_code_rank,pattern = '\\[',c('country_code','rank')))
names(p)[1] = 'player'
p$player = Trim(p$player)
p[,2:3] = apply(p[,2:3],2, function(x) Trim(gsub(pattern = '\\)|\\]','',x)))
p$match = i + 16
p$tour = 'ATP'
p$round = 1
men_bracket = rbind.fill(men_bracket,p)
}

## URL 3
for(i in 1:length(tables3)){
	table = data.frame(tables3[i])
	player = table[c(1,3),1]
	match = 
		data.frame(player = player)
	p = colsplit(match$player,pattern = '\\(',c('player','country_code_rank'))
	p = cbind(p$player,colsplit(p$country_code_rank,pattern = '\\[',c('country_code','rank')))
	names(p)[1] = 'player'
	p$player = Trim(p$player)
	p[,2:3] = apply(p[,2:3],2, function(x) Trim(gsub(pattern = '\\)|\\]','',x)))
	p$match = i + 32
	p$tour = 'ATP'
	p$round = 1
	men_bracket = rbind.fill(men_bracket,p)
}

## URL 4
for(i in 1:length(tables4)){
	table = data.frame(tables4[i])
	player = table[c(1,3),1]
	match = 
		data.frame(player = player)
	p = colsplit(match$player,pattern = '\\(',c('player','country_code_rank'))
	p = cbind(p$player,colsplit(p$country_code_rank,pattern = '\\[',c('country_code','rank')))
	names(p)[1] = 'player'
	p$player = Trim(p$player)
	p[,2:3] = apply(p[,2:3],2, function(x) Trim(gsub(pattern = '\\)|\\]','',x)))
	p$match = i + 48
	p$tour = 'ATP'
	p$round = 1
	men_bracket = rbind.fill(men_bracket,p)
}

men_bracket$player_id = 
	1:nrow(men_bracket)

men_bracket[men_bracket$rank %in% '','rank'] = NA
men_bracket$scrape_time = Sys.Date()
setwd("~/Desktop/Github/tennis_data/atp/usopen_2014")
write.csv(men_bracket,'us_open_atp_bracket.csv',row.names = F)
