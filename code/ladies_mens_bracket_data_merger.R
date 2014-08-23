##Bracket Merger
packages = c('dplyr','data.table','pipeR')
lapply(packages,library,character.only = T)
setwd("~/Desktop/Github/tennis_data")
#Ladies
ladies_bracket = read.csv('wta/usopen_2014/us_open_wta_bracket.csv') %>>% tbl_df()
ladies_metadata =  read.csv('wta/player_metadata/final_wta_female_player_metadata.csv') %>>% tbl_df()
missing =
	ladies_bracket[!ladies_bracket$player %in% ladies_metadata$player,'player']

ladies_bracket[ladies_bracket$player == missing[5],'player'] = 'Anastasia Pavlyuchenkova'
ladies_bracket[ladies_bracket$player == missing[12],'player'] ='Barbora Zahlavova Strycova'
ladies_bracket[ladies_bracket$player == missing[6],'player'] = 'Patricia Mayr-Achleitner'
ladies_bracket[ladies_bracket$player == missing[21],'player'] = 'Maria Teresa Torro Flor'
var_names = names(ladies_metadata)[c(1:2,4,6,7)]
ladies =
	merge(ladies_bracket,ladies_metadata[,names(ladies_metadata)[c(1:2,4,6,7)]], by = 'player', all.x = T)
ladies[ladies$player == 'Taylor Townsend','height_inches'] = 67
ladies[ladies$player == 'Taylor Townsend','date_of_birth'] = "4/6/96"
ladies[ladies$player == 'Taylor Townsend','weight_lbs'] = 170
ladies[ladies$player == 'Taylor Townsend','hand'] = "L"

ladies[ladies$player == 'Shelby Rogers','height_inches'] = 69
ladies[ladies$player == 'Shelby Rogers','date_of_birth'] = "10/13/92"
ladies[ladies$player == 'Shelby Rogers','weight_lbs'] = 155
ladies[ladies$player == 'Shelby Rogers','hand'] = "R"

ladies[ladies$player == 'Nicole Gibbs','height_inches'] = 66
ladies[ladies$player == 'Nicole Gibbs','date_of_birth'] = "03/03/92"
ladies[ladies$player == 'Nicole Gibbs','weight_lbs'] = 141
ladies[ladies$player == 'Nicole Gibbs','hand'] = "R"

ladies[ladies$player == 'Catherine Bellis','date_of_birth'] = "04/08/99"
ladies[ladies$player == 'Catherine Bellis','hand'] = "R"

ladies[ladies$player == 'Nicole Gibbs','height_inches'] = 66
ladies[ladies$player == 'Nicole Gibbs','date_of_birth'] = "03/03/92"
ladies[ladies$player == 'Nicole Gibbs','weight_lbs'] = 141
ladies[ladies$player == 'Nicole Gibbs','hand'] = "R"

ladies[ladies$player == 'Grace Min','height_inches'] = 66
ladies[ladies$player == 'Grace Min','date_of_birth'] = "05/06/94"
ladies[ladies$player == 'Grace Min','weight_lbs'] = 140
ladies[ladies$player == 'Grace Min','hand'] = "R"

ladies[ladies$player == 'Amandine Hesse','height_inches'] = 65
ladies[ladies$player == 'Amandine Hesse','date_of_birth'] = "01/16/93"
ladies[ladies$player == 'Amandine Hesse','weight_lbs'] = 121
ladies[ladies$player == 'Amandine Hesse','hand'] = "R"

ladies[ladies$player == 'Madison Brengle','height_inches'] = 64
ladies[ladies$player == 'Madison Brengle','date_of_birth'] = "04/03/90"
ladies[ladies$player == 'Madison Brengle','weight_lbs'] = 130
ladies[ladies$player == 'Madison Brengle','hand'] = "R"

ladies[ladies$player == 'Danielle Rose Collins','date_of_birth'] = "12/13/93"

ladies[ladies$player == 'Aleksandra Krunic','height_inches'] = 66
ladies[ladies$player == 'Aleksandra Krunic','date_of_birth'] = "03/15/93"
ladies[ladies$player == 'Aleksandra Krunic','weight_lbs'] = 106
ladies[ladies$player == 'Aleksandra Krunic','hand'] = "R"

missing2 = ladies[is.na(ladies$date_of_birth),'player']

ladies[ladies$player == missing2[1],'date_of_birth'] = "03/22/94"
ladies[ladies$player == missing2[1],'hand'] = "R"

ladies[ladies$player == missing2[2],'height_inches'] = 65
ladies[ladies$player == missing2[2],'date_of_birth'] = "04/24/96"
ladies[ladies$player == missing2[2],'weight_lbs'] = 137
ladies[ladies$player == missing2[2],'hand'] = "R"

ladies[ladies$player == missing2[3],'height_inches'] = 71
ladies[ladies$player == missing2[3],'date_of_birth'] = "02/15/97"
ladies[ladies$player == missing2[3],'hand'] = "R"

ladies[ladies$player == missing2[4],'height_inches'] = 69
ladies[ladies$player == missing2[4],'date_of_birth'] = "08/24/93"
ladies[ladies$player == missing2[4],'weight_lbs'] = 130
ladies[ladies$player == missing2[4],'hand'] = "R"

ladies[ladies$player == missing2[5],'height_inches'] = 66
ladies[ladies$player == missing2[5],'date_of_birth'] = "08/28/94"
ladies[ladies$player == missing2[5],'weight_lbs'] = 146
ladies[ladies$player == missing2[5],'hand'] = "R"

ladies[ladies$player == missing2[6],'height_inches'] = 66
ladies[ladies$player == missing2[6],'date_of_birth'] = "11/06/92"
ladies[ladies$player == missing2[6],'weight_lbs'] = 127
ladies[ladies$player == missing2[6],'hand'] = "R"

ladies[ladies$player == missing2[7],'height_inches'] = 69
ladies[ladies$player == missing2[7],'date_of_birth'] = "01/14/92"
ladies[ladies$player == missing2[7],'hand'] = "R"

ladies[ladies$player == missing2[8],'height_inches'] = 67
ladies[ladies$player == missing2[8],'date_of_birth'] = "02/05/94"
ladies[ladies$player == missing2[8],'hand'] = "R"

ladies[ladies$player == missing2[9],'height_inches'] = 73
ladies[ladies$player == missing2[9],'date_of_birth'] = "06/03/89"
ladies[ladies$player == missing2[9],'weight_lbs'] = 185
ladies[ladies$player == missing2[9],'hand'] = "R"

ladies$date_of_birth = ladies$date_of_birth %>>% as.Date('%m/%d/%y')
ladies$exact_age = as.numeric( as.Date(Sys.Date())-ladies$date_of_birth)/365.25
ladies$age = ladies$exact_age %>>% {substr(.,start = 1,stop = 2)} %>>% as.numeric()
write.csv(ladies,'wta/usopen_2014/ladies_bracket_with_metadata.csv', row.names = F)


mens_bracket = read.csv('atp/usopen_2014/us_open_atp_bracket.csv') %>>% tbl_df()
mens_metadata =  read.csv('atp/player_metadata/all_player_metadata.csv') %>>% tbl_df()

missing =
	mens_bracket[!mens_bracket$player %in% mens_metadata$player,'player']
mens =
	merge(mens_bracket,mens_metadata[,var_names], by = 'player', all.x = T)


mens[mens$player == missing[1],'height_inches'] = 72
mens[mens$player == missing[1],'date_of_birth'] = "02/27/90"
mens[mens$player == missing[1],'weight_lbs'] = 180
mens[mens$player == missing[1],'hand'] = "L"

mens[mens$player == missing[2],'date_of_birth'] = "07/24/93"
mens[mens$player == missing[2],'height_inches'] = 71
mens[mens$player == missing[2],'weight_lbs'] = 165
mens[mens$player == missing[2],'hand'] = "R"

mens[mens$player == missing[3],'date_of_birth'] = "06/10/87"
mens[mens$player == missing[3],'height_inches'] = 72
mens[mens$player == missing[3],'weight_lbs'] = 176
mens[mens$player == missing[3],'hand'] = "R"

mens[mens$player == missing[4],'date_of_birth'] = "11/14/99"
mens[mens$player == missing[4],'height_inches'] = 73
mens[mens$player == missing[4],'weight_lbs'] = 174
mens[mens$player == missing[4],'hand'] = "R"
missing[4] %>>% write.cb()

missing[5] %>>% write.cb()
mens[mens$player == missing[5],'date_of_birth'] = "01/27/93"
mens[mens$player == missing[5],'height_inches'] = 75
mens[mens$player == missing[5],'weight_lbs'] = 168
mens[mens$player == missing[5],'hand'] = "R"

missing[6] %>>% write.cb()
mens[mens$player == missing[6],'date_of_birth'] = "10/26/85"
mens[mens$player == missing[6],'height_inches'] = 72
mens[mens$player == missing[6],'weight_lbs'] = 165
mens[mens$player == missing[6],'hand'] = "R"

missing[7] %>>% write.cb(,col.names = F)
mens[mens$player == missing[7],'date_of_birth'] = "11/11/89"
mens[mens$player == missing[7],'height_inches'] = 69
mens[mens$player == missing[7],'weight_lbs'] = 154
mens[mens$player == missing[7],'hand'] = "R"

missing[8] %>>% write.cb(,col.names = F)
mens[mens$player == missing[8],'date_of_birth'] = "02/21/96"
mens[mens$player == missing[8],'height_inches'] = 69
mens[mens$player == missing[8],'weight_lbs'] = 145
mens[mens$player == missing[8],'hand'] = "R"

missing[9] %>>% write.cb(,col.names = F)
mens[mens$player == missing[9],'date_of_birth'] = "05/20/92"
mens[mens$player == missing[9],'height_inches'] = 69
mens[mens$player == missing[9],'weight_lbs'] = 154
mens[mens$player == missing[9],'hand'] = "R"

missing[10] %>>% write.cb(,col.names = F)
mens[mens$player == missing[10],'date_of_birth'] = "09/06/87"
mens[mens$player == missing[10],'height_inches'] = 75
mens[mens$player == missing[10],'weight_lbs'] = 185
mens[mens$player == missing[10],'hand'] = "R"

missing[11] %>>% write.cb(,col.names = F)
mens[mens$player == missing[11],'date_of_birth'] = "10/09/96"
mens[mens$player == missing[11],'height_inches'] = 74
mens[mens$player == missing[11],'weight_lbs'] = 155
mens[mens$player == missing[11],'hand'] = "R"

missing[12] %>>% write.cb(,col.names = F)
mens[mens$player == missing[12],'date_of_birth'] = "09/27/95"
mens[mens$player == missing[12],'height_inches'] = 67
mens[mens$player == missing[12],'weight_lbs'] = 138
mens[mens$player == missing[12],'hand'] = "R"

missing[13] %>>% write.cb(,col.names = F)
mens[mens$player == missing[13],'date_of_birth'] = "02/27/92"
mens[mens$player == missing[13],'height_inches'] = 73
mens[mens$player == missing[13],'weight_lbs'] = 165
mens[mens$player == missing[13],'hand'] = "R"

mens$date_of_birth =
	mens$date_of_birth %>>% as.Date('%m/%d/%y')
mens$exact_age =
	as.numeric( as.Date(Sys.Date())-mens$date_of_birth)/365.25
mens$age =
	mens$exact_age %>>% {substr(.,start = 1,stop = 2)} %>>% as.numeric()
write.csv(mens,'atp/usopen_2014/mens_bracket_with_metadata.csv', row.names = F)

all = rbind(mens,ladies)

players_codes = all[,c('player','country_code')]

players_codes = players_codes %>>% {merge(.,y = ladies_metadata[,c('player','primary_country')], all.x =  T)}
names(mens_metadata)[10] = 'primary_country'
players_codes = players_codes %>>% {merge(.,y = mens_metadata[,c('player','primary_country')], all.x =  T)}

countries_codes = players_codes[,2:3] %>>% unique()
names(countries_codes)[1] = 'country'
countries_codes[1,'country'] = 'France'
countries_codes[3,'country'] = 'Australia'
countries_codes[4,'country'] = 'Spain'
row.names(countries_codes) = 1:nrow(countries_codes)
countries_codes
countries_codes = countries_codes %>>%
	arrange(country_code)
countries_codes[1,'country'] = 'Argentina'
countries_codes[1,'country'] = 'Argentina'
countries_codes[82:84,'country'] = 'Slovakia'
countries_codes[c(5,6,9,10,11,13,15:16,19,21,22),'country'] = c('Australia','Austria','Belgium','Bosnia','Belarus','Belarus','Brazil','Bulgaria','Canada','China','Colombia')
countries_codes[23:25,'country'] = 'Croatia'
countries_codes[c(27:29),'country'] = 'Czech Republic'
countries_codes[c(27:29),'country'] = 'Czech Republic'
countries_codes[c(75:78),'country'] = 'Serbia'
countries_codes[c(79:81),'country'] = 'Switzerland'
countries_codes[c(69:72),'country'] = 'Russia'
countries_codes[c(73:74),'country'] = 'Slovakia'
countries_codes[88,'country'] = 'Tunisia'
countries_codes[90,'country'] = 'Ukraine'
countries_codes[91,'country'] = 'Uruguay'
countries_codes[93,'country'] = 'USA'
countries_codes[94,'country'] = 'Uzbekistan'

countries_codes = countries_codes %>>% unique()
row.names(countries_codes) = 1:nrow(countries_codes)
countries_codes[c(13,15,16,19,21,30,35,37,39:40,42,46),'country'] =
	c('Cypress','Denmark','Dominican Republic','Finland','France','Israel','Japan','Kazakhstan','Latvia','Luxembourg','Netherlands','Poland')
countries_codes = countries_codes %>>% unique()
row.names(countries_codes) = 1:nrow(countries_codes)
countries_codes[21:22,'country'] = 'Great Britain'
countries_codes[23:26,'country'] = 'Germany'
countries_codes[30:31,'country'] = 'Italy'
countries_codes[30:31,'country'] = 'Italy'
countries_codes = countries_codes %>>% unique()
row.names(countries_codes) = 1:nrow(countries_codes)
players_codes[players_codes$country_code == 'MDA','player'] %>>% write.cb(col.names = F)
countries_codes[35,'country'] = 'Portugal'
countries_codes[38:39,'country'] = 'South Africa'
countries_codes[36,'country'] = 'Puerto Rico'
countries_codes[31,'country'] = 'Moldova'
countries_codes[24,'country'] = 'Ireland'

ladies = merge(ladies,countries_codes,all.x = T)
mens = merge(mens,countries_codes,all.x = T)
write.csv(ladies,'wta/usopen_2014/ladies_bracket_with_metadata.csv',row.names = F)
write.csv(mens,'atp/usopen_2014/mens_bracket_with_metadata.csv',row.names = F)
