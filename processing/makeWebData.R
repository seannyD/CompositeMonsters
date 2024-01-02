library(readxl)
library(jsonlite)
library(stringr)
try(setwd("C:/Users/tknow/Desktop/project/analysis"))
try(setwd("~/OneDrive - Cardiff University/Conferences/EmberWorkshop/projects/Knowlton/project/processing/"))

monsters = read.csv("../data/clean/monsters.csv",stringsAsFactors = F,encoding = "UTF-8")


d = monsters[,c(   "pref_name_for_society",
                   "hybrid","monster_name",
                   'source',
                   "notes",
                   'latitude','longitude',
                   'id','HRAF_name_ID','xd_id','main_focal_year')]

d$HRAFID = substr(str_extract(monsters$HRAF_name_ID,"\\((.+?)\\)"),2,5)

d$monster_name[is.na(d$monster_name)] = ""
d$monster_name = tools::toTitleCase(d$monster_name)
d$notes[is.na(d$notes)] = ""

#js = toJSON(list(data=d))

write_json(list(data=d),"/Library/WebServer/Documents/HybridMonsters/data/data.json")
