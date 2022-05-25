library(readxl)
try(setwd("C:/Users/tknow/Desktop/project/analysis"))
try(setwd("~/OneDrive - Cardiff University/Conferences/EmberWorkshop/projects/Knowlton/project/processing/"))

monsters = read.csv("../data/raw/monsters/monsters.csv",stringsAsFactors = F,encoding = "UTF-8")

# Coding monsters as present
monsters$monster_present = monsters$hybrid == "Present"
# Mark unclear cases
monsters$monster_unclear = monsters$hybrid == "Unclear"
# Remove 2 cases of missing data
monsters$monster_present[monsters$hybrid == ""] = NA
monsters = monsters[!is.na(monsters$hybrid), ]

# Load glottolog data
# (from https://cdstar.shh.mpg.de/bitstreams/EAEA0-E62D-ED67-FD05-0/glottolog_languoid.csv.zip)
glottolog =  read.csv("../data/raw/glottolog_languoid.csv/languoid.csv",stringsAsFactors = F)
# Magic to get language family for each language
glottolog$family = glottolog[match(glottolog$family_id,glottolog$id),]$name
# Remove old languages
glottolog = glottolog[is.na(glottolog$family) | glottolog$family!="Bookkeeping",]

monsters$language_family = glottolog[match(monsters$glottocode,glottolog$id),]$family
# NAs are isolates
isolate = is.na(monsters$language_family)
monsters$language_family[isolate] = 
  paste("Isolate:", 
  glottolog[match(monsters[isolate,]$glottocode,glottolog$id),]$name)

monsters$iso = glottolog[match(monsters$glottocode,glottolog$id),]$iso639P3code

#monsters$latitude = glottolog[match(monsters$glottocode,glottolog$id),]$latitude
#monsters$longitude = glottolog[match(monsters$glottocode,glottolog$id),]$longitude

# Add location
soc = read.csv("../data/raw/dplace/SCSS_societies.csv",stringsAsFactors = F)

monsters$latitude = soc[match(monsters$id,soc$id),]$Lat
monsters$longitude = soc[match(monsters$id,soc$id),]$Long


# Add caste
# Load caste
caste = read.csv("../data/raw/dplace/SCCS Caste.csv",stringsAsFactors = F)

# Find matches based on xd-ids
casteMatch = match(monsters$xd_id, caste$society_xd_id)
# Add caste info
monsters$caste = caste[casteMatch,]$code_label
# Create binary info
monsters$caste.stratified = monsters$caste != "Absence of distinctions"
# Add focal year info
monsters$caste.focal_year = caste[casteMatch,]$focal_year

# Add class
socClass = read.csv("../data/raw/dplace/SCCS Class.csv",stringsAsFactors = F)

# Find matches based on xd-ids
classMatch = match(monsters$xd_id, socClass$society_xd_id)
# Add class info
monsters$class = socClass[classMatch,]$code_label
# Create binary info
monsters$class.stratified = monsters$class != "Absence of distinctions"
# Add focal year info
monsters$class.focal_year = socClass[classMatch,]$focal_year

# Add market exchange
# Load market exchange
market = read.csv("../data/raw/dplace/SCCS Market Exchange.csv",stringsAsFactors = F)

# Find matches based on xd-ids
marketMatch = match(monsters$xd_id, market$society_xd_id)
# Add market info
monsters$market = market[marketMatch,]$code
# Add focal year info
monsters$market.focal_year = market[marketMatch,]$focal_year


# Add urbanization
# Load urbanization
urban = read.csv("../data/raw/dplace/SCCS Urban.csv",stringsAsFactors = F)

# Find matches based on xd-ids
urbanMatch = match(monsters$xd_id, urban$society_xd_id)
# Add urbanization info
monsters$urban = urban[urbanMatch,]$code
# Add focal year info
monsters$urban.focal_year = urban[urbanMatch,]$focal_year

# Add agriculture
# Load agriculture
agriculture = read.csv("../data/raw/dplace/SCCS Agriculture.csv",stringsAsFactors = F)

# Find matches based on xd-ids
agMatch = match(monsters$xd_id, agriculture$society_xd_id)
# Add agriculture info
monsters$ag = agriculture[agMatch,]$code
# Add focal year info
monsters$ag.focal_year = agriculture[agMatch,]$focal_year

# Add Population Density
# Load population density
popdens = read.csv("../data/raw/dplace/SCCS Density of Population.csv",stringsAsFactors = F)

# Find matches based on xd-ids
popMatch = match(monsters$xd_id, popdens$society_xd_id)
# Add population density info
monsters$popdens = popdens[popMatch,]$code
# Add focal year info
monsters$popdens.focal_year = popdens[popMatch,]$focal_year

# Add Fixity of Residence
# Load Fixity of Residence
fixity = read.csv("../data/raw/dplace/SCCS Fixity of Residence.csv",stringsAsFactors = F)

# Find matches based on xd-ids
fixityMatch = match(monsters$xd_id, fixity$society_xd_id)
# Add Fixity of Residence info
monsters$fixity = fixity[fixityMatch,]$code
# Add focal year info
monsters$fixity.focal_year = fixity[fixityMatch,]$focal_year

# Add Land Transport
# Load land transport
land = read.csv("../data/raw/dplace/SCCS Land Transport.csv",stringsAsFactors = F)

# Find matches based on xd-ids
landMatch = match(monsters$xd_id, land$society_xd_id)
# Add land transport info
monsters$land = land[landMatch,]$code
# Add focal year info
monsters$land.focal_year = land[landMatch,]$focal_year

# Add Money
# Load Money
money = read.csv("../data/raw/dplace/SCCS Money.csv",stringsAsFactors = F)

# Find matches based on xd-ids
moneyMatch = match(monsters$xd_id, money$society_xd_id)
# Add urbanization info
monsters$money = money[moneyMatch,]$code
# Add focal year info
monsters$money.focal_year = money[moneyMatch,]$focal_year

# Add Political Integration
# Load political integration
politic = read.csv("../data/raw/dplace/SCCS Political Integration.csv",stringsAsFactors = F)

# Find matches based on xd-ids
politicMatch = match(monsters$xd_id, politic$society_xd_id)
# Add political integration info
monsters$politic = politic[politicMatch,]$code
# Add focal year info
monsters$politic.focal_year = politic[politicMatch,]$focal_year

# Add Social Stratification
# Load social stratification
strata = read.csv("../data/raw/dplace/SCCS Social Stratification.csv",stringsAsFactors = F)

# Find matches based on xd-ids
strataMatch = match(monsters$xd_id, strata$society_xd_id)
# Add stratification info
monsters$strata = strata[strataMatch,]$code
# Add focal year info
monsters$strata.focal_year = strata[strataMatch,]$focal_year

# Add Technological Specialization
# Load technological specialization
tech = read.csv("../data/raw/dplace/SCCS Technological Specialization.csv",stringsAsFactors = F)

# Find matches based on xd-ids
techMatch = match(monsters$xd_id, tech$society_xd_id)
# Add technological specialization info
monsters$tech = tech[techMatch,]$code
# Add focal year info
monsters$tech.focal_year = tech[techMatch,]$focal_year

# Add Writing and Records
# Load writing and records
writing = read.csv("../data/raw/dplace/SCCS Writing and Records.csv",stringsAsFactors = F)

# Find matches based on xd-ids
writingMatch = match(monsters$xd_id, writing$society_xd_id)
# Add writing and records info
monsters$writing = writing[writingMatch,]$code
# Add focal year info
monsters$writing.focal_year = writing[writingMatch,]$focal_year

# Add market exchange
# Load market exchange
market = read.csv("../data/raw/dplace/SCCS Market Exchange.csv",stringsAsFactors = F)

# Find matches based on xd-ids
marketMatch = match(monsters$xd_id, market$society_xd_id)
# Add market info
monsters$market = market[marketMatch,]$code
# Add focal year info
monsters$market.focal_year = market[marketMatch,]$focal_year

# Contact with other societies
contact = read.csv("../data/raw/dplace/SCSS_ContactWithOtherSocieties.csv",stringsAsFactors = F)
# Find matches based on xd-ids
contactMatch = match(monsters$xd_id, contact$society_xd_id)
# Add writing and records info
monsters$contact = contact[contactMatch,]$code_label
# Add focal year info
monsters$contact.focal_year = writing[contactMatch,]$focal_year

# Add info on sources from HRAF

hraf = read_xlsx("../data/raw/HRAF/Summary_WC_Arch-Combined_20210630.xlsx")
hraf = hraf[1:(nrow(hraf)-2),]
hid = gsub('\\)',"",gsub(".+\\(","",monsters$HRAF_name_ID))
monsters$HRAFNumPages = hraf[match(hid,hraf$OWC),]$`Page Count`
monsters$HRAFNumSources = hraf[match(hid,hraf$OWC),]$`Document Count`

monsters[monsters$glottocode == "nene1249",]$HRAFNumSources = 16
monsters[monsters$glottocode == "nene1249",]$HRAFNumPages = 1024
monsters[monsters$glottocode == "manc1252",]$HRAFNumSources = 32
monsters[monsters$glottocode == "manc1252",]$HRAFNumPages = 2873

# Write data
write.csv(monsters, "../data/clean/monsters.csv")


## Phylogeny stuff

# Jaeger's world tree - convert to glottocode/dplace codes
library(ape)
t = read.tree("../data/raw/trees/Global/Jaeger_world.tre")

l = read.table("../data/raw/trees/Global/dataset.tab",
               sep="\t",quote='"',na.strings="",stringsAsFactors = F,
               header = T)
tipCodes = sapply(t$tip.label,function(X){strsplit(X,"\\.")[[1]][3]})

isoCodes = l[match(tipCodes,l$names),]$iso

glottoCodes = glottolog[match(isoCodes,glottolog$iso639P3code),]$id

t$tip.label = glottoCodes

# Drop tips that aren't in our data!

t = drop.tip(t,which(duplicated(t$tip.label)))
t = drop.tip(t, t$tip.label[!t$tip.label %in% monsters$glottocode])
write.nexus(t,
    file="../data/raw/trees/Global/JaegerGlobalTree_SCSS.tree")

# Write 'taxa' file mapping tip names

SCSSIDs = soc[match(t$tip.label,soc$glottocode),]$id

write.csv(
  data.frame(
    taxon = t$tip.label,
    glottocode = t$tip.label,
    soc_ids = SCSSIDs),
  file = "../data/raw/trees/Global/taxa.csv",row.names = F)
