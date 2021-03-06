setwd('/Users/chase/Desktop/RProjects/ds202_project/')
install.packages('spotifyr')
library(spotifyr)

library(tidyverse)

pageAsDf <- function(URL)
{
  page <- readLines(URL)
  page <- as.factor(page)
  page <- page[grepl("/artist/", page)]
  pageDf <- data.frame(page,row.names = NULL)
  pageDf <- pageDf %>% mutate(Artist = gsub('</a>.*','',gsub('.*class="artist">', "", pageDf[,1])), Album = gsub("</a><.*","",gsub('.*"album">',"",pageDf[,1])))
  return(pageDf)
}
?select
jazzPage <- pageAsDf('https://rateyourmusic.com/find/userchart?usernames=Zappaholic%2C+Zed%2C+Groumfy%2C+timregler%2C+smzig99%2C+bruklover%2C+Footprints%2C+kjamag%2C+jazz_guru%2C+afroclonk%2C+jazyjef%2C+yofriend%2C+dannyhutton%2C+supportors%2C+DemBonez%2C+nervenet%2C+bluby%2C+HumanaCat%2C+p_lemberg%2C+idlemoments%2C+Senyer%2C+joNas%2C+bevrich%2C+superpang%2C+voyteck%2C+dre2k19%2C+BlueTrain%2C&usernames=Zappaholic%2c+Zed%2c+Groumfy%2c+timregler%2c+smzig99%2c+bruklover%2c+Footprints%2c+kjamag%2c+jazz%5fguru%2c+afroclonk%2c+jazyjef%2c+yofriend%2c+dannyhutton%2c+supportors%2c+DemBonez%2c+nervenet%2c+bluby%2c+HumanaCat%2c+p%5flemberg%2c+idlemoments%2c+Senyer%2c+joNas%2c+bevrich%2c+superpang%2c+voyteck%2c+dre2k19%2c+BlueTrain%2c')
jazzPage

indiePage <- pageAsDf('https://rateyourmusic.com/find/userchart?usernames=Prkl%2C+Iai%2C+SoylentBlack%2C+Iamthegamer%2C+indiesoc%2C+indiefan13%2C+Tezcatlipoca%2C+jshopa%2C+StretchOutAndWait%2C+Mikey2000%2C+bundtcake%2C+warg2%2C+shish%2C+PsychicWar%2C+Uppanun%2CJE_Androcoen%2C+%2C+lyla%2C+dannniel%2C+harrisped%2C+emptyflow%2C+eyeblueday%2C+shelterfromO%2C+BrokeAsAJoke%2C+idna%2C+builtonsquares%2C+Jdamen%2C+saturos%2C+esmoriak%2C+Fleurquin%2C+Hoya4Peace%2C+artists_only%2C+melitene%2C+hoshiko2600%2C+seaswell%2C+hiker%2C+xploder%2C+trelly_shazwan%2C+flood14571%2C+AyalaSurit%2C+xzero8x%2C+Highwind%2C+shish%2C+R_Kamidees%2C+petegidlund%2C+samuelraphael%2C+shilton81%2C+gman2005%2C+cyan%2C+hpnus%2C+steelytom%2C+pavskies%2C+TrackGoesBy%2C+onthetown%2C+oliverpattison%2C&usernames=Prkl%2c+Iai%2c+SoylentBlack%2c+Iamthegamer%2c+indiesoc%2c+indiefan13%2c+Tezcatlipoca%2c+jshopa%2c+StretchOutAndWait%2c+Mikey2000%2c+bundtcake%2c+warg2%2c+shish%2c+PsychicWar%2c+Uppanun%2cJE%5fAndrocoen%2c+%2c+lyla%2c+dannniel%2c+harrisped%2c+emptyflow%2c+eyeblueday%2c+shelterfromO%2c+BrokeAsAJoke%2c+idna%2c+builtonsquares%2c+Jdamen%2c+saturos%2c+esmoriak%2c+Fleurquin%2c+Hoya4Peace%2c+artists%5fonly%2c+melitene%2c+hoshiko2600%2c+seaswell%2c+hiker%2c+xploder%2c+trelly%5fshazwan%2c+flood14571%2c+AyalaSurit%2c+xzero8x%2c+Highwind%2c+shish%2c+R%5fKamidees%2c+petegidlund%2c+samuelraphael%2c+shilton81%2c+gman2005%2c+cyan%2c+hpnus%2c+steelytom%2c+pavskies%2c+TrackGoesBy%2c+onthetown%2c+oliverpattison%2c')
indiePage

electronicPage <- pageAsDf('https://rateyourmusic.com/find/userchart?usernames=QuartzM386%2C+Pinko%2CElvenraad%2Cdmtls%2COtherLeadingBrand%2CTorc%2Cpixiesfanyo%2Cvallinder%2Ctissefyren%2Cptscl%2Cauntler%2Ckbrooks0o0%2Cpaltsug%2Cgafnerostow%2CDr_Keloid%2Cartaudie%2CVintageEyes%2Cechoinggrove%2CCBarlo%2Csun_st%2Cillegible%2Cwill89%2Cnervenet%2Cdarsu%2CFlood%2Cmowgli%2CmarkandeaZang%2C+ZED%2C+NatBateman%2C+Cthulhu%2C+Crumpet%2C+jauly%2C+zang%2C+ignatius%2C+hepiladron%2C+Zombie55%2C+jlebre%2C+Telekon%2C+boomerang%2C+spiritual%2C+zerb%2C+weaver%2C+Zebra%2C+warg2%2C&usernames=QuartzM386%2c+Pinko%2cElvenraad%2cdmtls%2cOtherLeadingBrand%2cTorc%2cpixiesfanyo%2cvallinder%2ctissefyren%2cptscl%2cauntler%2ckbrooks0o0%2cpaltsug%2cgafnerostow%2cDr%5fKeloid%2cartaudie%2cVintageEyes%2cechoinggrove%2cCBarlo%2csun%5fst%2cillegible%2cwill89%2cnervenet%2cdarsu%2cFlood%2cmowgli%2cmarkandeaZang%2c+ZED%2c+NatBateman%2c+Cthulhu%2c+Crumpet%2c+jauly%2c+zang%2c+ignatius%2c+hepiladron%2c+Zombie55%2c+jlebre%2c+Telekon%2c+boomerang%2c+spiritual%2c+zerb%2c+weaver%2c+Zebra%2c+warg2%2c')
electronicPage

rapPage <- pageAsDf('https://rateyourmusic.com/find/userchart?usernames=diction%2C+Tre_Jay%2C+ChrisPC%2C+MSarabia1036%2C+Bogart45%2C+fo_shizzle65%2C+Powelldinho%2C+Sh0wWhuT%2C+Trackmaster%2C+Kevindagame%2C+Kingpin96%2C&usernames=diction%2c+Tre%5fJay%2c+ChrisPC%2c+MSarabia1036%2c+Bogart45%2c+fo%5fshizzle65%2c+Powelldinho%2c+Sh0wWhuT%2c+Trackmaster%2c+Kevindagame%2c+Kingpin96%2c')
rapPage

popPage <- pageAsDf('https://rateyourmusic.com/find/userchart?usernames=Chuckyk18%2C+karen_uk%2C+sassymag%2C+coldplayfan1%2C+metclub2003%2C+HalynInReverie%2C+sarita%2C+lyssigrl%2C+themister00%2C+ferr0x%20music%2C+jds2110%2C+BeachTaco%2C+atidnaloy%2C+runnerdean%2C+Baybeegurl02%2C+cunnilingus%2C+big80slover%2C+missy9%2C+littlebobby%2C+Bushy%2C+teresa14127%2C+Ronnie%2C+rufieo27us%2C+jpete%2C+Honey11682%2C+backstreetboys%2C+lb1xxxx%2C+sita%2C+Bushy%2C+MRCD8%2C+Chibi9000%2C+kylie_x%2C+Frusciante%2C+anniebarlow%2C+Barbarella81%2C+mhshuvo%2C+crazycat%2C+lori_marie197%2C&usernames=Chuckyk18%2c+karen%5fuk%2c+sassymag%2c+coldplayfan1%2c+metclub2003%2c+HalynInReverie%2c+sarita%2c+lyssigrl%2c+themister00%2c+ferr0x+music%2c+jds2110%2c+BeachTaco%2c+atidnaloy%2c+runnerdean%2c+Baybeegurl02%2c+cunnilingus%2c+big80slover%2c+missy9%2c+littlebobby%2c+Bushy%2c+teresa14127%2c+Ronnie%2c+rufieo27us%2c+jpete%2c+Honey11682%2c+backstreetboys%2c+lb1xxxx%2c+sita%2c+Bushy%2c+MRCD8%2c+Chibi9000%2c+kylie%5fx%2c+Frusciante%2c+anniebarlow%2c+Barbarella81%2c+mhshuvo%2c+crazycat%2c+lori%5fmarie197%2c')
popPage

metalPage <- pageAsDf('http://rateyourmusic.com/find/userchart?usernames=Abuyss%2C+Alberick%2C+Alu_chan%2C+ArneZ%2C+asperity666%2C+Asphyxiation%2C+Celticknife%2C+chris83%2C+DarkFox%2C+Deathmetal_h%2C+DTJesus%2C+evilbryan%2C+Flippy%2C+Garand%2C+goatlipss%2C+gojira%2C+grinders%2C+itchytasty%2C+jigaboo1488%2C+JonFox%2C+Katmandu%2C+Mechanix%2C+metachoz%2C+MetalHed%2C+MetalSoul%2C+nacho220%2C+Numinous666%2C+PassionImmortal%2C+piepants%2C+Quezacotl21%2C+r0ckmys0x%2C+rectal_anarc%2C+rosdaw%2C+sim_maiden%2C+SlavikCC%2C+Stee_monkey%2C+stradicaster%2C+Tarquinius%2C+ThorUK3%2C+totalmetal%2C+VASTDEFERENS%2C+Venom5000%2C+Wogbog%2C+Vitus_Freak%2C+Miyuko%2C+shuli%2C+Demonic666%2C+LiAM2K5%2C+Ozmossis%2C+StoneSour86%2C+natkaos%2C+Madbutcher%2C+MetalHead84%2C+Offman%2C+Belphegor%2C+Intervention%2C+Horfixion%2C+Kensix%2C+Traveler%2C+MarkKonstans%2C+gorection%2C+Underaker%2C+rimfaxe67%2C+wicked0ne%2C+jack_the_ripoff%2C+jwh88%2C+dirk.%2C+DEATHMETALTYLER%2C+The_Dystopia%2C+bowelseater%2C+Snowblind%2C+budap00%2C+Kensix%2C+AD720%2C+proskater7%2C+eestwahn%2C+ironpriest66%2C+extreme_metalhead%2C+Pwntendo%2C+deathomatic%2C+bobbydrake%2C+RotGut%2C+MetalHead666%2C+Neslepaks%2C+Aldersig%2C+samiam667%2C+metalhead101%2C+Dago%2C+falseprofit%2C+dnlg%2C+orphx%2C+titusfox%2C+mcwicked%2C+JANISSARY%2C+mox%2C+naginalJJ%2C+caexar%2C+Brant_Villan%2C')
metalPage

progPage <- pageAsDf('http://rateyourmusic.com/find/userchart?usernames=groonrikk%2C+holerj%2C+grendel71%2C+Mategra%2C+PhilZ%2C+Proghead72%2C+Sandrose%2C+hierophant%2C+DTJesus%2C')
progPage

soulPage <- pageAsDf('http://rateyourmusic.com/find/userchart?usernames=Intheway%2C+BRG%2C+chrismass61%2C+Jock%2C+gnirob%2C+bruklover%2C+KOHA%2C+jkra3168%2C+silverjew')
soulPage

grungePage <- pageAsDf('http://rateyourmusic.com/find/userchart?usernames=goregirl%2C+multiverse%2C+ean%2C+Dancon7%2C+jederin%2C+_jmc_%2C+Falkman%2C+JackL%2C+nina3115%2C+chili_pepper%2C+monkeychunx%2C+Bloodaxe%2C+dustyhands%2C+Ceeker%2C+Spoonman%2C+oceanic%2C+randakk%2C+brainofj72%2C+CodeNemesis%2C+greyninjax%2C+quixote%2C+lennydubber%2C+pearljam1990%2C+denisobreja%2C+svarog86%2C+whiplash1980%2C+Worstcritic%2C+Lonniemyidol%2C+norain%2C+PsychicWar%2C+fasterdisco%2C+pearljammer9%2C+Pearl4ever%2C+taj%2C+Kensnroses%2C+mr1879%2C+skyrat%2C+RangerX%2C+stavanger%2C+Superunknown0101%2C+fluentchaos%2C')
grungePage

fullSet <- rbind(grungePage,soulPage,progPage,metalPage,popPage,rapPage, electronicPage, indiePage, jazzPage)
fullSet <- fullSet[,c('Artist','Album')]


#write.csv(fullSet, file = '/Users/chase/Desktop/RProjects/ds202_project/genreData')


#fullSet <- read.csv('/Users/chase/Desktop/RProjects/ds202_project/genre_data', )
#fullSet <- fullSet[,c('Artist','Album')]
str(fullSet)


spotifyInfo <- read.csv('./Top_100_by_fans.csv', header = TRUE)
str(spotifyInfo)
spotifyInfo




