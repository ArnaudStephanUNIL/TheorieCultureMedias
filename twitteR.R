library(twitteR)
library(ROAuth)
library(stringr)
library(tidyr)
library(magrittr)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

options(digits=1)
setup_twitter_oauth("2oBeh5pzqXHFNNcfCITXo2e3J", "Mas8sVej4so0oOaSg1TNXfvNgZuzaGOBA2SpeDUBTD8rOJLKt2",
                    "3115642545-wPfP3XRojlvQOyr4jy5gXDpQtcrvxjT2DI4ggMU", "SJ40K4ayEFWjXVGNpwzvmEQw28ObrL8TKXlj73TtSz3cc")

#On prend 15.000 tweets concernant #BalanceTonPorc
tweets15k <- searchTwitter("#BalanceTonPorc",n=15000)
df_15k <- twListToDF(strip_retweets(tweets15k,strip_manual = T,strip_mt = T))

#On prend 2.500 tweets du 13 au 16 octobre, sans les RT
bpt_tweets_1 <- searchTwitter("#BalanceTonPorc",n=2500,since="2017-10-13",until="2017-10-16")
df_13_16 <- twListToDF(strip_retweets(bpt_tweets_1, strip_manual=T, strip_mt=T))

#On prend 2.500 tweets du 16 au 19 octobre, sans les RT
bpt_tweets_2 <- searchTwitter("#BalanceTonPorc",n=2500,since="2017-10-16",until="2017-10-19")
df_16_19 <- twListToDF(strip_retweets(bpt_tweets_2, strip_manual=T, strip_mt=T))

################################################################
############### OPERATIONS SUR LE DF DU 13 AU 16 ###############
################################################################

#Nettoyage du dataframe
df_13_16 <- df_13_16[,c("text","favoriteCount","retweetCount")] #On ne garde que les colonnes qui nous intéressent
df_13_16$hashtags <- str_extract_all(df_13_16$text,"#\\S+") #On extrait tous les hashtags des textes
df_13_16$hashtags <- gsub("c\\(|\\)|\"", "", df_13_16$hashtags) #On enlève les "c(" et les ")" et les guillemets
df_13_16$hashtags <- gsub("[^,]*balancetonporc[^,]*", "#BalanceTonPorc", df_13_16$hashtags, ignore.case=TRUE) #On regroupe tout sous le même hashtag
df_13_16$hashtags <- gsub("[^,]*balancetatruie[^,]*", "#BalanceTaTruie", df_13_16$hashtags, ignore.case=TRUE) #Pareil pour balancetatruie
df_13_16$hashtags <- gsub("[^,]*metoo[^,]*", "#MeToo", df_13_16$hashtags, ignore.case=TRUE) #Pareil pour MeToo
df_13_16$hashtags <- gsub("[^,]*moiaussi[^,]*", "#MeToo", df_13_16$hashtags, ignore.case=TRUE)
df_13_16$hashtags <- gsub("\\s+", "", df_13_16$hashtags, ignore.case=TRUE) #Pareil pour MeToo
df_13_16 <- df_13_16[- grep("character\\(0", df_13_16$hashtags),] #On vire ce hashtag bizarre

length(which(df_13_16$hashtags == "#BalanceTonPorc")) #358 tweets sur 455 ne contiennent que le hashtag en question

#On regarde les hashtags qui reviennent le plus avec #BalanceTonPorc
freq_13_16 <- df_13_16$hashtags %>%
  str_split(",") %>%
  unlist %>%
  table %>%
  data.frame %>%
  arrange(-Freq)

#On enlève la ligne qui ne correspond à aucun hashtag
freq_13_16 <- freq_13_16[-2,]
names(freq_13_16) <- c("Hashtag","Frequence")
#On calcule la fréquence en %age des hashtags totaux
freq_13_16$Frequence <- (freq_13_16$Frequence/max(freq_13_16$Freq))*100
#Et enfin on enlève la 1e ligne
freq_13_16 <- freq_13_16[-1,]

#Et on fait un bel histogramme
ggplot(data = freq_13_16[1:15,],aes(x=Hashtag,y=Frequence))+
  geom_bar(stat="identity",fill="#9dd0e1",width=0.5,color="black",size=0.4) +
  ylab("Fréquence, en %") +
  xlab("") +
  theme(text=element_text(size=15))

################################################################
############### OPERATIONS SUR LE DF DU 16 AU 19 ###############
################################################################

#Nettoyage du dataframe
df_16_19 <- df_16_19[,c("text","favoriteCount","retweetCount")] #On ne garde que les colonnes qui nous intéressent
df_16_19$hashtags <- str_extract_all(df_16_19$text,"#\\S+") #On extrait tous les hashtags des textes
df_16_19$hashtags <- gsub("c\\(|\\)|\"", "", df_16_19$hashtags) #On enlève les "c(" et les ")" et les guillemets
df_16_19$hashtags <- gsub("[^,]*balancetonporc[^,]*", "#BalanceTonPorc", df_16_19$hashtags, ignore.case=TRUE) #On regroupe tout sous le même hashtag
df_16_19$hashtags <- gsub("[^,]*balancetatruie[^,]*", "#BalanceTaTruie", df_16_19$hashtags, ignore.case=TRUE) #Pareil pour balancetatruie
df_16_19$hashtags <- gsub("[^,]*metoo[^,]*", "#MeToo", df_16_19$hashtags, ignore.case=TRUE) #Pareil pour MeToo
df_16_19$hashtags <- gsub("[^,]*moiaussi[^,]*", "#MeToo", df_16_19$hashtags, ignore.case=TRUE)
df_16_19$hashtags <- gsub("\\s+", "", df_16_19$hashtags, ignore.case=TRUE) #Pareil pour MeToo
df_16_19 <- df_16_19[- grep("character\\(0", df_16_19$hashtags),] #On vire ce hashtag bizarre

length(which(df_16_19$hashtags == "#BalanceTonPorc")) #257 tweets sur 455 ne contiennent que le hashtag en question

#On regarde les hashtags qui reviennent le plus avec #BalanceTonPorc
freq_16_19 <- df_16_19$hashtags %>%
  str_split(",") %>%
  unlist %>%
  table %>%
  data.frame %>%
  arrange(-Freq)

#On enlève la ligne qui ne correspond à aucun hashtag
freq_16_19 <- freq_16_19[-3,]
names(freq_16_19) <- c("Hashtag","Frequence")
#On calcule la fréquence en %age des hashtags totaux
freq_16_19$Frequence <- (freq_16_19$Frequence/max(freq_16_19$Freq))*100
#Et enfin on enlève la 1e ligne
freq_16_19 <- freq_16_19[-1,]

#Et on fait un bel histogramme
ggplot(data = freq_16_19[1:15,],aes(x=Hashtag,y=Frequence))+
  geom_bar(stat="identity",fill="#9dd0e1",width=0.5,color="black",size=0.4) +
  ylab("Fréquence, en %") +
  xlab("") +
  theme(text=element_text(size=15))

###################
### NE PAS ECRIRE LA DESSOUS C'EST JUSTE DU CODE D'EXEMPLE
###################

r_tweets <- searchTwitter("#rstats", n=300)
sources <- sapply(r_tweets, function(x) x$getStatusSource()) #On prend la colonne StatusSource
sources <- gsub("</a>", "", sources) #On remplace la balise de fermeture par rien du tout (en gros on la vire)
sources <- strsplit(sources, ">") #On coupe la source en deux au niveau du ">"
sources <- sapply(sources, function(x) ifelse(length(x) > 1, x[2], x[1])) #J'ai pas compris mais en gros ça ne prend que la partie qu'on veut
source_table = table(sources) #On transforme ça en table
pie(source_table[source_table > 10])
source_table
