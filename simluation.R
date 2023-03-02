#monte carlo simulation of war (card game)
library(sets)

#PARAMS
playerCount <- 2

countGames <- vector()

for (x in 1:10000) {
  print(c("Almost there! Count is ", x))
  shoe <- vector()
  for(i in 1:13){
    shoe=append(shoe, c(paste("H",i,sep=""),paste("C",i,sep=""),paste("S",i,sep=""),paste("D",i,sep="")))
  }
  
  #more logic needed if 52%playercount != 0
  deck1 <- sample(shoe, 52/playerCount)
  #remove drawn cards f
  shoe = shoe[! shoe %in% deck1]
  deck2 <- sample(shoe, 52/playerCount)
  shoe = shoe[! shoe %in% deck2]
  #war starts
  won=FALSE
  count <- 0
  popd1 <- vector()
  popd2 <- vector()
  while(!won){
    count=count+1
    
    deck1 = sample(deck1)
    deck2 = sample(deck2)
    
    popd1 = append(popd1, deck1[1])
    deck1 = deck1[-1]
    
    popd2 = append(popd2, deck2[1])
    deck2 = deck2[-1]
    # cat(c("Deck1 score: ", length(deck1),"\n"))
    # cat(c("Deck2 score: ", length(deck2),"\n"))
    # print(c("Popd1 is ", popd1))
    # print(c("Popd2 is ", popd2))
    # print(as.integer(substr(tail(popd1,n=1),2,nchar(tail(popd1,n=1))))>as.integer(substr(tail(popd2,n=1),2,nchar(tail(popd2,n=1)))))
    if(as.integer(substr(tail(popd1,n=1),2,nchar(tail(popd1,n=1))))>as.integer(substr(tail(popd2,n=1),2,nchar(tail(popd2,n=1))))){
      #deck 1 wins
      # print("P1 won!")
      deck1=append(deck1,c(popd1,popd2))
      popd1 = vector()
      popd2 = vector()
    }else if(as.integer(substr(tail(popd1,n=1),2,nchar(tail(popd1,n=1))))<as.integer(substr(tail(popd2,n=1),2,nchar(tail(popd2,n=1))))){
      #deck 2 wins
      # print("P2 won!")
      deck2=append(deck2,c(popd1,popd2))
      popd1 = vector()
      popd2 = vector()
    }else{
      #tie
      # print(c("Popd1 is ", popd1))
      # print(c("Popd2 is ", popd2))
      # print("WAR")
      
    }
    # cat(c("Deck1 score: ", length(deck1),"\n"))
    # cat(c("Deck2 score: ", length(deck2),"\n"))
    if (length(deck1) > 50 || length(deck2) > 50) {
      # cat(c("Deck1 score: ", length(deck1),"\n"))
      # cat(c("Deck2 score: ", length(deck2),"\n"))
    }
    if(length(deck1)==0 || length(deck2)==0 || count==10000){
      won=TRUE
      # print("WE HAVE A WINNER")
      print(c("Count: ", count))
      countGames = append(countGames, count)
    }
  }
}

hist(countGames,breaks=100)
mean(countGames)
sd(countGames)
