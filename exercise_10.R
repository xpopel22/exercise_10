#setwd("V:/MPA-BTB/MPA-PRG/exercise_10")
#library("Biostrings")


SuffixArray <- function(seq){
  endSeq <- length(seq)
  listsuffix <- rep(as.character(seq), endSeq)
  suffixdf <- data.frame (
    offset = 1:endSeq,
    suffix = listsuffix
  )
  
  for (i in 1:endSeq){
    suffixdf[["suffix"]][i] <- as.character(seq[i:endSeq])
  }
  #print(suffixdf[order(suffixdf$suffix),])
  
  return(suffixdf[order(suffixdf$suffix),][["offset"]])
}

InverseSuffixArray <- function(SA){
  endArray <- length(SA)
  ISA <- rep(0,endArray)
  for (i in 1:endArray){
    ISA[i] <- which(SA == i)
  }
  return(ISA)
}

LCPArray <- function(text, SA, ISA){
  textEnd <- length(text)
  LCP <- rep(0, (textEnd + 1))
  LCP[1] <- 1
  LCP[(textEnd+1)] <- -1
  l <- 0
  for (i in 1:textEnd){
    j <- ISA[i]
    if (j > 1){
      k <- SA[(j - 1)]
      while (text[(k + l)] == text[(i + l)]){
        l <- l + 1
      }
      LCP[j] <- l
      l <- max((l - 1), 0)
    }
  }
  return(LCP)
}

BinarySearchSA <- function(pattern, text, SA){
  minIndex <- 0
  maxIndex <- length(text)
  print(maxIndex)
  endText <- maxIndex
  while (minIndex < maxIndex){
    print(text[SA[midIndex]:endText])
    midIndex <- floor(((minIndex+maxIndex)/2))
    if (as.character(pattern) <= as.character(text[SA[midIndex]:maxIndex])){
      maxIndex <- midIndex
    }else{
      minIndex <- midIndex + 1
    }
  }
  print(minIndex)
  firstidx <- minIndex
  maxIndex <- endText
  while (maxIndex > minIndex){
    midIndex <- floor(((minIndex+maxIndex)/2))
    print(text[SA[midIndex]:endText])
    if (as.character(text[SA[midIndex]:endText]) <= as.character(pattern)){
      minIndex <- midIndex + 1
    }else{
      maxIndex <- midIndex
    }
  }
  lastidx <- maxIndex - 1
  if (lastidx < firstidx){
    return("Pattern does not appear in text")
  }else{
    return(c(firstidx, lastidx))
  }
}

seq <- DNAString("CTAATAATG")
SA <- SuffixArray(seq)
ISA <- InverseSuffixArray(SA)
print(seq)
print(SA)
print(ISA)
LCPArray(seq,SA,ISA)
pattern <- DNAString("TG")
BinarySearchSA(pattern, seq, SuffixArray(seq))


