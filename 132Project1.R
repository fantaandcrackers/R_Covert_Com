asc <- function(x) {
  raw = charToRaw(x)
  return (strtoi(raw, 16L))
}
charToBinary <- function(ch) {
  asci = asc(ch)
  bin = as.integer(intToBits(asci))
  return (bin[8:1])
}
strToBinary <- function(str) {
  outputBin = c()
  for (i in 1:(nchar(str))) {
    ch = substr(str,i,i)
    outputBin = c(outputBin, charToBinary(ch))
  }
  return(outputBin)
}
encodeMessageDumb <- function(str) {
  binaryArray = strToBinary(str)
  #print(binaryArray)
  encodedArray = c()
  encodedDelay = c()
  total = 0
  for (i in 1:length(binaryArray)) {
    if (binaryArray[i] == 0) {
      total = total + 0.25
      encodedArray = c(encodedArray, total)
    } else {
      total = total + 0.75
      encodedArray = c(encodedArray, total)
    }
  }
  return (encodedArray)
}
encodeMessageSmart <- function(str, m, max) {
  binaryArray = strToBinary(str)
  #print(binaryArray)
  encodedArray = c()
  total = 0
  for (i in 1:length(binaryArray)) {
    if (binaryArray[i] == 0) {
      total = total + runif(1, 0, m)
      encodedArray = c(encodedArray, total)
    } else {
      total = total + runif(1, m, max)
      encodedArray = c(encodedArray, total)
    }
  }
  return (encodedArray)
}

delayHistogram <- function(data) {
  delay = c()
  for (i in 1:(length(data) - 1)) {
    delay[i] = data[i+1] - data[i]
  }
  hist(delay)
}

setwd("~/Dropbox/College/Junior Q1/132/Project 1")
original = read.csv("trafficData.csv", header = TRUE)

message = "this is a secret message"
dumb = encodeMessageDumb(message)
delayHistogram(dumb)
encodedCSVDumb = original
encodedCSVDumb[2:(length(dumb)+1),2] = dumb
# write.csv(encodedCSVDumb[0:length(dumb)+1, 1:2], file = "encodesdArrayDumb.csv", row.names = FALSE)

smart = encodeMessageSmart(message, 3, 12)
delayHistogram(smart)
encodedCSVSmart = original
encodedCSVSmart[2:(length(smart)+1),2] = smart
# write.csv(encodedCSVSmart[0:length(smart)+1, 1:2], file = "encodedArraySmart.csv", row.names = FALSE)

originalArray = original[1:192,2]
qqplot(originalArray, dumb, xlab = "Overt", ylab ="Covert", main = "Step 2.1 Step 6")
qqplot(originalArray, smart, xlab = "Overt", ylab ="Covert", main = "Step 2.1 Step 8")
# qqplot(dumb, smart, xlab = "Overt", ylab ="Covert", main = "Step 2.1 Step 5")

