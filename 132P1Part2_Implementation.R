# Bit Embedding Code
embedBit <-function(bit, min, med, maxx){
  if (bit == 0) {
    delay_zero <- runif(1, min, med)
    return (delay_zero)
  } else {
    delay_one <- runif(1, med, maxx)
    return (delay_one)
  }
}

# Save identifying number for Exponential Distribution
exponential <-function(){
  return(0)
}
# Save identifying number for Uniform Distribution
uniform <- function() {
  return(1)
}

# Run trial
runTrial <- function(bufferSize, packetsInBuffer, messageSize, type) {
  
  # Custom Variables
  B = bufferSize # Buffer Size
  I = packetsInBuffer # Number of Packets already in Buffer
  M = messageSize # messageSize
  
  # Buffer Variables
  underflowDetected = FALSE
  overflowDetected = FALSE
  allowEnqueue = TRUE
  allowDequeue = TRUE
  buffer = c()
  
  # Clock Variables
  clock = 0
  done = FALSE
  delays = c()
  enqueue = FALSE
  dequeue = FALSE
  valueToEnqueue = 0
  
  # Select Distribution Type
  dist = rexp(n = 80, rate = 1)
  if (type == exponential()) {
    dist = rexp(n = 80, rate = 1)
  } else {
    dist = runif(n = 80, min=0, max=1) 
  }
  
  # Set min/median/max for data embedding
  minimum = min(dist)
  median = median(dist)
  max = max(dist)
  
  distTimeToEnqueue = 0
  distTimeToDequeue = 0
  message <- sample(0:1, M, replace = TRUE)
  
  # Preload Buffer
  for (i in 1:I) {
    buffer[length(buffer)+1] = dist[1]
    dist = dist[-1]
  }
  
  # Run trial until Message is fully encoded
  while (!done) {
    
    if (length(message) == 0) {
      done = TRUE
    } else {
      
      # Enqueue
      if (distTimeToEnqueue < clock && !done)  {
        # Check for Overflow
        if (length(buffer) == B) {
          overflowDetected = TRUE
        } else {
          # Enqueue to Buffer
          currentDelay = dist[1]
          dist = dist[-1]
          buffer[length(buffer) + 1] = currentDelay
          distTimeToEnqueue = clock + currentDelay
        }
      }
      
      # Dequeue
      if (distTimeToDequeue < clock && !done) {
        # Check for Underflow
        if (length(buffer) == 0) {
          underflowDetected = TRUE
        } else {
          # Dequeue from buffer and inject encoded Delay
          bufferDelay = buffer[1]
          buffer = buffer[-1]
          currentBit = message[1]
          message = message[-1]
          distTimeToDequeue = clock + embedBit(currentBit, minimum, median, max)
          
        }
      }
    }
    
    clock = clock + 0.01
  }
  
  # Output Result
  outcome = c()
  outcome[1] = 0
  outcome[2] = 0
  if (underflowDetected) {
    outcome[1] = 1
  }
  if (overflowDetected) {
    outcome[2] = 1
  }
  return(outcome)
  
}


# Run 1000 Trials
numTrials = 1000
preloadedB = c(2, 6, 10, 14, 18)
messageSizes = c(16, 32)
# Print Results
for (msize in messageSizes) {
  for (preloaded in preloadedB) {
    underflows = c()
    overflows = c()
    underflowsU = c()
    overflowsU = c()
    for (i in 1:numTrials) {
      # Buffer size, preloaded, message size
      outcome = runTrial(20, preloaded, msize , exponential())
      underflows[length(underflows)+1] = outcome[1]
      overflows[length(overflows)+1] = outcome[2]
      outcomeU = runTrial(20, preloaded, msize , uniform())
      underflowsU[length(underflowsU)+1] = outcomeU[1]
      overflowsU[length(overflowsU)+1] = outcomeU[2]
    }
    print((sprintf("i=%i, m=%i", preloaded, msize)))
    print(sprintf("Exponential: pOver= %.5f | pUnder= %.5f", length(overflows[overflows == 1]) / numTrials, length(underflows[underflows == 1]) / numTrials))
    print(sprintf("    Uniform: pOver= %.5f | pUnder= %.5f", length(overflowsU[overflowsU == 1]) / numTrials, length(underflowsU[underflowsU == 1]) / numTrials))
  }
}
