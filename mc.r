
nazwiska <- c("Ethan", "Epthan", "Ethn")

nazw <- data.frame(nazwiska = nazwiska, kod = rep(NA, 3))


mc.del <- function(word){
  l <- nchar(word)
  i <- floor(runif(1, 0, l) + 1)
  
  if (i == 1) {
    str <- substring(word, 2, l)
  } else if (i == l){
    str <- substring(word, 1, l-1)
  } else {
    str <- paste(substring(word, 1, i-1), substring(word, i+1, l), sep = "")
  }
  
  return(str)
}

mc.ins <- function(word){
  l <- nchar(word)
  i <- floor(runif(1, 0, l) + 1)
  n <- letters[runif(1, 0, 26) + 1]
  
  if (i == 1) {
    str <- paste(n, word, sep = "")
  } else if (i == l){
    str <- paste(word, n, sep = "")
  } else {
    str <- paste(substring(word, 1, i-1), n, substring(word, i, l), sep = "")
  }
  
  return(str)
}

mc.rep <- function(word){
  l <- nchar(word)
  i <- floor(runif(1, 0, l) + 1)
  n <- letters[runif(1, 0, 26) + 1]
  
  if (i == 1) {
    str <- paste(n, substring(word, 2, l), sep = "")
  } else if (i == l){
    str <- paste(substring(word, 1, l-1), n, sep = "")
  } else {
    str <- paste(substring(word, 1, i-1), n, substring(word, i+1, l), sep = "")
  }
  
  return(str)
}



mc.code <- function(word = word, operations = 10){
  new_word <- numeric(3*operations)
  for (i in 1:operations){
    new_word[i] <- mc.del(word)
    new_word[i+operations] <- mc.ins(word)
    new_word[i+2*operations] <- mc.rep(word)
  }
  l <- nchar(word)
  code <- vector("list", l)
  if (l %% 2 == 0){
    for (i in 1:(l/2)){
      code[[i]] <- append(code[[i]], substring(new_word, i, i))
    }
    j <- 0
    for (i in l:(l/2)){
      code[[i]] <- append(code[[i]], substring(new_word, nchar(new_word)-j, nchar(new_word)-j))
      j <- j+1
    }
    new_word_mid <- new_word[operations:2*operations]
    j <- ceiling(nchar(new_word_mid)[1]/2)
    code[[l/2]] <- append(code[[l/2]], substring(new_word_mid, j, j))
    code[[(l/2)+1]] <- append(code[[(l/2)+1]], substring(new_word_mid, j, j))
  } else {
    for (i in 1:floor(l/2)){
      code[[i]] <- append(code[[i]], substring(new_word, i, i))
    }
    j <- 0
    for (i in l:(ceiling(l/2)+1)){
      code[[i]] <- append(code[[i]], substring(new_word, nchar(new_word)-j, nchar(new_word)-j))
      j <- j+1
    }
    new_word_mid <- new_word[operations:(2*operations)]
    j <- nchar(new_word_mid)[1]/2
    code[[floor(l/2)]] <- append(code[[floor(l/2)]], substring(new_word_mid, j, j))
    code[[ceiling(l/2)]] <- append(code[[ceiling(l/2)]], substring(new_word_mid, j+1, j+1))
  }
  
  code <- lapply(lapply(code, table), which.max)
  code_new <- character(0)
  for (i in 1:length(code)){
    code_new <- paste(code_new, names(code[[i]]), sep = "")
  }
  return(list(code_new=code_new, new_word=new_word))
}

mc.code("Etan")



