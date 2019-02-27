# Materiel a charger ----

library(tidyverse)

enigma.env <- new.env()

tableau <- array(data = 1:26, dim = 26, dimnames = list(letters))

ntl <- function(number){
  return(names(tableau)[number])
}

ltn <- function(letter){
  return(tableau[letter])
}

preparation <- function(text){
  
  text <- unlist(strsplit(text, NULL))
  indices <- grep("[a-z]", text, ignore.case = TRUE, perl = TRUE)
  text <- text[indices]
  
  for(j in 1:26){
    text <- gsub(LETTERS[j], letters[j], text)
  }
  return(text)
}

association <- function(text){
  
  stock <- vector(mode = "integer", length = length(text))
  for(k in 1:length(text)){
    stock[k] <- ltn(text[k])
  }
  return(stock)
}

modulo <- function(int1, int2){
  if(int1 + int2 > 26){
    return(int1 + int2 - 26)
  }else{
    return(int1 + int2)
  }
}

Plugboard <- vector(mode = "list", length = 10)
set.Plugboard <- function(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                          arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20){
  pool <- c(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
            arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
  for(i in 1:10){
    Plugboard[[i]][1] <- pool[2*i-1]
    Plugboard[[i]][2] <- pool[2*i]
  }
  return(Plugboard)
}

forward.3 <- function(input){
  output <- modulo(input, Rotor3[[modulo(input, enigma.env$pos.rotor3 - 1)]][2])
  return(output)
}

forward.2 <- function(input){
  output <- modulo(input, Rotor2[[modulo(input, enigma.env$pos.rotor2 - 1)]][2])
  return(output)
}

forward.1 <- function(input){
  output <- modulo(input, Rotor1[[modulo(input, enigma.env$pos.rotor1 - 1)]][2])
  return(output)
}

reflect <- function(input){
  output <- modulo(input, Reflector[[input]][2])
  return(output)
}

backward.1 <- function(input){
  output <- modulo(input, Rotorback1[[modulo(input, enigma.env$pos.rotor1 - 1)]][2])
  return(output)
}

backward.2 <- function(input){
  output <- modulo(input, Rotorback2[[modulo(input, enigma.env$pos.rotor2 - 1)]][2])
  return(output)
}

backward.3 <- function(input){
  output <- modulo(input, Rotorback3[[modulo(input, enigma.env$pos.rotor3 - 1)]][2])
  return(output)
}

Plugboard.swap <- function(input){
  input <- as.character(input)
  flag <- FALSE
  for(i in 1:10){
    if(input %in% enigma.env$Plugboard[[i]][1]){
      box <- enigma.env$Plugboard[[i]][2]
      flag <- TRUE
      break
    }
    if(input %in% enigma.env$Plugboard[[i]][2]){
      box <- enigma.env$Plugboard[[i]][1]
      flag <- TRUE
      break
    }
  }
  if(flag){
    output <- box
  }
  output <- if(flag){as.integer(output)} else{as.integer(input)}
  return(output)
}

translate <- function(input){
  output <- input %>% Plugboard.swap() %>% forward.3() %>% forward.2() %>% forward.1() %>%
    reflect() %>% backward.1() %>% backward.2() %>% backward.3() %>% Plugboard.swap()
  return(output)
}

Rotor1 <- vector(mode = "list", length = 26)
Rotor2 <- vector(mode = "list", length = 26)
Rotor3 <- vector(mode = "list", length = 26)

stock <- vector(mode = "list", length = 3)
stock[[1]] <- association(preparation("BDFHJLCPRTXVZNYEIWGAKMUSQO"))
stock[[2]] <- association(preparation("AJDKSIRUXBLHWTMCQGZNPYFVOE"))
stock[[3]] <- association(preparation("EKMFLGDQVZNTOWYHXUSPAIBRCJ"))

for(i in 1:26){
  Rotor1[[i]] <- c(i, if(stock[[1]][i] - i > 0){stock[[1]][i] - i}
                   else{26 - i + stock[[1]][i]})
}
for(i in 1:26){
  Rotor2[[i]] <- c(i, if(stock[[2]][i] - i > 0){stock[[2]][i] - i}
                   else{26 - i + stock[[2]][i]})
}
for(i in 1:26){
  Rotor3[[i]] <- c(i, if(stock[[3]][i] - i > 0){stock[[3]][i] - i}
                   else{26 - i + stock[[3]][i]})
}

Rotorback1 <- vector(mode = "list", length = 26)
Rotorback2 <- vector(mode = "list", length = 26)
Rotorback3 <- vector(mode = "list", length = 26)

for(i in 1:26){
  Rotorback1[[i]] <- c(i, if(order(stock[[1]])[i] - i > 0){order(stock[[1]])[i] - i}
                       else{26 - i + order(stock[[1]])[i]})
}
for(i in 1:26){
  Rotorback2[[i]] <- c(i, if(order(stock[[2]])[i] - i > 0){order(stock[[2]])[i] - i}
                       else{26 - i + order(stock[[2]])[i]})
}
for(i in 1:26){
  Rotorback3[[i]] <- c(i, if(order(stock[[3]])[i] - i > 0){order(stock[[3]])[i] - i}
                       else{26 - i + order(stock[[3]])[i]})
}

Reflector <- vector(mode = "list", length = 26)
hold <- association(preparation("EJMZALYXVBWFCRQUONTSPIKHGD"))
for(i in 1:26){
  Reflector[[i]] <- c(i, if(hold[i] - i > 0){hold[i] - i} 
                      else{26 - i + hold[i]})
}

rm(stock, hold, i)

# Fonction Enigma ----

Enigma <- function(plaintext, pos.rotor1, pos.rotor2, pos.rotor3, arg1, arg2, arg3, arg4, arg5,
                   arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16,
                   arg17, arg18, arg19, arg20){
  
  assign("pos.rotor1", pos.rotor1, envir = enigma.env)
  assign("pos.rotor2", pos.rotor2, envir = enigma.env)
  assign("pos.rotor3", pos.rotor3, envir = enigma.env)
  
  Plugboard <- set.Plugboard(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
                             arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
  assign("Plugboard", Plugboard, envir = enigma.env)
  
  text <- association(preparation(plaintext))
  cyphertext <- vector(mode = "character", length = length(text))
  for(i in 1:length(text)){
    cyphertext[i] <- translate(text[i])
    enigma.env$pos.rotor3 <- modulo(enigma.env$pos.rotor3, 1)
    if(enigma.env$pos.rotor3 == 23){
      enigma.env$pos.rotor2 <- modulo(enigma.env$pos.rotor2, 1)
      if(enigma.env$pos.rotor2 == 6){
        enigma.env$pos.rotor1 <- modulo(enigma.env$pos.rotor1, 1)
      }
    }
  }
  for(i in 1:length(cyphertext)){
    cyphertext[i] <- ntl(as.integer(cyphertext[i]))
  }
  cyphertext <- paste(cyphertext, collapse = "")
  print(cyphertext)
}
