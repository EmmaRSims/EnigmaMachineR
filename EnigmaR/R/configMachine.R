#---------------------------#
#                           #
#  ORIGINAL ENIGMA MACHINE  #
#       Emma Sims 2020      #
#                           #
#---------------------------#

#   MACHINE SETUP

configMachine <- function(cogs, shiftAlphabet){

  #Rotors
  I     <- unlist(strsplit(c("EKMFLGDQVZNTOWYHXUSPAIBRCJ"), ""))
  II    <- unlist(strsplit(c("AJDKSIRUXBLHWTMCQGZNPYFVOE"), ""))
  III   <- unlist(strsplit(c("BDFHJLCPRTXVZNYEIWGAKMUSQO"), ""))
  IV    <- unlist(strsplit(c("ESOVPZJAYQUIRHXLNFTGKDCMWB"), ""))
  V     <- unlist(strsplit(c("VZBRGITYUPSDNHLXAWMJQOFECK"), ""))
  ROTORS <- list(I,II,III,IV,V)

  NOTCHES = c("Q","E","V","J","Z")

  ALPHA <- unlist(strsplit(c("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), ""))
  if(missing(shiftAlphabet)){shiftAlphabet = ALPHA}
  COGS  <- ROTORS[cogs]
  COGS_ <- matrix(0,nrow = 26, ncol = 3)

  for(i in 1:3){
    for(j in 1:26){
      COGS_[j,i] = match(COGS[[i]][j], shiftAlphabet) - j
    }
  }

  return(list("cogs" = COGS_, "cogs_" = COGS, "trip" = NOTCHES[cogs]))
}
