#---------------------------#
#                           #
#  ORIGINAL ENIGMA MACHINE  #
#       Emma Sims 2020      #
#                           #
#---------------------------#

#   REFLECTOR

reflectLetter <- function(letter, cog){
  UKW_A <- unlist(strsplit("EJMZALYXVBWFCRQUONTSPIKHGD",""))
  UKW_B <- unlist(strsplit("YRUHQSLDPXNGOKMIEBFZCWVJAT",""))
  UKW_C <- unlist(strsplit("FVPJIAOYEDRZXWGCTKUQSBNMHL",""))

  ALPHA <- unlist(strsplit(c("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), ""))

  if(     cog == "UKW-A"){letter = UKW_A[which(ALPHA %in% letter)]}
  else if(cog == "UKW-B"){letter = UKW_B[which(ALPHA %in% letter)]}
  else if(cog == "UKW-C"){letter = UKW_C[which(ALPHA %in% letter)]}
  else{stop("Reflector Cog Input Not Recognised. Must be one of: UKW-A, UKW-B, UKW-C")}

  return(letter)
}
