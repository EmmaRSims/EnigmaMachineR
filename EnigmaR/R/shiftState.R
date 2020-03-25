#---------------------------#
#                           #
#  ORIGINAL ENIGMA MACHINE  #
#       Emma Sims 2020      #
#                           #
#---------------------------#

#   TURN COG

shiftState <- function(cog, n){
  if(n < 1){return(cog)}

  len = length(cog)

  for(i in 1:n){
    first = cog[1]
    temp = cog[-1]
    cog = c(temp, first)
  }

  return(cog)
}
