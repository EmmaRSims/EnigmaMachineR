#---------------------------#
#                           #
#  ORIGINAL ENIGMA MACHINE  #
#       Emma Sims 2020      #
#                           #
#---------------------------#

enigmaMachine <- function(message, rotorOrder, plugConnections, groundSetting, Trigram){
  
  if(missing(message)){         stop("No Message to Encode.") }
  if(missing(rotorOrder)){      rotorOrder = c(1,2,3)         }
  if(missing(groundSetting)){   groundSetting = c("A","A","A")}
  if(missing(Trigram)){         Trigram = c("A","A","A")      }
  if(missing(plugConnections)){ plugConnections = cbind(c("A","B","C","D","E","F"),c("Z","Y","X","W","V","U"))}
  
  if(class(message)         != "character"){stop("message is not of character class."      )}
  if(class(rotorOrder)      != "numeric"){stop("rotorOrder is not of numeric class."       )}
  if(class(plugConnections) != "matrix"){stop("plugConnections is not of matrix class."    )}
  if(class(groundSetting)   != "character"){stop("groundSetting is not of character class.")}
  if(class(Trigram)         != "character"){stop("Trigram is not of character class."      )}
  
  if(length(rotorOrder)      != 3){stop("rotorOrder must be length: 3"     )}
  if(length(plugConnections) != 6){stop("plugConnections must be length: 6")}
  if(length(groundSetting)   != 3){stop("groundSetting must be length: 3"  )}
  if(length(Trigram)         != 3){stop("Trigram must be length: 3"        )}
  
  #Config-Messages
  cat(paste("\n---------------\nCONFIG\n---------------\nMessage: ", message))
  cat(paste("\nRotor Order: ", rotorOrder[1], "-", rotorOrder[2], "-", rotorOrder[3]))
  cat(paste("\nPlug Connections: ", plugConnections[1,1], plugConnections[1,2], " ", plugConnections[2,1], plugConnections[2,2], " ", plugConnections[3,1], plugConnections[3,2], " ", plugConnections[4,1], plugConnections[4,2], " ", plugConnections[5,1], plugConnections[5,2], " ", plugConnections[6,1], plugConnections[6,2]))
  cat(paste("\nGround Setting: ", groundSetting[1], "-", groundSetting[2], "-", groundSetting[3]))
  cat(paste("\nTrigram: ", Trigram[1], Trigram[2], Trigram[3]))
}