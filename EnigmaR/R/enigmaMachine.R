#---------------------------#
#                           #
#  ORIGINAL ENIGMA MACHINE  #
#       Emma Sims 2020      #
#                           #
#---------------------------#

#   MAIN

enigmaMachine <- function(message, rotorOrder, reflector, plugConnections, groundSetting, Trigram){

  if(missing(message)){         stop("No Message to Encode.") }
  if(missing(rotorOrder)){      rotorOrder = c(1,2,3)         }
  if(missing(reflector)){       reflector = "UKW-A"           }
  if(missing(groundSetting)){   groundSetting = c("A","A","A")}
  if(missing(Trigram)){         Trigram = c("A","A","A")      }
  if(missing(plugConnections)){ plugConnections = cbind(c("A","B","C","D","E","F"),c("Z","Y","X","W","V","U"))}

  if(class(message)         != "character"){stop("message is not of character class."      )}
  if(class(rotorOrder)      != "numeric"  ){stop("rotorOrder is not of numeric class."     )}
  if(class(reflector)       != "character"){stop("reflector is not of character class."    )}
  if(class(plugConnections) != "matrix"   ){stop("plugConnections is not of matrix class." )}
  if(class(groundSetting)   != "character"){stop("groundSetting is not of character class.")}
  if(class(Trigram)         != "character"){stop("Trigram is not of character class."      )}

  if(length(rotorOrder)      != 3){stop("rotorOrder must be length: 3"     )}
  if(length(groundSetting)   != 3){stop("groundSetting must be length: 3"  )}
  if(length(Trigram)         != 3){stop("Trigram must be length: 3"        )}
  if((nrow(plugConnections)  != 6) || (ncol(plugConnections)   != 2)){stop("plugConnections must have nrow: 6 and ncol: 2")}

  cat(paste("\n---------------\n    CONFIG\n---------------\nMessage:          ", message))

  message         = toupper(message);             message = gsub(" ","", message)
  groundSetting   = toupper(groundSetting)
  Trigram         = toupper(Trigram)
  plugConnections = toupper(plugConnections)
  message         = unlist(strsplit(message, ""))

  REFLECTORS = c("UKW-A","UKW-B","UKW-C")
  ALPHA <- unlist(strsplit(c("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), ""))
  if((max(rotorOrder) > 5) || min(rotorOrder) < 1){  stop("\nrotorOrder range is between 1 and 5.")}
  if(!all(is.element(groundSetting, ALPHA))){        stop("\ngroundSetting must contain letters A-Z only.")}
  if(!all(is.element(message, ALPHA))){              stop("\nmessage must contain letters A-Z only.")}
  if(!all(is.element(Trigram, ALPHA))){              stop("\nTrigram must contain letters A-Z only.")}
  if(!all(is.element(plugConnections, ALPHA))){      stop("\nplugConnections must contain letters A-Z only.")}
  if(!any(is.element(reflector, REFLECTORS))){       stop("\nreflector is not any of: UKW-A, UKW-B, UKW-C.")}

  #Config-Messages
  cat(paste("\nRotor Order:      ", rotorOrder[1], "-", rotorOrder[2], "-", rotorOrder[3]))
  cat(paste("\nPlug Connections: ", plugConnections[1,1], plugConnections[1,2], " ", plugConnections[2,1], plugConnections[2,2], " ", plugConnections[3,1], plugConnections[3,2], " ", plugConnections[4,1], plugConnections[4,2], " ", plugConnections[5,1], plugConnections[5,2], " ", plugConnections[6,1], plugConnections[6,2]))
  cat(paste("\nGround Setting:   ", groundSetting[1], "-", groundSetting[2], "-", groundSetting[3]))
  cat(paste("\nTrigram:          ", Trigram[1], Trigram[2], Trigram[3]))
  cat("\n---------------\n")

  #Machine Setup
  configM  <- configMachine(rotorOrder)
  trips    <- unlist(configM$trip)
  cogs     <- configM$cogs
  cogs_    <- configM$cogs_
  I        <- shiftState(cogs[,1],  (match(groundSetting[3], ALPHA)) - 1) #Right Cog - Fast Cog
  II       <- shiftState(cogs[,2],  (match(groundSetting[2], ALPHA)) - 1) #Middle Cog
  III      <- shiftState(cogs[,3],  (match(groundSetting[1], ALPHA)) - 1) #Left Cog  - Slow Cog
  I_       <- shiftState(cogs_[[1]], (match(groundSetting[3], ALPHA)) - 1) #Right Cog - Fast Cog
  II_      <- shiftState(cogs_[[2]], (match(groundSetting[2], ALPHA)) - 1) #Middle Cog
  III_     <- shiftState(cogs_[[3]], (match(groundSetting[1], ALPHA)) - 1) #Left Cog  - Slow Cog

  steckers <- cbind(c(plugConnections[,1], plugConnections[,2]),c(plugConnections[,2], plugConnections[,1]))


  cat("\nMESSAGE: ")
  #Encoding Loop
  for(i in 1:length(message)){

    #Plugboard
    letter = message[i]
    if(any(is.element(letter, steckers[,1]))){
      letter = steckers[which(steckers[,1] %in% letter),2]
    }

    letter_ind = which(ALPHA %in% letter)
    #Right Cog
    letter_ind = letter_ind + I[letter_ind];    if(letter_ind < 1){letter_ind = 26+letter_ind}; if(letter_ind > 26){letter_ind = letter_ind - 26};
    #Middle Cog
    letter_ind = letter_ind + II[letter_ind];   if(letter_ind < 1){letter_ind = 26+letter_ind}; if(letter_ind > 26){letter_ind = letter_ind - 26};
    #Left Cog
    letter_ind = letter_ind + III[letter_ind];  if(letter_ind < 1){letter_ind = 26+letter_ind}; if(letter_ind > 26){letter_ind = letter_ind - 26};

    #Reflector
    letter = reflectLetter(ALPHA[letter_ind],reflector)
    letter_ind = which(ALPHA %in% letter)

    #Left Cog
    letter_ind = letter_ind - III[which(III_ %in% letter)]; letter = ALPHA[letter_ind]
    #Middle Cog
    letter_ind = letter_ind - II[which(II_ %in% letter)];   letter = ALPHA[letter_ind]
    #Right Cog
    letter_ind = letter_ind - I[which(I_ %in% letter)];     letter = ALPHA[letter_ind]


    #Rotor Shifts
    if(ALPHA[II[1]] == trips[2]){III  = shiftState(III,1); III_ = shiftState(III_,1)}
    suppressWarnings(if(ALPHA[I[1]]  == trips[3]){II   = shiftState(II,1); II_ = shiftState(II_,1)})
    I = shiftState(I,1); I_ = shiftState(I_,1)


    cat(letter)
  }


  return(cat("\nFINISHED"))



}
