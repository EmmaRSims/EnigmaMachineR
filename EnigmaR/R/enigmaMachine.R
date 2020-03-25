#---------------------------#
#                           #
#  ORIGINAL ENIGMA MACHINE  #
#       Emma Sims 2020      #
#                           #
#---------------------------#

#   MAIN

enigmaMachine <- function(message, rotorOrder, reflector, plugConnections, groundSetting){

  if(missing(message)){         stop("No Message to Encode.") }
  if(missing(rotorOrder)){      rotorOrder = c(1,2,3)         }
  if(missing(reflector)){       reflector = "UKW-A"           }
  if(missing(groundSetting)){   groundSetting = c("A","A","A")} #RML Format
  if(missing(plugConnections)){ plugConnections = cbind(c("A","B","C","D","E","F"),c("Z","Y","X","W","V","U"))}

  if(class(message)         != "character"){stop("message is not of character class."      )}
  if(class(rotorOrder)      != "numeric"  ){stop("rotorOrder is not of numeric class."     )}
  if(class(reflector)       != "character"){stop("reflector is not of character class."    )}
  if(class(plugConnections) != "matrix"   ){stop("plugConnections is not of matrix class." )}
  if(class(groundSetting)   != "character"){stop("groundSetting is not of character class.")}

  if(length(rotorOrder)      != 3){stop("rotorOrder must be length: 3"     )}
  if(length(groundSetting)   != 3){stop("groundSetting must be length: 3"  )}
  if((nrow(plugConnections)  != 6) || (ncol(plugConnections)   != 2)){stop("plugConnections must have nrow: 6 and ncol: 2")}

  cat(paste("\n---------------\n    CONFIG\n---------------\nMessage:                    ", message))

  white_space_ind = which(unlist(strsplit(message, "")) == " ")

  message         = toupper(message);             message = gsub(" ","", message)
  groundSetting   = toupper(groundSetting)
  plugConnections = toupper(plugConnections)
  message         = unlist(strsplit(message, ""))

  REFLECTORS = c("UKW-A","UKW-B","UKW-C")
  ALPHA <- unlist(strsplit(c("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), ""))
  if((max(rotorOrder) > 5) || min(rotorOrder) < 1){  stop("\nrotorOrder range is between 1 and 5.")}
  if(!all(is.element(groundSetting, ALPHA))){        stop("\ngroundSetting must contain letters A-Z only.")}
  if(!all(is.element(message, ALPHA))){              stop("\nmessage must contain letters A-Z only.")}
  if(!all(is.element(plugConnections, ALPHA))){      stop("\nplugConnections must contain letters A-Z only.")}
  if(!any(is.element(reflector, REFLECTORS))){       stop("\nreflector is not any of: UKW-A, UKW-B, UKW-C.")}

  #Config-Messages
  cat(paste("\nRotor Order (R - M - L):    ", rotorOrder[1], "-", rotorOrder[2], "-", rotorOrder[3]))
  cat(paste("\nPlug Connections:           ", plugConnections[1,1], plugConnections[1,2], " ", plugConnections[2,1], plugConnections[2,2], " ", plugConnections[3,1], plugConnections[3,2], " ", plugConnections[4,1], plugConnections[4,2], " ", plugConnections[5,1], plugConnections[5,2], " ", plugConnections[6,1], plugConnections[6,2]))
  cat(paste("\nGround Setting (R - M - L): ", groundSetting[1], "-", groundSetting[2], "-", groundSetting[3]))
  cat(      "\n-------------------------\n")

  #--- Machine Setup
  configM  <- configMachine(rotorOrder)
  trips    <- unlist(configM$trip) #RML Format
  cogs     <- configM$cogs
  cogs_    <- configM$cogs_
  #---Cog Shifts
  I        <- shiftState(cogs[,1],  (match(groundSetting[1],  cogs_[[1]]) - 1)) #Right Cog - Fast Cog
  II       <- shiftState(cogs[,2],  (match(groundSetting[2],  cogs_[[2]]) - 1)) #Middle Cog
  III      <- shiftState(cogs[,3],  (match(groundSetting[3],  cogs_[[3]]) - 1)) #Left Cog  - Slow Cog
  #---Cog Letters
  I_       <- shiftState(cogs_[[1]], (match(groundSetting[1], cogs_[[1]]) - 1)) #Right Cog - Fast Cog
  II_      <- shiftState(cogs_[[2]], (match(groundSetting[2], cogs_[[2]]) - 1)) #Middle Cog
  III_     <- shiftState(cogs_[[3]], (match(groundSetting[3], cogs_[[3]]) - 1)) #Left Cog  - Slow Cog
  #---Steckers
  steckers <- cbind(c(plugConnections[,1], plugConnections[,2]),c(plugConnections[,2], plugConnections[,1]))


  cat("\nMESSAGE: ")
  message_output = rep("A", length(message))
  #Encoding Loop
  for(i in 1:length(message)){

    #Plugboard
    letter = message[i]
    if(any(is.element(letter, steckers[,1]))){
      letter = steckers[which(steckers[,1] %in% letter),2]
    }
    letter_target = match(letter, ALPHA)


    ALL_LETTERS = ALPHA
    for(j in 1:26){
      letter_ind = j;
      letter_ind = letter_ind + I[letter_ind];   if(letter_ind > 26){letter_ind = letter_ind - 26}; if(letter_ind < 1){letter_ind = letter_ind + 26} ;
      letter_ind = letter_ind + II[letter_ind];  if(letter_ind > 26){letter_ind = letter_ind - 26}; if(letter_ind < 1){letter_ind = letter_ind + 26} ;
      letter_ind = letter_ind + III[letter_ind]; if(letter_ind > 26){letter_ind = letter_ind - 26}; if(letter_ind < 1){letter_ind = letter_ind + 26} ;
      ALL_LETTERS[j] = reflectLetter(ALPHA[letter_ind], reflector)
    }


    #Forward Rotors
    letter_ind = letter_target + I[letter_target];   if(letter_ind > 26){letter_ind = letter_ind - 26}; if(letter_ind < 1){letter_ind = letter_ind + 26} ;
    letter_ind = letter_ind + II[letter_ind];        if(letter_ind > 26){letter_ind = letter_ind - 26}; if(letter_ind < 1){letter_ind = letter_ind + 26} ;
    letter_ind = letter_ind + III[letter_ind];       if(letter_ind > 26){letter_ind = letter_ind - 26}; if(letter_ind < 1){letter_ind = letter_ind + 26} ;

    letter_ref = ALPHA[letter_ind]


    #Backwards Rotors
    letter_final = match(letter_ref, ALL_LETTERS)

    #Plugboard
    letter = ALPHA[letter_final]
    if(any(is.element(letter, steckers[,1]))){
      letter = steckers[which(steckers[,1] %in% letter),2]
    }
    letter_ind = match(letter, ALPHA)
    message_output[i] = ALPHA[letter_ind]


    #Double Step
    currentI = I_[1]; currentII = II_[1];
    I = shiftState(I,1); I_ = shiftState(I_,1);
    if(currentI == trips[1]){II = shiftState(II,1); II_ = shiftState(II_,1)}
    if(currentII == trips[2]){II = shiftState(II,1); II_ = shiftState(II_,1); III = shiftState(III,1); III_ = shiftState(III_,1)}
  }

  if(length(white_space_ind > 0)){
    for(i in 1:length(white_space_ind)){
      temp = message_output[1:(white_space_ind[i] - 1)]
      temp_ = message_output[white_space_ind[i]:length(message_output)]

      message_output = c(temp, " ", temp_)
    }
  }

  message_final = ""

  for(i in 1:length(message_output)){
    message_final = paste0(message_final, message_output[i])
  }

  cat(message_final)
  cat("\n")

  return(message_final)
}
