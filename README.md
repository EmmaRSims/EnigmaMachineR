# EnigmaMachineR
This is a simulation of the Enigma I machine in R.

Written as part of a mini-jam set by @CalMaths on twitter.

Can be installed by cloning this repository via the `devtools` package:

`library(devtools); devtools::install_github("EmmaRSims/EnigmaMachineR/EnigmaR")`

To use, there are 5 inputs, of which only `message` is required:
* `message` - a string with the message you wish to encode
* `rotorOrder` - a numeric vector of length 3, with values from 1 to 5, depicting the rotor order in the format R-M-L; by default this is `c(1,2,3)`
* `reflector` - a string depicting the reflector to use, the options are: "UKW-A", "UKW-B", "UKW-C"; by default this is "UKW-A"
* `plugConnections` - a character matrix of two columns and 6 rows representing the stecker connections, by default it is `cbind(c("A","B","C","D","E","F"),c("Z","Y","X","W","V","U"))` 
* `groundSetting` - a character vector of length 3, depicting the ground setting of the three rotors in R-M-L format; by default this is c("A","A","A")

The main function is `enigmaMachine()`, which will both print the output message to console, and return the output message string.

Have fun and let me know if there are any improvements!
