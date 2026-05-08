Main def file is msf-eval.rkt

msf-testing.rkt is for testing, and has some example problems: grover search and deutch-josza, with various oracles


How to run:

Source from msf-eval.rkt like so:

#lang s-exp "msf-eval.rkt"

Type and run code

See grammar in this repo - it will provide an idea of what each syntax does and how to use it


Update 5/8/26

added core file vs main file to separate phases and provide definitions

this version is not functional - states vanish when running hadamard, probably because of how vectors are being handled 
 in that gate operation

I'm assuming working code will get a better grade than broken code even if it's slow; the old working version 
 is in msf-eval-old.rkt

this needs to be fixed still - but I am pushing what I have done right now. I plan on fixing this sometime over
 the next week or two, but with graduation stuff and family being in town right now I've kind of run out of time 
 in the short term

