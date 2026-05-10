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


Update 5/10/26

It's done finally! Yay!

Used gvectors to simplify all vector operations and make them go by faster. There's still some hijinx with 
 conversions when it comes to gvector-remove because I needed it to remove specific items like list's remove,
 and I could not find something like that in the light gvector documentation I was reading.

Additionally, the program still outputs equivalent states when simplifying (ex: (B B) and (b b))
In terms of the math this is not incorrect, as these are the same states, but it's a tad irksome

I have an interest in further developing this to have some sort of ui/visual aspect, and I plan on fixing
 this minor issue when doing that

To be a little more precise, it's because phase disappears at the end of the circuit, when everything is
 measured. The ui layer of it will transform output into some sort of code that can represent white and black dots,
 which will not take phase into account. The lack of phase will make the two offending states be not just equivalent
 but equal, at which point we can do remove-duplicates. The other option would be to code a check for equivalency
 and make an algorithm scanning the entire list at the end for duplicates using that, but it sounds a little more
 involved than simply checking at the ui/visual layer

In a way, all of that's an excuse to justify not doing the work now ... but i do think it would be better than
 doing it in a more manual way, as long as everything goes the way I've laid out above

Anyways, thank you very much! I've enjoyed this class a lot, and although I'm still newer and inexperienced with 
 PL and functional programming, I do like it and hope to continue studying it!