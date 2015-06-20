sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin232088507 diskin2 p4, p5
a_var32088508 = a_diskin232088507*p6
a_adsr32088509 adsr 0.0001, 0, 1, 0.1
a_var32088510 = a_var32088508*a_adsr32088509
a_var32088511 = +(a_var32088510)
 outc a_var32088511

endin
