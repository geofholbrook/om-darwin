sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin247942689 diskin2 p4, p5
a_var47942738 = a_diskin247942689*p6
a_adsr47942749 adsr 0.0001, 0, 1, 0.1
a_var47942750 = a_var47942738*a_adsr47942749
a_var47942751 = +(a_var47942750)
 outc a_var47942751

endin
