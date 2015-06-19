sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin270296 diskin2 p4, p5
a_var70297 = a_diskin270296*p6
a_adsr70298 adsr 0.0001, 0, 1, 0.1
a_var70299 = a_var70297*a_adsr70298
a_var70300 = +(a_var70299)
 outc a_var70300

endin
