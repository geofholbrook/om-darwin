sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin268980 diskin2 p4, p5
a_var68981 = a_diskin268980*p6
a_adsr68982 adsr 0.0001, 0, 1, 0.1
a_var68983 = a_var68981*a_adsr68982
a_var68984 = +(a_var68983)
 outc a_var68984

endin
