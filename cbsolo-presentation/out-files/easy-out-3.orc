sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin2733254 diskin2 p4, p5
a_var733255 = a_diskin2733254*p6
a_adsr733258 adsr 0.0001, 0, 1, 0.1
a_var733271 = a_var733255*a_adsr733258
a_var733272 = +(a_var733271)
 outc a_var733272

endin
