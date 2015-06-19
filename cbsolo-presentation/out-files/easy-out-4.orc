sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin26831152 diskin2 p4, p5
a_var6831153 = a_diskin26831152*p6
a_adsr6831154 adsr 0.0001, 0, 1, 0.1
a_var6831155 = a_var6831153*a_adsr6831154
a_var6831156 = +(a_var6831155)
 outc a_var6831156

endin
