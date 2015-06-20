sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin232089933 diskin2 p4, p5
a_var32089934 = a_diskin232089933*p6
a_adsr32089935 adsr 0.0001, 0, 1, 0.1
a_var32089936 = a_var32089934*a_adsr32089935
a_var32089937 = +(a_var32089936)
 outc a_var32089937

endin
