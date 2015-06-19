sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin215505093 diskin2 p4, p5
a_var15505094 = a_diskin215505093*p6
a_adsr15505095 adsr 0.0001, 0, 1, 0.1
a_var15505096 = a_var15505094*a_adsr15505095
a_var15505111 = +(a_var15505096)
 outc a_var15505111

endin
