sr = 44100
kr = 44100
ksmps = 1
nchnls = 1





instr 1

a_diskin211819075 diskin2 p4, p5
a_var11819088 = a_diskin211819075*p6
a_adsr11819089 adsr 0.0001, 0, 1, 0.1
a_var11819090 = a_var11819088*a_adsr11819089
a_var11819091 = +(a_var11819090)
 outc a_var11819091

endin
