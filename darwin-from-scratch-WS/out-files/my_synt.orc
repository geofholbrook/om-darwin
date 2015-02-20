; HEADER 

sr = 44100
kr = 44100
ksmps = 1
nchnls = 1

; GLOBAL VARIABLES

0dbfs   = 8388607 ; 24 bits




;INSTRUMENTS

;==============
instr 1
;==============


idur		= p3
idurosc		= 1/idur
iamp 		= (p4 > 0.0 ? (p4*0.001*0dbfs) : (ampdbfs (p4)))
ifq		= p5
iaenv		= p6
iaudiofun	= 1

a2    poscil  iamp, idurosc, iaenv
a1    poscil  a2, ifq, iaudiofun
out     a1

endin

