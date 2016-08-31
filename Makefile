##This is a Makefile

current: target

target pngtarget pdftarget vtarget acrtarget: fit.hyb.b.p.1.4000.Rout 

##################################################################

Sources += Makefile stuff.mk
include stuff.mk
-include $(ms)/git.def

sim.%.Rout: parameters.CBB.R name.R simulate.R
	$(run-R)

bugstemp.%.Rout: name.R parameters.CBB.R process.R observations.R bugstemp.R
	$(run-R)

fit.%.Rout: names.R sim.%.Rout bugstemp.%.Rout fit.R
	$(run-R)


clean:
	rm *.nimble.R *.buggen *.wrapR.r *.Rout

#############
Sources += $(wildcard *.R *.bug)

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/linux.mk
-include $(ms)/wrapR.mk
-include rmd.mk
