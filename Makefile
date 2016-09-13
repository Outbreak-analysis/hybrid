##This is a Makefile

current: target

target pngtarget pdftarget vtarget acrtarget: run_bash
##################################################################

Sources += Makefile stuff.mk
include stuff.mk
-include $(ms)/git.def

sim.%.Rout: parameters.CBB.R name.R simulate.CBB.R
	$(run-R)

templates.%.Rout: sim.%.Rout name.R parameters.CBB.R process.R observations.R bugstemplate.R stantemplate.R 	
	$(run-R)

fit.%.Rout: name.R sim.%.Rout templates.%.Rout fit.R
	$(run-R)

run_bash: run_all
	bash run_all

clean:
	rm -f *.nimble.R *.buggen *.wrapR.r *.Rout *.nimcode *.stan *.init.R *.data.R *.Rlog *.wrapR.rout .sim* .template* .fit*

run_dis:
	bash run_all_b

#############
Sources += $(wildcard *.R *.bug)

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/linux.mk
-include $(ms)/wrapR.mk
-include rmd.mk
