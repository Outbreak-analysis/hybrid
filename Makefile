##This is a Makefile

current: target

target pngtarget pdftarget vtarget acrtarget: fit.hyb.nb.nb.1.4000.Rout
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

all: fit.dis.b.p.1.4000.Rout fit.dis.b.b.1.4000.Rout fit.dis.b.nb.1.4000.Rout fit.hyb.b.p.1.4000.Rout fit.hyb.b.nb.1.4000.Rout

all.dis: fit.dis.bb.b.1.4000.Rout fit.dis.bb.p.1.4000.Rout fit.dis.bb.nb.1.4000.Rout fit.dis.bb.bb.1.4000.Rout

all.hyb: fit.hyb.bb.p.1.4000.Rout fit.hyb.bb.nb.1.4000.Rout fit.hyb.b.p.1.4000.Rout fit.hyb.b.nb.1.4000.Rout

clean:
	rm -f *.nimble.R *.buggen *.wrapR.r *.Rout *.nimcode *.stan *.init.R *.data.R *.Rlog *.wrapR.rout .sim* .template* .fit*

#############
Sources += $(wildcard *.R *.bug)

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/linux.mk
-include $(ms)/wrapR.mk
-include rmd.mk
