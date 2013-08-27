all:: rocketwiki-lqfb rocketwiki-lqfb-compat

rocketwiki-lqfb: rocketwiki-lqfb.hs
	ghc --make rocketwiki-lqfb

rocketwiki-lqfb-compat: rocketwiki-lqfb-compat.hs
	ghc --make rocketwiki-lqfb-compat

clean::
	rm -f rocketwiki-lqfb rocketwiki-lqfb.hi rocketwiki-lqfb.o
	rm -f rocketwiki-lqfb-compat rocketwiki-lqfb-compat.hi rocketwiki-lqfb-compat.o
