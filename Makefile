
# Needs the cl21 dist.
build:
	sbcl --non-interactive \
	     --load clwhosconnected.asd \
             --load ../replic/replic.asd \
		--eval '(ql:quickload :clwhosconnected)' \
		--eval '(asdf:make :clwhosconnected)'
