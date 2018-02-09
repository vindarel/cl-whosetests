
# Needs the cl21 dist.
build:
	sbcl --load clwhosconnected.asd \
             --load ../replic/replic.asd \
		--eval '(ql:quickload :clwhosconnected)' \
		--eval '(asdf:make :clwhosconnected)' \
		--eval '(quit)'
