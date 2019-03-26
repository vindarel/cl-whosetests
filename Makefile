
# Needs the cl21 dist.
build:
	sbcl --non-interactive \
	     --load clwhosconnected.asd \
		--eval '(ql:quickload :clwhosconnected)' \
		--eval '(asdf:make :clwhosconnected)'
