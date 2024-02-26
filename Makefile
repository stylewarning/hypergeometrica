.PHONY: all
all: hypergeometrica-manager hypergeometrica-worker

# FIXME: this seems to depend on quicklisp being loaded, probably so
# ASDF can know about the paths.
hypergeometrica-manager:
	sbcl --non-interactive --eval '(asdf:make "hypergeometrica-manager")'
	mv src-manager/hypergeometrica-manager .

hypergeometrica-worker:
	sbcl --non-interactive --eval '(asdf:make "hypergeometrica-worker")'
	mv src-worker/hypergeometrica-worker .

.PHONY: clean
clean:
	rm -f hypergeometrica-manager hypergeometrica-worker
