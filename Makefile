STACKPATH=$(shell stack path | grep local-install-root | sed 's/local-install-root: //')

default:
	stack build
	@mkdir -p builds
	@cp ${STACKPATH}/bin/rpgprint builds/rpgprint
	@cp ${STACKPATH}/bin/rpgencode builds/rpgencode

clean:
	stack clean
	@rm -r builds

