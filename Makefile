EMACS=emacs

travis-ci:
	${EMACS} --version
	${EMACS} -batch -Q -l test/env.el -f 'el-spec:run-tests'
