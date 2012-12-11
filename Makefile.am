#
# Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
# Copyright (C) DIGITEO - 2010 - Allan CORNET
# Copyright (C) DIGITEO - 2010 - Bruno JOFRET
#
# This file must be used under the terms of the CeCILL.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution.  The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
#

#### Target ######
modulename=jit_ocaml


#### jit_ocaml : Conf files ####
libscijit_ocaml_la_rootdir = $(mydatadir)
libscijit_ocaml_la_root_DATA =  license.txt


#### jit_ocaml : init scripts ####
libscijit_ocaml_la_etcdir = $(mydatadir)/etc
libscijit_ocaml_la_etc_DATA = etc/jit_ocaml.quit \
			  etc/jit_ocaml.start

include $(top_srcdir)/Makefile.incl.am


JIT_OCAML_CPP_SOURCES = \
			src/cpp/scicaml_ast2string.cpp \
			src/cpp/scicaml_string2ast.cpp

#JIT_OCAML_C_SOURCES = \
#			src/c/jit_callback.c

libscijit_ocaml_la_CFLAGS = -I$(srcdir)/includes

libscijit_ocaml_la_CPPFLAGS = \
	-I$(srcdir)/includes \
        -I$(top_srcdir)/libs/MALLOC/includes \
	-I$(top_srcdir)/modules/elementary_functions/includes \
	-I$(top_srcdir)/modules/localization/includes \
	-I$(top_srcdir)/modules/operations/includes \
	-I$(top_srcdir)/modules/symbol/includes \
	-I$(top_srcdir)/modules/ast/includes \
	-I$(top_srcdir)/modules/types/includes \
	-I$(top_srcdir)/modules/parse/includes \
	-I$(top_srcdir)/modules/system_env/includes \
	-I$(top_srcdir)/modules/functions_manager/includes \
	-I$(top_srcdir)/modules/string/includes \
	-I$(top_srcdir)/modules/threads/includes \
        -I$(top_srcdir)/modules/api_scilab/includes \
        -I$(top_srcdir)/modules/console/includes \
        -I$(top_srcdir)/modules/output_stream/includes \
        -I$(top_srcdir)/libs/dynamiclibrary/includes \
	-I src/cpp \
	$(EIGEN_CPPFLAGS) \
	$(AM_CPPFLAGS)

pkglib_LTLIBRARIES = libscijit_ocaml.la
#BUILT_SOURCES = libscijit_ocaml-runtime.a
#
libscijit_ocaml_la_LIBADD= libscijit_ocaml-runtime.o
libscijit_ocaml_la_SOURCES = $(JIT_OCAML_CPP_SOURCES)
OCAMLC_WHERE := `ocamlc -where`

jit_ocaml.o: src/c/jit_ocaml.c
	ocamlopt -c -I includes -fPIC src/c/jit_ocaml.c

OCAML_SRCS = \
	src/ocaml/scilabAst.ml

libscijit_ocaml-runtime.o: $(OCAML_SRCS) jit_ocaml.o libasmrun.a
	ocamlopt -c -fPIC src/ocaml/scilabAst.ml
	ocamlopt -o libscijit_ocaml-runtime.o -output-obj src/ocaml/scilabAst.cmx

libasmrun.a:
	cp $(OCAMLC_WHERE)/libasmrun.a .


