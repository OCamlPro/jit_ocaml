/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2010-2010 - DIGITEO - Antoine ELIAS
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 */

#include "jit_ocaml.hxx"

extern ast::Exp* ast_saved;

ast::Exp* scicaml_string2ast(char *buf)
{
  std::cerr << "scicaml_string2ast" << std::endl;
  ast::Exp* ast = ast_saved;
  ast_saved = NULL;
  return ast;
}
