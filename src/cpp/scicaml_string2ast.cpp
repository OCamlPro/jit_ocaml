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

#include <stdlib.h>
#include "jit_ocaml.hxx"

#include <seqexp.hxx>
#include <stringexp.hxx>

extern ast::Exp* ast_saved;

char *buf;

static unsigned int get_uint32(void)
{
  unsigned int c0 = *buf++;
  unsigned int c1 = *buf++;
  unsigned int c2 = *buf++;
  unsigned int c3 = *buf++;
  return c0 + ((c1 + ((c2 + (c3 << 8)) << 8 )) << 8 );
}

static unsigned int get_uint8(void)
{
  return *buf++;
}

static bool get_bool(void)
{
  return *buf++;
}

Location *get_loc(void)
{
  Location *loc = new Location();
  loc->first_line = get_uint8();
  loc->first_column = get_uint8();
  loc->last_line = get_uint8();
  loc->last_column = get_uint8();
  return loc;
}

static ast::Exp* get_exp(void);

static std::list<ast::Exp*> get_exp_list(void)
{
  int nitems = get_uint32();
  std::list<ast::Exp*> *list = new  std::list<ast::Exp*>;
  for(int i = 0; i < nitems; i++){
    ast::Exp* exp = get_exp();
    list->push_back(exp);
  }
  return *list;
}


static std::wstring* get_wstring(void)
{
  int size = get_uint32();
  wchar_t* ss = (wchar_t*)buf;
  std::wstring* s = new wstring(ss, size / sizeof(wchar_t));
  buf += size;
  return s;
}

static ast::Exp* get_exp(void)
{
  ast::Exp* exp;
  int code = get_uint8();
  Location *loc = get_loc();
  int is_verbose = get_bool();
  int is_break = get_bool();
  int is_breakable = get_bool();
  int is_return = get_bool();
  int is_returnable = get_bool();
  int is_continue = get_bool();
  int is_continuable = get_bool();
  
  
  switch(code){
  case 1: {   
    std::list<ast::Exp *> l_body = get_exp_list();
    exp = new ast::SeqExp(*loc, l_body);
    break;
  }
  case 2: {
    std::wstring* s = get_wstring();
    exp = new ast::StringExp(*loc, *s);
    break;
  }
  default: 
    std::cerr << "Unkown code " << code << std::endl;
    exit(2);
  }

  exp->set_verbose(is_verbose);
  if(is_break) exp->break_set();
  if(is_breakable) exp->breakable_set();
  if(is_return) exp->return_set();
  if(is_returnable) exp->returnable_set();
  if(is_continue) exp->continue_set();
  if(is_continuable) exp->continuable_set();
  
  return exp;
}

ast::Exp* scicaml_string2ast(char *buffer)
{
  std::cerr << "scicaml_string2ast" << std::endl;
  buf = buffer;
  int buflen = get_uint32();
  ast::Exp* new_ast = get_exp();

  ast::Exp* ast = ast_saved;
  ast_saved = NULL;
  return ast;
}
