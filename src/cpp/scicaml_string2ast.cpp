/*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2012-2012 - OCAMLPRO INRIA - Fabrice LE FESSANT
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

#include <all.hxx>
#include <commentexp.hxx>

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
  case 3: {
    std::wstring* s = get_wstring();
    exp = new ast::CommentExp(*loc, s);
    break;
  }
    /*
  case 4: {
    exp = new ast::IntExp(*loc);
    break;
  }
  case 5: {
    exp = new ast::FloatExp(*loc);
    break;
  }
  case 6: {
    exp = new ast::DoubleExp(*loc);
    break;
  }
  case 7: {
    exp = new ast::BoolExp(*loc);
    break;
  }
  case 8: {
    exp = new ast::NilExp(*loc);
    break;
  }
  case 9: {
    exp = new ast::SimpleVar(*loc);
    break;
  }
  case 10: {
    exp = new ast::ColonVar(*loc);
    break;
  }
  case 11: {
    exp = new ast::DollarVar(*loc);
    break;
  }
  case 12: {
    exp = new ast::ArrayListVar(*loc);
    break;
  }
  case 13: {
    exp = new ast::FieldExp(*loc);
    break;
  }
  case 14: {
    exp = new ast::IfExp(*loc);
    break;
  }
  case 15: {
    exp = new ast::TryCatchExp(*loc);
    break;
  }
  case 16: {
    exp = new ast::WhileExp(*loc);
    break;
  }
  case 17: {
    exp = new ast::ForExp(*loc);
    break;
  }
  case 18: {
    exp = new ast::BreakExp(*loc);
    break;
  }
  case 19: {
    exp = new ast::ContinueExp(*loc);
    break;
  }
  case 20: {
    exp = new ast::ReturnExp(*loc);
    break;
  }
  case 21: {
    exp = new ast::SelectExp(*loc);
    break;
  }
  case 22: {
    exp = new ast::CaseExp(*loc);
    break;
  }
  case 23: {
    exp = new ast::CellExp(*loc);
    break;
  }
  case 24: {
    exp = new ast::ArrayListExp(*loc);
    break;
  }
  case 25: {
    exp = new ast::AssignListExp(*loc);
    break;
  }
  case 26: {
    exp = new ast::NotExp(*loc);
    break;
  }
  case 27: {
    exp = new ast::TransposeExp(*loc);
    break;
  }
  case 28: {
    exp = new ast::VarDec(*loc);
    break;
  }
  case 29: {
    exp = new ast::FunctionDec(*loc);
    break;
  }
  case 30: {
    exp = new ast::ListExp(*loc);
    break;
  }
  case 31: {
    exp = new ast::AssignExp(*loc);
    break;
  }
  case 32: {
    exp = new ast::OpExp(*loc);
    break;
  }
  case 33: {
    exp = new ast::LogicalExp(*loc);
    break;
  }
  case 34: {
    exp = new ast::MatrixExp(*loc);
    break;
  }
  case 35: {
    exp = new ast::CallExp(*loc);
    break;
  }
  case 36: {
    exp = new ast::MatrixLineExp(*loc);
    break;
  }
  case 37: {
    exp = new ast::CallCallExp(*loc);
    break;
  }
*/
  default: 
    std::cerr << "Unknown code " << code << std::endl;
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
  /*
  buf = buffer;
  int buflen = get_uint32();
  ast::Exp* new_ast = get_exp();
  */
  ast::Exp* ast = ast_saved;
  ast_saved = NULL;
  return ast;
}
