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

/* TODO: call the destructor of the C++ AST when it becomes useless */

ast::Exp* ast_saved = NULL;

#include <time.h>
#include <string>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <iostream>
#include "location.hxx"

#include "jit_ocaml.hxx"
#include "visitor_common.hxx"
//#include "runvisitor.hxx"
//#include "execvisitor.hxx"
//#include "timedvisitor.hxx"
#include "shortcutvisitor.hxx"
#include "printvisitor.hxx"
#include "mutevisitor.hxx"

// Needed by visitprivate(const OpExp &)
// Needed by visitprivate(const LogicalOpExp &)
#include "generic_operations.hxx"
#include "types_bitwiseOp.hxx"
#include "configvariable.hxx"
#include "overload.hxx"
#include "scilabexception.hxx"

#include "matrix_transpose_int.hxx"

extern "C" {
#include "doublecomplex.h"
#include "matrix_transpose.h"
#include "os_swprintf.h"
#include "more.h"
    //#include "HandleManagement.h"
}

#include "timer.hxx"
#include "localization.h"

#include "scilabWrite.hxx"
#include "context.hxx"

#include "all.hxx"
#include "types.hxx"
#include "alltypes.hxx"

// FIXME : remove those using
using namespace types;

namespace ast
{
class SerializerVisitor : public ConstVisitor
{
public:
    SerializerVisitor()
    {
      buf = NULL;
      buflen = 0;
      bufsize = 0;
    }

    ~SerializerVisitor()
    {
      // if we free [buf] now, it is not available for OCaml...
      // free(buf);
      std::cerr << "~SerializerVisitor" << std::endl;
      buf = NULL;
      bufsize = 0;
      buflen = 0;
    }

public:
  char *get_buf(void){ 
    need(1);
    set_uint32(0, buflen);
    std::cerr << "get_buf :" << buflen << std::endl;
    fprintf(stderr, "%d %d %d %d\n", buf[0],buf[1],buf[2],buf[3]);
    return buf; 
  }

  void add_ast(unsigned int code, const ast::Exp* e)
  {
    Location loc = e->location_get();
    std::cerr << "add_ast :" << code << std::endl;
    add_uint8(code);
    add_uint32(loc.first_line);
    add_uint32(loc.first_column);
    add_uint32(loc.last_line);
    add_uint32(loc.last_column);    
    add_uint8(e->is_verbose());
    add_uint8(e->is_break());
    add_uint8(e->is_breakable());
    add_uint8(e->is_return());
    add_uint8(e->is_returnable());
    add_uint8(e->is_continue());
    add_uint8(e->is_continuable());
  }

  /* ensure that we have [size] bytes in the buffer */
  void need(int size)
  {
    if( bufsize - buflen < size ){
      bufsize = 2 * bufsize + size + 2048;
      char *newbuf = (char*) malloc(bufsize);
      std::cerr << "need : malloc " << bufsize << std::endl;
      if( buflen > 0 )
	memcpy(newbuf, buf, buflen);
      if( buf != NULL)
	free(buf);
      else 
	buflen = 4; /* space for the size of the string */
      buf = newbuf;
    }
  }

  void add_uint8(unsigned int n)
  {
    need(1);
    buf[buflen++] = n;
  }

  void add_uint32(unsigned int n)
  {
    need(4);
    buf[buflen++] = (n & 0xff); n >>= 8;
    buf[buflen++] = (n & 0xff); n >>= 8;
    buf[buflen++] = (n & 0xff); n >>= 8;
    buf[buflen++] = (n & 0xff);
  }

  void set_uint32(unsigned int pos, unsigned int n)
  {
     std::cerr << "set_uint32 : n " << n << std::endl;
     buf[pos++] = (n & 0xff); n >>= 8;
     std::cerr << "set_uint32 : n " << n << std::endl;
    buf[pos++] = (n & 0xff); n >>= 8;
     std::cerr << "set_uint32 : n " << n << std::endl;
    buf[pos++] = (n & 0xff); n >>= 8;
     std::cerr << "set_uint32 : n " << n << std::endl;
    buf[pos++] = (n & 0xff);
  }

  int get_pos()
  { return buflen; }

    /*-------------.
    | Attributes.  |
    `-------------*/
protected:
   char *buf;
   int buflen;
   int bufsize;
};

template <class T>
class SerializeVisitorT : public SerializerVisitor
{
public :
    SerializeVisitorT() : SerializerVisitor()
    {
    }

public :

  void visitprivate_SeqExp(const SeqExp *e){
    add_ast(1,e);
    int current_pos = get_pos();
    int nitems = 0;
    add_uint32(0);
    const std::list<Exp *> exps = e->exps_get();
    std::list<Exp *>::const_iterator it;
    for(it = exps.begin() ; it != exps.end() ; it++)
      {
	(*it)->accept(*this);
	nitems ++;
      }
    set_uint32(current_pos, nitems);
  }
  void visitprivate_StringExp(const StringExp *e){ /* done */
    add_ast(2,e);
    std::wstring w = e->value_get();
    int size = w.size();
    const wchar_t *c_str = w.c_str();
    int final_size = size * sizeof(wchar_t);
    add_uint32(final_size);
    need(final_size);
    memcpy(buf + buflen, c_str, final_size);
    buflen += final_size;
  }
  void visitprivate_CommentExp(const CommentExp *e){
    add_ast(3,e);
  }
  void visitprivate_IntExp(const IntExp *e){
    add_ast(4,e);
  }
  void visitprivate_FloatExp(const FloatExp *e){
    add_ast(5,e);
  }
  void visitprivate_DoubleExp(const DoubleExp *e){
    add_ast(6,e);
  }
  void visitprivate_BoolExp(const BoolExp *e){
    add_ast(7,e);
  }
  void visitprivate_NilExp(const NilExp *e){
    add_ast(8,e);
  }
  void visitprivate_SimpleVar(const SimpleVar *e){
    add_ast(9,e);
  }
  void visitprivate_ColonVar(const ColonVar *e){
    add_ast(10,e);
  }
  void visitprivate_DollarVar(const DollarVar *e){
    add_ast(11,e);
  }
  void visitprivate_ArrayListVar(const ArrayListVar *e){
    add_ast(12,e);
  }
  void visitprivate_FieldExp(const FieldExp *e){
    add_ast(13,e);
  }
  void visitprivate_IfExp(const IfExp *e){
    add_ast(14,e);
  }
  void visitprivate_TryCatchExp(const TryCatchExp *e){
    add_ast(15,e);
  }
  void visitprivate_WhileExp(const WhileExp *e){
    add_ast(16,e);
  }
  void visitprivate_ForExp(const ForExp *e){
    add_ast(17,e);
  }
  void visitprivate_BreakExp(const BreakExp *e){
    add_ast(18,e);
  }
  void visitprivate_ContinueExp(const ContinueExp *e){
    add_ast(19,e);
  }
  void visitprivate_ReturnExp(const ReturnExp *e){
    add_ast(20,e);
  }
  void visitprivate_SelectExp(const SelectExp *e){
    add_ast(21,e);
  }
  void visitprivate_CaseExp(const CaseExp *e){
    add_ast(22,e);
  }
  void visitprivate_CellExp(const CellExp *e){ /* done */
    add_ast(23,e);
    visitprivate_MatrixExp(e);
  }
  void visitprivate_ArrayListExp(const ArrayListExp *e){
    add_ast(24,e);
  }
  void visitprivate_AssignListExp(const AssignListExp *e){
    add_ast(25,e);
  }
  void visitprivate_NotExp(const NotExp *e){
    add_ast(26,e);
  }
  void visitprivate_TransposeExp(const TransposeExp *e){
    add_ast(27,e);
  }
  void visitprivate_VarDec(const VarDec *e){
    add_ast(28,e);
  }
  void visitprivate_FunctionDec(const FunctionDec *e){
    add_ast(29,e);
  }
  void visitprivate_ListExp(const ListExp *e){
    add_ast(30,e);
  }
  void visitprivate_AssignExp(const AssignExp *e){
    add_ast(31,e);
  }
  void visitprivate_OpExp(const OpExp *e){
    add_ast(32,e);
  }
  void visitprivate_LogicalOpExp(const LogicalOpExp *e){
    add_ast(33,e);
  }
  void visitprivate_MatrixExp(const MatrixExp *e)
  {
    add_ast(34,e);
  }
  void visitprivate_CallExp(const CallExp *e){
    add_ast(35,e);
  }
  void visitprivate_MatrixLineExp(const MatrixLineExp *e){
    add_ast(36,e);
  }
  void visitprivate_CellCallExp(const CellCallExp *e){
    add_ast(37,e);
  }

};

class SerializeVisitor : public SerializeVisitorT<SerializeVisitor>
{
  void visit (const SeqExp  &e)
  {
    visitprivate_SeqExp(&e);
  }
  
  void visit (const MatrixExp &e)
  {
    visitprivate_MatrixExp(&e);
  }
  
  void visit (const MatrixLineExp &e)
  {
    visitprivate_MatrixLineExp(&e);
  }
  
  void visit (const CellExp &e)
  {
    visitprivate_CellExp(&e);
  }
  
  void visit (const StringExp &e)
  {
    visitprivate_StringExp(&e);
  }
  
  void visit (const CommentExp &e)
  {
    visitprivate_CommentExp(&e);
  }
  
  void visit (const IntExp &e)
  {
    visitprivate_IntExp(&e);
  }
  
  void visit (const FloatExp &e)
  {
    visitprivate_FloatExp(&e);
  }
  
  void visit (const DoubleExp &e)
  {
    visitprivate_DoubleExp(&e);
  }
  
  void visit (const BoolExp &e)
  {
    visitprivate_BoolExp(&e);
  }
  
  void visit (const NilExp &e)
  {
    visitprivate_NilExp(&e);
  }
  
  void visit (const SimpleVar &e)
  {
    visitprivate_SimpleVar(&e);
  }
  
  void visit (const ColonVar &e)
  {
    visitprivate_ColonVar(&e);
  }
  
  void visit (const DollarVar &e)
  {
    visitprivate_DollarVar(&e);
  }
  
  void visit (const ArrayListVar &e)
  {
    visitprivate_ArrayListVar(&e);
  }
  
  void visit (const FieldExp &e)
  {
    visitprivate_FieldExp(&e);
  }
  
  void visit (const OpExp &e)
  {
    visitprivate_OpExp(&e);
  }
  
  void visit (const LogicalOpExp &e)
  {
    visitprivate_LogicalOpExp(&e);
  }
  
  void visit (const AssignExp &e)
  {
    visitprivate_AssignExp(&e);
  }
  
  void visit (const CellCallExp &e)
  {
    visitprivate_CellCallExp(&e);
  }
  
  void visit (const CallExp &e)
  {
    visitprivate_CallExp(&e);
  }

  void visit (const IfExp &e)
  {
    visitprivate_IfExp(&e);
  }
  
  void visit (const TryCatchExp &e)
  {
    visitprivate_TryCatchExp(&e);
  }
  
  void visit (const WhileExp &e)
  {
    visitprivate_WhileExp(&e);
  }
  
  void visit (const ForExp &e)
  {
    visitprivate_ForExp(&e);
  }
  
  void visit (const BreakExp &e)
  {
    visitprivate_BreakExp(&e);
  }
  
  void visit (const ContinueExp &e)
  {
    visitprivate_ContinueExp(&e);
  }
  
  void visit (const ReturnExp &e)
  {
    visitprivate_ReturnExp(&e);
  }
  
  void visit (const SelectExp &e)
  {
    visitprivate_SelectExp(&e);
  }
  
  void visit (const CaseExp &e)
  {
    visitprivate_CaseExp(&e);
  }
  
  void visit (const ArrayListExp &e)
  {
    visitprivate_ArrayListExp(&e);
  }
  
  void visit (const AssignListExp &e)
  {
    visitprivate_AssignListExp(&e);
  }
  
  void visit (const NotExp &e)
  {
    visitprivate_NotExp(&e);
  }
  
  void visit (const TransposeExp &e)
  {
    visitprivate_TransposeExp(&e);
  }
  
  void visit (const VarDec &e)
  {
    visitprivate_VarDec(&e);
  }
  
  void visit (const FunctionDec &e)
  {
    visitprivate_FunctionDec(&e);
  }
  
  void visit(const ListExp &e)
  {
    visitprivate_ListExp(&e);
  }
};

}



char* scicaml_ast2string(ast::Exp* ast)
{
  std::cerr << "scicaml_ast2string" << std::endl;

  ast::SerializeVisitor visitor;

  ast->accept(visitor);
  ast_saved = ast;
  return visitor.get_buf();
}
