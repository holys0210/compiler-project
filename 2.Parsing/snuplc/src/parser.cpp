//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      //if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  // TODO: add predefined functions here
}

CAstModule* CParser::module(void)
{
  //
  // module ::= statSequence  ".".
	// "module" ident ";" varDeclaration { subroutineDecl }
	// "begin" statSequence "end" ident "."
  //

  CToken dummy;
	Consume(tModule, &dummy);

	CToken module_name;
	Consume(tIdent,  &module_name);

	// ";"
	Consume(tSemicolon, &dummy);
	
  CAstModule *m = new CAstModule(dummy, module_name.GetValue());
	// varDeclaration
	varDeclaration(m);

	// subroutineDecl
	// TODO
	// FIRST(subroutineDecl)={"procedure", "function"}
	EToken tt=_scanner->Peek().GetType();
	while((tt== tProcedure)||(tt==tFunction)){
		subroutineDecl(m);
		tt=_scanner->Peek().GetType();
	}


	// begin
	Consume(tBegin, &dummy);

	// statSequence
  CAstStatement *statseq = NULL;

  statseq = statSequence(m);

	// end
	Consume(tEnd, &dummy);

	// ident
	CToken end_module_name;
	Consume(tIdent, &end_module_name); 

	if(module_name.GetValue()!=end_module_name.GetValue()){
		SetError(end_module_name, "Not mahch on module name");
	}

	// "."
  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment.
  // FIRST(statSequence) = { tNumber }
  // FOLLOW(statSequence) = { tEnd }
  //
  CAstStatement *head = NULL;

  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tEnd)) {
    CAstStatement *tail = NULL;

    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;

      switch (tt) {
        // statement ::= assignment
        case tNumber:
          st = assignment(s);
          break;

        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt == tEnd) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
  //
  // assignment ::= number ":=" expression.
  //
  CToken t;

  CAstConstant *lhs = number();
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpression ].
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= term { termOp term }.
  //
  CAstExpression *n = NULL;

  n = term(s);

  while (_scanner->Peek().GetType() == tPlusMinus) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tPlusMinus, &t);

    r = term(s);

    n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
  }


  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tMulDiv)) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tMulDiv, &t);

    r = factor(s);

    n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= number | "(" expression ")"
  //
  // FIRST(factor) = { tNumber, tLBrak }
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;

  switch (tt) {
    // factor ::= number
    case tNumber:
      n = number();
      break;

    // factor ::= "(" expression ")"
    case tLParens:
      Consume(tLParens);
      n = expression(s);
      Consume(tRParens);
      break;

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

void CParser::varDeclaration(CAstScope* m){
	//
	// "var" varDeclSequence ";"
	// varDeclSequence = varDecl { ";" varDecl }
	// varDecl = ident { "," ident } ":" type
	// FOLLOW(varDeclaration)={"begin", "procedure", "function"}
	// another varDeclaration exists, but this grammer is not LL(1)
	//

	EToken tt = _scanner->Peek().GetType();
	if((tt==tBegin)||(tt==tProcedure)||(tt==tFunction)){
		return;
	}

	// "var"
	CToken dummy;
	Consume(tVarDecl, &dummy);

	// typedef for varDecl
	CToken id[16];
	int index;

	CSymtab* mod_symtab=m->GetSymbolTable();

	// varDeclSequence + ";"
	do {
		tt = _scanner->Peek().GetType();

		// varDecl = ident { "," ident } ":" type
		index=0;
		//ident
		while(1){
			Consume(tIdent, &id[index++]);

			if(_scanner->Peek().GetType() == tColon){
				// ":"
				Consume(tColon, &dummy);
				break;
			}
			// ","
			Consume(tComma, &dummy);
		}

		// type
		const CScalarType* var_type=type();

		//add symbol to symtab
		for(int i=0; i<index; i++){
			mod_symtab->AddSymbol(m->CreateVar(id[i].GetValue(), var_type));
		}
		/////////

		Consume(tSemicolon, &dummy);
		tt=_scanner->Peek().GetType();
		if((tt==tBegin)||(tt==tProcedure)||(tt==tFunction)){
			break;
		}
	}while(!_abort);

}

const CScalarType* CParser::type(){
		CToken dummy;
		EToken tt=_scanner->Peek().GetType();
		switch(tt){
			case tInteger:
				Consume(tt, &dummy);
				return CTypeManager::Get()->GetInt();
				break;
			case tBoolean:
				Consume(tt, &dummy);
				return CTypeManager::Get()->GetBool();
				break;
			case tChar:
				Consume(tt, &dummy);
				return CTypeManager::Get()->GetChar();
				break;

			default:
				//error (temporary handling error)
				//TODO
				Consume(tInteger, &dummy);
		}
		//TODO
		// array
}

void CParser::subroutineDecl(CAstModule* m){
	EToken tt=_scanner->Peek().GetType();
	CToken dummy;
	CAstProcedure* proc;

	switch(tt){
		case tProcedure:
			proc=procedureDecl(m);
			break;

		case tFunction:
			proc=functionDecl(m);
			break;

		case tBegin:
			return;

		default:
			//error
			//TODO
			Consume(tBegin, &dummy);
	}

	subroutineBody(proc);

	//ident

	// ";"


}

CAstProcedure* CParser::procedureDecl(CAstModule* m){
	//
	// procedureDecl = "procedure" ident [ formalParam ] ";"
	//
	CToken dummy, name;

	Consume(tProcedure, &dummy);
	Consume(tIdent, &name);

	// create AstNode
	// procedure's return value is void, NULL
	CSymProc* sym_proc = new CSymProc(name.GetValue(), CTypeManager::Get()->GetNull());
	CAstProcedure *proc = new CAstProcedure(name, name.GetValue(), m, sym_proc);

	EToken tt= _scanner->Peek().GetType();
	if(tt == tLParens){
		formalParam(proc);
	}

	Consume(tSemicolon, &dummy);

	// print for check
	//proc->GetSymbol()->print(cout, 4);
	//


	return proc;

}

CAstProcedure* CParser::functionDecl(CAstModule* m){
	//
	// functionDecl = "function" ident [ formalParam ] ":" type ";"
	//
	CToken dummy, name;

	Consume(tFunction, &dummy);
	Consume(tIdent, &name);

	CSymProc* sym_proc = new CSymProc(name.GetValue(), CTypeManager::Get()->GetNull());
	CAstProcedure *proc = new CAstProcedure(name, name.GetValue(), m, sym_proc);

	EToken tt= _scanner->Peek().GetType();
	if(tt == tLParens){
		formalParam(proc);
	}

	Consume(tColon, &dummy);

	const CScalarType* var_type=type();

	Consume(tSemicolon, &dummy);

	//change return type
	sym_proc->SetDataType(var_type);

	
	// print for check
	//proc->GetSymbol()->print(cout, 4);
	//

	return proc;

}

void CParser::formalParam(CAstProcedure* proc){
	//
	// formalParam = "(" [ varDeclSequence ] ")"
	//

	CToken dummy;
	Consume(tLParens, &dummy);


	EToken tt=_scanner->Peek().GetType();
	// FIRST(varDeclSequence) = { ident}
	if(tt==tIdent){
		varDeclSequence(proc);
	}

	Consume(tRParens, &dummy);
}


void CParser::varDeclSequence(CAstProcedure* proc){
	//
	// varDeclSequence = varDecl { ";" vrDecl }
	//

	CToken dummy;
	varDecl(proc);

	while(_scanner->Peek().GetType() == tSemicolon){
		Consume(tSemicolon, &dummy);
		varDecl(proc);
	}

}

void CParser::varDecl(CAstProcedure* proc){
	//
	// varDecl = ident { "," ident } ":" type
	//

	CToken id[16], dummy;
	EToken  tt;

	int index=0;

	while(1){
		Consume(tIdent, &id[index++]);

		if(_scanner->Peek().GetType() == tColon){
			Consume(tColon, &dummy);
			break;
		}
		Consume(tComma, &dummy);
	}
	
	const CScalarType* var_type=type();
	CSymtab* symtab = proc->GetSymbolTable();
	CSymProc* symproc = proc->GetSymbol();

	for(int i=0; i<index; i++){
		symtab->AddSymbol(proc->CreateVar(id[i].GetValue(), var_type));
		symproc->AddParam(new CSymParam(i, id[i].GetValue(), var_type));
	}

}

void CParser::subroutineBody(CAstProcedure* proc){
	//
	// subroutineBody = varDeclaration "begin" statSequence "end"
	//
	varDeclaration(proc);

	CToken dummy;

	Consume(tBegin, &dummy);

	// statSequence(proc);

	Consume(tEnd, &dummy);

}
