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
  if ((tt == tEnd)||(tt==tElse)) {
		return head;
	}

	CAstStatement *tail = NULL;

	do {
		CToken t;
		EToken tt = _scanner->Peek().GetType();
		CAstStatement *st = NULL;

		switch (tt) {
			// statement ::= assignment
			case tIdent:
				st = assignment_or_subroutineCall(s);
				break;

			case tIf:
				//ifStatement(s);
				break;

			case tWhile:
				//whileStatement(s);
				break;

			case tReturn:
				//returnStatment(s);
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
		if ((tt == tEnd)||(tt== tElse)) break;

		Consume(tSemicolon);
	} while (!_abort);

  return head;
}
/*
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
*/

CAstStatAssign* CParser::assignment(CAstScope *s, CToken name)
{
  //
	// assignment ::= qualident ":=" expression
  //
  CToken t;

	CAstDesignator* lhs = qualident(s, name);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstDesignator* CParser::qualident(CAstScope* s, CToken name){
	//
	// qualident ::= ident { "[" expression "]" } 
	// qualident->factor->term->simpleexpr->expression->returnStatment->statment->statSeqeunce
	//

	EToken tt = _scanner->Peek().GetType();

	CSymtab* symtab=s->GetSymbolTable();
	const CSymbol* sym = symtab->FindSymbol(name.GetValue());
	if(sym==NULL){
		SetError(name, "No identifier in symbol table");
	}


	if(!(tt == tLBrak)){
		// non-array
		return new CAstDesignator(name, sym);
	}
	else{
		CAstArrayDesignator* design=new CAstArrayDesignator(name, sym);
		do{
			Consume(tLBrak);
			CAstExpression* expr=expression(s);
			Consume(tRBrak);
			design->AddIndex(expr);

			tt=_scanner->Peek().GetType();
		}while(tt==tLBrak);
		return design;
	}

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

		string relop_str=t.GetValue();

		if (relop_str == "=")       relop = opEqual;
		else if (relop_str == "#")  relop = opNotEqual;
		else if (relop_str == "<")	relop = opLessThan;
		else if (relop_str == "<=")	relop = opLessEqual;
		else if (relop_str == ">") 	relop = opBiggerThan;
		else if (relop_str == ">=")	relop = opBiggerEqual;
		else SetError(t, "invalid relation.");

		return new CAstBinaryOp(t, relop, left, right);
	} 
	else {
		return left;
	}
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= ["+"|"-"] term { termOp term }.
	// ["+"|"-'] is stronger than termOp
  //
	EToken tt = _scanner->Peek().GetType();
	
  CAstExpression *n= NULL;

	if(tt==tPlusMinus){
		CToken sign;
		Consume(tPlusMinus, &sign);
		CAstExpression* m =term(s);
		n = new CAstUnaryOp(sign, sign.GetValue() == "+" ? opPos : opNeg, m);
	}
	else{
		n = term(s);
	}

	tt=_scanner->Peek().GetType();
	EOperation termop;

  while ((tt==tPlusMinus)||(tt==tOr)) {
    CToken t;
    CAstExpression *l = n, *r;

		switch(tt){
			case tPlusMinus:
				Consume(tPlusMinus, &t);
				termop= t.GetValue() == "+" ? opAdd : opSub;
				break;

			case tOr:
				Consume(tOr, &t);
				termop=opOr;
				break;
		}

    r = term(s);

    n = new CAstBinaryOp(t, termop, l, r);

		tt=_scanner->Peek().GetType();
  }


  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/"|"&&") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tMulDiv)||(tt==tAnd)) {
    CToken t;
    CAstExpression *l = n, *r;
		EOperation factop;

		switch(tt){
			case tMulDiv:
				Consume(tMulDiv, &t);
				factop= t.GetValue() == "*" ? opMul : opDiv;
				break;

			case tAnd:
				Consume(tAnd, &t);
				factop = opAnd;
				break;
		}

    r = factor(s);

    n = new CAstBinaryOp(t, factop, l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
	// factor ::= qualident | number | boolean | char | string
	//					| "(" expression ")" | subroutineCall | "!" factor
  //
  // FIRST(factor) = { tNumber, tLBrak }
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;

  switch (tt) {
		case tIdent:
		{
		// qualident & subroutineCall
			Consume(tIdent, &t);
			tt=_scanner->Peek().GetType();

			// subroutineCall
			if(tt==tLParens){
				 n = subroutineCall_expr(s, t);
			}
			// qualident
			else{
				// "(" not in FOLLOW(qualident)
				n = qualident(s, t);
			}
			break;
		}

    // factor ::= number
    case tNumber:
      n = number();
      break;

		// factor ::= boolean
		case tBoolean:
			n = boolean();
			break;

		// factor ::= character
		case tChar:
//			n = character()
			break;

		// factor ::= string 
		case tString:
			Consume(tString, &t);
			n = new CAstStringConstant(t, t.GetValue(), s);
			break;

    // factor ::= "(" expression ")"
    case tLParens:
      Consume(tLParens);
      n = expression(s);
      Consume(tRParens);
      break;

		// factor ::= "!" factor
		case tNot:
			Consume(tNot, &t);
			n = new CAstUnaryOp(t, opNop, factor(s));
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

CAstConstant* CParser::boolean(void){
	//
	// 1 ::= true
	// 0 ::= false
	//
	CToken t;
	Consume(tBoolean, &t);
	CAstConstant* constant=NULL;

	if((t.GetValue()=="true")){
		constant=new CAstConstant(t, CTypeManager::Get()->GetBool(), 1);
	}
	else{
		constant=new CAstConstant(t, CTypeManager::Get()->GetBool(), 0);
	}
	return constant;
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
	CToken end_proc_name;
	Consume(tIdent, &end_proc_name);

	if(end_proc_name.GetValue()!=proc->GetName()){
		SetError(end_proc_name, "Not match on subroutine name");
	}


	// ";"
	Consume(tSemicolon, &dummy);



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

	statSequence(proc);

	Consume(tEnd, &dummy);

}
/*
CAstStatement* CParser::statSequence(CAstScope* s){
	//
	// statSequence = [ statement { ";" statement } ]
	// FIRST(statSequence) = { ident, "if", "while", "return" }
	// FOLLOW(statSequence) = { "end", "else" }
	//
	CAstStatement* head = NULL;

	EToken tt = _scanner->Peek().GetType();
	bool escape=false;

	if( !((tt == tEnd)||(tt == tElse))){
		CAstStatement* tail=NULL;

		do {
			CToken t;
			tt =_scanner->Peek().GetType();
			CAstStatement* st=NULL;

			switch(tt){
				case tIdent:
					st = assignemt_or_subroutinCall(s);
					break;

				case tIf:
					//ifStatement(s);
					break;

				case tWhile:
					//whileStatement(s);
					break;

				case tReturn:
					//returnStatment(s);
					break;

				default:
					SetError(_scanner->Peek(), "statement expected.");
					break;
			}

			assert(st != NULL);
			if ( head ==NULL){
				head =st;
			}
			else{
				tail->SetNext(st);
				tail=st;
			}

			tt= _scanner->Peek().GetType();
			
			if(tt == tEnd){
				break;
			}

			Consume(tSemicolon);
		}while(!_abort);
	}


	return head;
}
*/
CAstStatement* CParser::assignment_or_subroutineCall(CAstScope* s){
	//
	// assignment_or_subroutinCall = ident ...
	// ... = "[" || ":" -> assignment
	//		 = '(" 				-> subroutineCall
	//

	CToken name;
	Consume(tIdent, &name);

	CAstStatement *st=NULL;
	EToken tt = _scanner->Peek().GetType();
	switch(tt){
		case tLBrak:
		case tAssign:
			st = assignment(s, name);
			break;

		case tLParens:
			//subroutineCall(s, name);
			break;

	}

	return st;

}
		
CAstStatement* CParser::subroutineCall_stat(CAstScope* s, CToken a){
	return NULL;

}

CAstFunctionCall* CParser::subroutineCall_expr(CAstScope* s, CToken name){
	return NULL;
}

