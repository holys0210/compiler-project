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
	CSymProc* sym_proc;
	CSymParam* param;

	//
	// DIM(array: ptr to array, dim:integer) : integer
	//
	sym_proc= new CSymProc("DIM", tm->GetInt());
	param = new CSymParam(0, "array", tm->GetPointer(tm->GetNull()));
	sym_proc->AddParam(param);
	param = new CSymParam(1, "dim", tm->GetInt());
	sym_proc->AddParam(param);
	s->AddSymbol(sym_proc);

	//
	//  DOFS(array: ptr to array) : integer
	//
	sym_proc= new CSymProc("DOFS", tm->GetInt());
	param = new CSymParam(0, "array", tm->GetPointer(tm->GetNull()));
	sym_proc->AddParam(param);
	s->AddSymbol(sym_proc);

	//
	// ReadInt() : integer
	//
	sym_proc= new CSymProc("ReadInt", tm->GetInt());
	s->AddSymbol(sym_proc);

	// 
	// WriteInt(i: integer):
	//
	sym_proc= new CSymProc("WriteInt", tm->GetNull());
	param = new CSymParam(0, "i", tm->GetInt());
	sym_proc->AddParam(param);
	s->AddSymbol(sym_proc);

	//
	// WriteChar(c: char):
	//
	sym_proc= new CSymProc("WriteChar", tm->GetNull());
	param = new CSymParam(0, "c", tm->GetChar());
	sym_proc->AddParam(param);
	s->AddSymbol(sym_proc);

	//
	// WriteStr(string: char[])
	//
	sym_proc= new CSymProc("WriteStr", tm->GetNull());
	param = new CSymParam(0, "string", tm->GetPointer(tm->GetArray(-1, tm->GetChar())));
	sym_proc->AddParam(param);
	s->AddSymbol(sym_proc);

	//
	// WriteLn()
	//
	sym_proc= new CSymProc("WriteLn", tm->GetNull());
	s->AddSymbol(sym_proc);

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
	InitSymbolTable(m->GetSymbolTable());
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
				st = ifStatement(s);
				break;

			case tWhile:
				st = whileStatement(s);
				break;

			case tReturn:
				st = returnStatment(s);
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

	const CType* ctype=sym->GetDataType();

	if((ctype->IsArray())||(ctype->IsPointer())){
		//array
			
		CAstArrayDesignator* design=new CAstArrayDesignator(name, sym);
		tt=_scanner->Peek().GetType();
		while(tt==tLBrak){
			Consume(tLBrak);
			CAstExpression* expr=expression(s);
			Consume(tRBrak);
			design->AddIndex(expr);

			tt=_scanner->Peek().GetType();
		}
		return design;

	}
	else{
		// non-array
		return new CAstDesignator(name, sym);
	}

}



CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpression ].
	// FIRST(expression) = { "+", "-", ident, number, boolean, char, string, "(", "procedure, "fuction", "!"
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
	CAstConstant* con=NULL;

	if(tt==tPlusMinus){
		CToken sign;
		Consume(tPlusMinus, &sign);
		CAstExpression* m =term(s);

		con=dynamic_cast<CAstConstant*>(m);
		if(con==NULL){
			n = new CAstUnaryOp(sign, sign.GetValue() == "+" ? opPos : opNeg, m);
		}
		else{
			if(con->GetType()==CTypeManager::Get()->GetInt()){
				long long v = con->GetValue();
				con->SetValue(-v);
				n = con;
			}
			else{
				n = new CAstUnaryOp(sign, sign.GetValue() == "+" ? opPos : opNeg, m);
			}
		}




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
		case tBoolConst:
			n = boolean();
			break;

		// factor ::= character
		case tCharConst:
			n = character();
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
			n = new CAstUnaryOp(t, opNot, factor(s));
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
	Consume(tBoolConst, &t);
	CAstConstant* constant=NULL;

	if((t.GetValue()=="true")){
		constant=new CAstConstant(t, CTypeManager::Get()->GetBool(), 1);
	}
	else{
		constant=new CAstConstant(t, CTypeManager::Get()->GetBool(), 0);
	}
	return constant;
}

CAstConstant* CParser::character(void){
	//
	// ascii code value
	//
	CToken t;
	Consume(tCharConst, &t);
	const char* c=t.GetValue().c_str();
	CAstConstant* con= new CAstConstant(t, CTypeManager::Get()->GetChar(), c[0]);
	if(con==NULL){
		SetError(t, "can't create const char");
	}
	return con;
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
		const CType* var_type=type();

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

const CType* CParser::type(){
		CToken dim;
		EToken tt=_scanner->Peek().GetType();
		const CType* ct;
		CTypeManager* tm=CTypeManager::Get();
		switch(tt){
			case tInteger:
				Consume(tt);
				ct = tm->GetInt();
				break;
			case tBoolean:
				Consume(tt);
				ct = tm->GetBool();
				break;
			case tChar:
				Consume(tt);
				ct = tm->GetChar();
				break;

			default:
			//TODO
				//error (temporary handling error)
				Consume(tInteger, &dim);
				SetError(dim, "invalid token for type");
		}
		//TODO
		// array
		vector<int> dim_stack;
		int d;
		while(_scanner->Peek().GetType()==tLBrak){
			Consume(tLBrak);
			if(_scanner->Peek().GetType()==tNumber){
				Consume(tNumber, &dim);
				d=stoi(dim.GetValue());
			}
			else{
				d=-1;
			}
			Consume(tRBrak);

			dim_stack.push_back(d);

		}

		for(vector<int>::reverse_iterator it=dim_stack.rbegin(); it != dim_stack.rend(); ++it){
			ct = tm->GetArray(*it, ct);
		}

		return ct;


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

	CAstStatement* statseq=subroutineBody(proc);
	proc->SetStatementSequence(statseq);

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

	// add subroutin name into symbol table
	CSymtab* symtab=m->GetSymbolTable();
	bool flag=	symtab->AddSymbol(sym_proc);

	if(!flag){
		//already exist
		SetError(name, "Another symbol with same name already exist");
	}


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

	const CType* var_type=type();

	Consume(tSemicolon, &dummy);

	//change return type
	sym_proc->SetDataType(var_type);

	
	// print for check
	//proc->GetSymbol()->print(cout, 4);
	//

	// add subroutin name into symbol table
	CSymtab* symtab=m->GetSymbolTable();
	bool flag=	symtab->AddSymbol(sym_proc);

	if(!flag){
		//already exist
		SetError(name, "Another symbol with same name already exist");
	}

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
		varDeclSequence(proc, true);
	}

	Consume(tRParens, &dummy);
}


void CParser::varDeclSequence(CAstProcedure* proc, bool is_para){
	//
	// varDeclSequence = varDecl { ";" vrDecl }
	//

	CToken dummy;
	varDecl(proc, is_para);

	while(_scanner->Peek().GetType() == tSemicolon){
		Consume(tSemicolon, &dummy);
		varDecl(proc, is_para);
	}

}

void CParser::varDecl(CAstProcedure* proc, bool is_para){
	//
	// varDecl = ident { "," ident } ":" type
	//

	CToken id_token;
	vector<CToken> id;
	EToken  tt;

	while(1){
		Consume(tIdent, &id_token);
		id.push_back(id_token);

		if(_scanner->Peek().GetType() == tColon){
			Consume(tColon);
			break;
		}
		Consume(tComma);
	}
	
	const CType* var_type=type();
	//array
	if(var_type->IsArray()){
		var_type=CTypeManager::Get()->GetPointer(var_type);
	}
	CSymtab* symtab = proc->GetSymbolTable();
	CSymProc* symproc = proc->GetSymbol();
	CSymbol* sym=NULL;

	int i=0;
	for(vector<CToken>::iterator it=id.begin(); it != id.end(); ++it){
		if(is_para){
			sym=new CSymParam(i, it->GetValue(), var_type);
		}
		else{
			sym=proc->CreateVar(it->GetValue(), var_type);
		}

		symtab->AddSymbol(sym);
		symproc->AddParam(new CSymParam(i++, it->GetValue(), var_type));
	}

}

CAstStatement* CParser::subroutineBody(CAstProcedure* proc){
	//
	// subroutineBody = varDeclaration "begin" statSequence "end"
	//
	varDeclaration(proc);

	CToken dummy;

	Consume(tBegin, &dummy);

	CAstStatement* statseq=statSequence(proc);

	Consume(tEnd, &dummy);

	return statseq;

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
			st = subroutineCall_stat(s, name);
			break;

	}

	return st;

}
		
CAstStatement* CParser::subroutineCall_stat(CAstScope* s, CToken a){
	CAstFunctionCall* func_call=subroutineCall_expr(s, a);

	return new CAstStatCall(a, func_call);


}

CAstFunctionCall* CParser::subroutineCall_expr(CAstScope* s, CToken name){
	//
	// subroutineCall_expr = ident "(" [ expression { "," expression } ] ")"
	//

	Consume(tLParens);
	EToken tt=_scanner->Peek().GetType();
	
	// create CAstFunctionCall
	CSymtab* symtab=s->GetSymbolTable();
	const CSymbol* sym=symtab->FindSymbol(name.GetValue());

	if(sym==NULL){
		SetError(name, "no procedure is matched");
	}
	const CSymProc* symproc=dynamic_cast<const CSymProc* >(sym);

	CAstFunctionCall* func_call = new CAstFunctionCall(name, symproc);

	// argument
	if((tt==tPlusMinus)||(tt==tIdent)||(tt=tNumber)||(tt==tBoolConst)||(tt==tCharConst)||(tt==tString)||(tt==tLParens)||(tt==tNot)){
		while(1){
			CAstExpression* exp=expression(s);

			CAstArrayDesignator* arr_dsg=dynamic_cast<CAstArrayDesignator*>(exp);
			if(arr_dsg!=NULL){
				exp=new CAstSpecialOp(exp->GetToken(), opAddress, exp);
			}
			func_call->AddArg(exp);
			tt=_scanner->Peek().GetType();
			if(tt!=tComma){
				break;
			}
			Consume(tComma);
		}
	}
	
	Consume(tRParens);

	return func_call;


	/*
	size_t child_num = s->GetNumChildren();
	string a=name.GetValue();
	CAstProcedure* child;
	CSymProc* symproc=NULL;
	for(size_t i=0; i<child_num; i++){
		child = s->GetChild(i);
		if(child->GetName()==a){
			symproc=child->GetSymbol();
			break;
		}
	}

	if(symproc==NULL){
		SetError(name, "No procedure is matched");
	}

	return new CAstFunctionCall(name, symproc);

	}
	*/
	return NULL;
}

CAstStatement* CParser::ifStatement(CAstScope* s){
	//
	// ifStatement = "if" "(" expression ")" "then" statSequence
	// 								[ "else" statSequence ] "end"
	//
	CToken if_token;
	
	// if
	Consume(tIf, &if_token);

	// condition
	Consume(tLParens);

	CAstExpression* cond=expression(s);

	Consume(tRParens);
	
	// then
	Consume(tThen);

	CAstStatement* ifBody = statSequence(s);
	CAstStatement* elseBody = NULL;

	if(_scanner->Peek().GetType()==tElse){
		//else
		Consume(tElse);
		elseBody = statSequence(s);
	}

	Consume(tEnd);

	return new CAstStatIf(if_token, cond, ifBody, elseBody);
}

CAstStatement* CParser::whileStatement(CAstScope* s){
	//
	// whileStatement = "while" "(" expression ")" "do" statSequence "end"
	//

	CToken while_token;

	Consume(tWhile, &while_token);

	Consume(tLParens);

	CAstExpression* cond = expression(s);

	Consume(tRParens);

	Consume(tDo);

	CAstStatement* body = statSequence(s);

	Consume(tEnd);

	return new CAstStatWhile(while_token, cond, body);
}

CAstStatement* CParser::returnStatment(CAstScope* s){
	//
	// returnStatment = "return" [ expression ]
	// FOLLOW(returnStatment) = {"end"}
	//


	CToken return_token;

	Consume(tReturn, &return_token);

	EToken tt=_scanner->Peek().GetType();

	CAstExpression* expr=NULL;

	if((tt==tPlusMinus)||(tt==tIdent)||(tt==tNumber)||(tt==tBoolConst)||(tt==tChar)||(tt==tString)||(tt==tLParens)||(tt==tProcedure)||(tt==tFunction)||(tt==tNot)){
		expr=expression(s);
	}

	// type check
	// not implemented in reference

	return new CAstStatReturn(return_token, s, expr);
}



