//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/05/22 Bernhard Egger reimplemented TAC generation
/// 2013/11/04 Bernhard Egger added typechecks for unary '+' operators
/// 2016/03/12 Bernhard Egger adapted to SnuPL/1
/// 2014/04/08 Bernhard Egger assignment 2: AST for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016 Bernhard Egger
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

#include <iostream>
#include <cassert>
#include <cstring>

#include <typeinfo>

#include "ast.h"
using namespace std;

string OpToStr(const CAstOperation* oper){
	ostringstream out;
	out << oper-> GetOperation();
	return out.str();
}

string TypeToStr(const CType* ct){
	ostringstream out;
	ct->print(out, 0 );
	return out.str();
}


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::GetTacAddr(void) const
{
  return _addr;
}

ostream& operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope* CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;

	try {
		CAstStatement *s = _statseq;
		while (result && (s != NULL)) {
			result = s->TypeCheck(t, msg);
			s = s->GetNext();
		}
		vector<CAstScope*>::const_iterator it = _children.begin();
		while (result && (it != _children.end())) {
			// child scope type check
			result = (*it)->TypeCheck(t, msg);
			it++;
		}
	} catch (...) {
		result = false;
	} 
	if(result)

  return result;
}

ostream& CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent+4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent+4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i=0; i<_children.size(); i++) {
      _children[i]->print(out, indent+4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
	assert(cb != NULL);
	CAstStatement *s = GetStatementSequence();
	while (s != NULL) {
		CTacLabel *next = cb->CreateLabel();
		s->ToTac(cb, next);
		cb->AddInstr(next);
		s = s->GetNext();
	}

	cb->CleanupControlFlow();
	return NULL;

}

CCodeBlock* CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name,
                             CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol)
{
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}


//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const
{
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const
{
  return _next;
}

CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatAssign

CAstStatAssign::CAstStatAssign(CToken t,
                               CAstDesignator *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator* CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const
{
	bool result = true;

	result = (_lhs->TypeCheck(t, msg)) && (_rhs->TypeCheck(t, msg));

	if(result){
		// if lhs is array type
		if( _lhs->GetType()->IsArray()){
			result=false;
			*t=GetToken();
			*msg = "assignments to compound types are not supported.\n  LHS: "+TypeToStr(_lhs->GetType())+"\n  RHS: "+TypeToStr(_rhs->GetType());
		}
		else{
			result= (_lhs->GetType()==_rhs->GetType());

			if(!result){
				*t=GetToken();
				*msg = "incompatible types in assignment:\n  LHS: "+TypeToStr(_lhs->GetType())+"\n  RHS: "+TypeToStr(_rhs->GetType());
			}
		}
	}

  return result;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}

ostream& CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent+2);
  _rhs->print(out, indent+2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	CTacAddr* left_tac=_lhs->ToTac(cb);
	CTacAddr* right_tac=NULL;
	CTacLabel* ltrue=cb->CreateLabel();
	CTacLabel* lfalse=cb->CreateLabel();
	CTacLabel* end_label=cb->CreateLabel();
	if(GetType()->IsBoolean()){

		right_tac=cb->CreateTemp(_rhs->GetType());
		_rhs->ToTac(cb, ltrue, lfalse );

		// ltrue:
		// 				code
		// 				goto end
		cb->AddInstr(ltrue);
		cb->AddInstr(new CTacInstr(opAssign, right_tac, new CTacConst(1)));
		cb->AddInstr( new CTacInstr(opGoto, end_label));
		// lfalse:
		// code
		cb->AddInstr( lfalse);
		cb->AddInstr(new CTacInstr(opAssign, right_tac, new CTacConst(0)));
		// end:
		cb->AddInstr(end_label);
	}
	else{
		right_tac = _rhs->ToTac(cb);
	}

	if(right_tac == NULL){
		cout<<"eroor!!!!!!!\n";
	}

	cb->AddInstr(new CTacInstr(opAssign, left_tac, right_tac));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const
{
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const
{
  return GetCall()->TypeCheck(t, msg);
}

ostream& CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	CTacAddr* cta = GetCall()->ToTac(cb);
	cb->AddInstr(new CTacInstr(opGoto, next));
  return cta;
}


//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const
{
	const CType *st = GetScope()->GetType();
	CAstExpression *e = GetExpression();
	if (st->Match(CTypeManager::Get()->GetNull())) {
		if (e != NULL) {
			if (t != NULL) *t = e->GetToken();
			if (msg != NULL) *msg = "superfluous expression after return.";
			return false;
		}
	}
	else {
		if (e == NULL) {
			if (t != NULL) *t = GetToken();
			if (msg != NULL) *msg = "expression expected after return.";
			return false;
		}
		if (!e->TypeCheck(t, msg)) return false;
		if (!st->Match(e->GetType())) {
			if (t != NULL) *t = e->GetToken();
			if (msg != NULL) *msg = "return type mismatch.";
			return false;
		}
	} 

  return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent+2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	cb->AddInstr( new CTacInstr(opReturn, GetExpression()->ToTac(cb)));
	cb->AddInstr( new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
                       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const
{
	if(!GetCondition()->GetType()->Compare(CTypeManager::Get()->GetBool())){
		*t=GetCondition()->GetToken();
		*msg="boolean expression expected.";
		return false;
	}

  return true;
}

ostream& CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" 
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	CAstExpression* cond=GetCondition();
	CTacLabel* if_true = cb->CreateLabel("if_true");
	CTacLabel* if_false = cb->CreateLabel("if_false");

	cond->ToTac(cb, if_true, if_false);

	// ltrue:
	// 				code
	// 				goto end
	cb->AddInstr(if_true);
	GetIfBody()->ToTac(cb, next);
	cb->AddInstr(new CTacInstr(opGoto, next));
	// lfalse:
	// code
	cb->AddInstr( if_false);
	if(GetElseBody()!=NULL){
		GetElseBody()->ToTac(cb, next);
	}
	// end:

	cb->AddInstr(new CTacInstr(opGoto, next));


  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t,
                             CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const
{
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const
{
	if(!GetCondition()->GetType()->Compare(CTypeManager::Get()->GetBool())){
		*t=GetCondition()->GetToken();
		*msg="boolean expression expected.";
		return false;
	}

  return true;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  }
  else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	CTacLabel* cond_label = cb->CreateLabel("while_cond");
	CTacLabel* body_label = cb->CreateLabel("while_body");

	// while_cond:
	// cond code
	cb->AddInstr( cond_label );
	GetCondition()->ToTac(cb, body_label, next);
	
	// while_body:
	// body code
	cb->AddInstr( body_label);
	GetBody()->ToTac(cb, cond_label);
	cb->AddInstr(new CTacInstr(opGoto, cond_label));

	cb->AddInstr(new CTacInstr(opGoto, next));



  return NULL;
}


//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t)
  : CAstNode(t)
{
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
                           CAstExpression *l,CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd)        || (oper == opSub)         ||
         (oper == opMul)        || (oper == opDiv)         ||
         (oper == opAnd)        || (oper == opOr)          ||
         (oper == opEqual)      || (oper == opNotEqual)    ||
         (oper == opLessThan)   || (oper == opLessEqual)   ||
         (oper == opBiggerThan) || (oper == opBiggerEqual)
        );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const
{
  return _right;
}


bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const
{
	bool result=true;
	CTypeManager* tm =CTypeManager::Get();

	result = (_left->TypeCheck(t, msg)) && (_right->TypeCheck(t, msg));
	if(!result){
		return result;
	}

	switch(GetOperation()){
		case opAdd:
		case opSub:
		case opMul:
		case opDiv:
			result = (_left->GetType()==tm->GetInt())&&(_right->GetType()==tm->GetInt());
			break;

		case opAnd:
		case opOr:
			result = (_left->GetType() == tm->GetBool())&&(_right->GetType()==tm->GetBool());
			break;

		case opEqual:
		case opNotEqual:
			result = (_left->GetType() == _right->GetType());
			break;

		case opLessThan:
		case opLessEqual:
		case opBiggerEqual:
		case opBiggerThan:
			result = (_left->GetType() == _right->GetType()) && ( (_left->GetType()==tm->GetInt())||(_left->GetType()==tm->GetChar()));
			break;

	}
	if(!result){
		*t=GetToken();
		*msg= OpToStr(this)+": type mismatch.\n"+"  left  operand: ";
		*msg+=TypeToStr(_left->GetType())+"\n  right operand: "+TypeToStr(_right->GetType());
	}

  return result;
}

const CType* CAstBinaryOp::GetType(void) const
{
	const CType* ct;

	switch(GetOperation()){
		case opAdd:
		case opSub:
		case opMul:
		case opDiv:
		 ct = CTypeManager::Get()->GetInt();
		 break;

		case opAnd:
		case opOr:
		case opEqual:
		case opNotEqual:
		case opLessEqual:
		case opLessThan:
		case opBiggerThan:
		case opBiggerEqual:
			ct = CTypeManager::Get()->GetBool();
			break;
	}

	return ct;
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _left->print(out, indent+2);
  _right->print(out, indent+2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb)
{
	CTacAddr* left_tac=_left->ToTac(cb);
	CTacAddr* right_tac=_right->ToTac(cb);
	CTacTemp* temp_val=cb->CreateTemp(GetType());
	cb->AddInstr(new CTacInstr(GetOperation(), temp_val, left_tac, right_tac));
  return temp_val;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb,
                              CTacLabel *ltrue, CTacLabel *lfalse)
{
	CTacAddr* left_tac, *right_tac;
	CTacLabel* mid_lab= cb->CreateLabel();
	if( GetOperation()==opAnd){
		// left
		left_tac = _left->ToTac(cb, mid_lab, lfalse);
		
		// mid :
		cb->AddInstr( mid_lab );

		// right
		right_tac = _right->ToTac(cb, ltrue, lfalse);
	}
	else if( GetOperation()==opOr){
		//left
		_left->ToTac(cb, ltrue, mid_lab);

		// mid :
		cb->AddInstr( mid_lab);

		// right
		_right->ToTac(cb, ltrue, lfalse);

	}
	else{
		left_tac=_left->ToTac(cb);
		right_tac= _right->ToTac(cb);

		// if ( expr ) goto ltrue
		cb->AddInstr(new CTacInstr(GetOperation(), ltrue, left_tac, right_tac));
		// goto lfalse
		cb->AddInstr(new CTacInstr(opGoto, lfalse));
	}


	return NULL;
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opPos) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const
{
	bool result;

	result = _operand->TypeCheck(t, msg);
	if(!result){
		return result;
	}

	switch(GetOperation()){
		case opPos:
		case opNeg:
			result = (_operand->GetType() == CTypeManager::Get()->GetInt());
			break;

		case opNot:
			result = (_operand->GetType() == CTypeManager::Get()->GetBool());
			break;
	}
	if(!result){
		*t=GetToken();
		*msg= OpToStr(this)+": type mismatch.\n"+"  operand:      ";
		*msg+=TypeToStr(_operand->GetType());
	}


  return result;
}

const CType* CAstUnaryOp::GetType(void) const
{
	const CType* ct;
	
	switch(GetOperation()){
		case opNot:
			ct = CTypeManager::Get()->GetBool();
			break;

		case opPos:
		case opNeg:
			ct = CTypeManager::Get()->GetInt();
			break;

	}
	return ct;
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb,
                             CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstSpecialOp
//
CAstSpecialOp::CAstSpecialOp(CToken t, EOperation oper, CAstExpression *e,
                             const CType *type)
  : CAstOperation(t, oper), _operand(e), _type(type)
{
  assert((oper == opAddress) || (oper == opDeref) || (oper = opCast));
  assert(e != NULL);
  assert(((oper != opCast) && (type == NULL)) ||
         ((oper == opCast) && (type != NULL)));
}

CAstExpression* CAstSpecialOp::GetOperand(void) const
{
  return _operand;
}

bool CAstSpecialOp::TypeCheck(CToken *t, string *msg) const
{
	if(GetOperation() == opAddress){
		return _operand->TypeCheck(t, msg);
	}
 // return false;
 return true;
}

const CType* CAstSpecialOp::GetType(void) const
{
	if(GetOperation() == opAddress){
		return CTypeManager::Get()->GetPointer(_operand->GetType());
	}
  return NULL;
}

ostream& CAstSpecialOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstSpecialOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstSpecialOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstSpecialOp::ToTac(CCodeBlock *cb)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const
{
  return (int)_arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const
{
	bool result = true;
	if(_symbol->GetNParams() > GetNArgs()){
		*t=GetToken();
		*msg="not enough arguments.";
		result=false;
	}
	else if(_symbol->GetNParams() < GetNArgs()){
		*t=GetToken();
		*msg="too many arguments.";
		result=false;
	}
	else{
		int n = GetNArgs();
		for(int i=0; i<n; i++){
			if(! GetArg(i)->TypeCheck(t, msg) ){
				result=false;
				break;
			}


			const CType* ct= _symbol->GetParam(i)->GetDataType();


			if((ct->IsPointer())&&(!GetArg(i)->GetType()->IsPointer())){
				const CPointerType* cpt=dynamic_cast<const CPointerType*>(ct);
				ct = cpt->GetBaseType();
//	ct= CTypeManager::Get()->GetPointer(ct);
			}


		if(! ct->Match( GetArg(i)->GetType())){
				result=false;
				*t=GetArg(i)->GetToken();
				*msg="parameter "+to_string(i+1)+": argument type mismatch.\n  expected "+TypeToStr(_symbol->GetParam(i)->GetDataType())+"\n  got      "+TypeToStr(GetArg(i)->GetType());
				break;
			}

		}
	}
  return result;
}

const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->print(out, indent+2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb)
{
	int n = GetNArgs();
	CTacAddr* arg_tac;
	for(int i=n-1; i>=0; i--){
		arg_tac=GetArg(i)->ToTac(cb);
		cb->AddInstr( new CTacInstr(opParam, new CTacConst(i), arg_tac));
	}

	cb->AddInstr( new CTacInstr(opCall, new CTacLabel(_symbol->GetName())));

  return NULL;
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb,
                                  CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}



//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t)
{
}


//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol)
  : CAstOperand(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstDesignator::GetType(void) const
{
	/*
	if(GetSymbol()->GetDataType()->IsPointer()){
		const CPointerType* cpt=dynamic_cast<const CPointerType*>(GetSymbol()->GetDataType());
		cpt->GetBaseType()->print(cout, 0);
		cout<<endl;
		return CTypeManager::Get()->GetPointer(cpt->GetBaseType());

	}
	*/
  return GetSymbol()->GetDataType();
}

ostream& CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb)
{
  return new CTacName(GetSymbol());
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstArrayDesignator
//
CAstArrayDesignator::CAstArrayDesignator(CToken t, const CSymbol *symbol)
  : CAstDesignator(t, symbol), _done(false), _offset(NULL)
{
}

void CAstArrayDesignator::AddIndex(CAstExpression *idx)
{
  assert(!_done);
  _idx.push_back(idx);
}

void CAstArrayDesignator::IndicesComplete(void)
{
  assert(!_done);
  _done = true;
}

int CAstArrayDesignator::GetNIndices(void) const
{
  return (int)_idx.size();
}

CAstExpression* CAstArrayDesignator::GetIndex(int index) const
{
  assert((index >= 0) && (index < _idx.size()));
  return _idx[index];
}

bool CAstArrayDesignator::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;

	const CType* ct=_symbol->GetDataType();
	bool isptr=false;

	if(ct->IsPointer()){
		const CPointerType* pt = dynamic_cast<const CPointerType*>(ct);
		ct = pt -> GetBaseType();
		isptr=true;
	}



	if(ct->IsArray()){
		const CArrayType *arrtype=dynamic_cast<const CArrayType*>(ct);

		if(isptr){
			if(GetNIndices()>arrtype->GetNDim()){
				*t = GetToken();
				*msg="invalid array expression.";
				return false;
			}
		}
		else{
			if(GetNIndices() > arrtype->GetNDim()){
				*t = GetToken();
				*msg="invalid array expression.";
				return false;
			}
		}
		int N = GetNIndices();
		for(int i=0; i<N; i++){
			result = GetIndex(i)->TypeCheck(t, msg);
			if(!result){
				return result;
			}

			result = GetIndex(i)->GetType()->Compare(CTypeManager::Get()->GetInt());
			if(!result){
				*t = GetIndex(i)->GetToken();
				*msg="invalid array index expression.";
				return result;
			}

		}

	}
	else{
		*t = GetToken();
		*msg="invalid array expression.";
		return false;
	}

  //assert(_done);

  return result;
}

const CType* CAstArrayDesignator::GetType(void) const
{
	const CType* type=_symbol->GetDataType();
	const CArrayType* arr_type;

	if((type->IsPointer())&&(GetNIndices()>0)){
		const CPointerType* ptr_type=dynamic_cast<const CPointerType*>(type);
		type=ptr_type->GetBaseType();
	}

	for(int i=1; i<=GetNIndices(); i++){
		if(type->IsArray()){
			arr_type=dynamic_cast<const CArrayType*>(type);
			type=arr_type->GetInnerType();
		}
	}

  return type;
}

ostream& CAstArrayDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->print(out, indent+2);
  }

  return out;
}

string CAstArrayDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName() << "[]\",shape=ellipse]";
  return out.str();
}

void CAstArrayDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_idx.size(); i++) {
    _idx[i]->toDot(out, indent);
    out << ind << dotID() << "-> " << _idx[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb)
{
	CTacTemp* temp, *array, *index, *dim, *real_index, *dofs, *pos;
	const CType* int_type=CTypeManager::Get()->GetInt();

	int n = GetNIndices();
	// address of array
	array = cb->CreateTemp(CTypeManager::Get()->GetPointer(GetSymbol()->GetDataType()));
	cb->AddInstr( new CTacInstr(opAddress, array,new CTacName(GetSymbol())));
	// dim = 1
	if(n==1){
		real_index = cb->CreateTemp(int_type);
		cb->AddInstr( new CTacInstr(opMul, real_index, GetIndex(0)->ToTac(cb), new CTacConst( GetType()->GetDataSize())));
	}
	// dim > 1
	else{

		for(int i=1; i<n; i++){
			// call DIM
			// param1 : which dim
			cb->AddInstr( new CTacInstr(opParam, new CTacConst(1), new CTacConst(i+1)));
			// param0 : array pointer
			temp = cb->CreateTemp(CTypeManager::Get()->GetPointer(GetSymbol()->GetDataType()));
			cb->AddInstr( new CTacInstr(opAddress, temp, new CTacName(GetSymbol())));
			cb->AddInstr( new CTacInstr(opParam, new CTacConst(0), temp));
			// call DIM
			dim = cb->CreateTemp(int_type);
			const CSymbol* sym = cb->GetOwner()->GetSymbolTable()->FindSymbol("DIM");
			cb->AddInstr( new CTacInstr(opCall, dim, new CTacName(sym)));
			// mul
			temp = cb->CreateTemp(int_type);
			if(i==1){
				cb->AddInstr( new CTacInstr(opMul, temp, GetIndex(0)->ToTac(cb), dim));
			}
			else{
				cb->AddInstr( new CTacInstr(opMul, temp, index, dim));
			}
			// add
			index = cb->CreateTemp(int_type);
			cb->AddInstr( new CTacInstr(opAdd, index, temp, GetIndex(i)->ToTac(cb)));

		}
		// multiply data size
		real_index = cb->CreateTemp(int_type);
		cb->AddInstr( new CTacInstr(opMul, real_index, index, new CTacConst( GetType()->GetDataSize())));

	}


	// DOFS
	// param 0 : address of array
	temp = cb->CreateTemp(CTypeManager::Get()->GetPointer(GetSymbol()->GetDataType()));
	cb->AddInstr( new CTacInstr(opAddress, temp,new CTacName(GetSymbol())));
	cb->AddInstr( new CTacInstr(opParam, new CTacConst(0), temp));
	// call DOFS
	dofs = cb->CreateTemp(int_type);
	const CSymbol* sym = cb->GetOwner()->GetSymbolTable()->FindSymbol("DOFS");
	cb->AddInstr( new CTacInstr(opCall, dofs, new CTacName(sym)));

	// calculate real position
	temp = cb->CreateTemp(int_type);
	cb->AddInstr( new CTacInstr(opAdd, temp, real_index, dofs));
	pos = cb->CreateTemp(int_type);
	cb->AddInstr( new CTacInstr(opAdd, pos, array, temp));

	// return reference of pos
	CTacReference* ref_pos = new CTacReference(pos->GetSymbol());

  return ref_pos;
}

CTacAddr* CAstArrayDesignator::ToTac(CCodeBlock *cb,
                                     CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value)
{
}

void CAstConstant::SetValue(long long value)
{
  _value = value;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const
{
	if(GetType()->IsInt()){
		if( (_value > 2147483647) || (_value < -2147483648)){
			*t=GetToken();
			*msg="integer constant outside valid range.";
			return false;
		}
	}

  return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb)
{
  return new CTacConst(GetValue());
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStringConstant
//
int CAstStringConstant::_idx = 0;

CAstStringConstant::CAstStringConstant(CToken t, const string value,
                                       CAstScope *s)
  : CAstOperand(t)
{
  CTypeManager *tm = CTypeManager::Get();

  _type = tm->GetArray(strlen(CToken::unescape(value).c_str())+1,
                       tm->GetChar());
  _value = new CDataInitString(value);

  ostringstream o;
  o << "_str_" << ++_idx;

  _sym = new CSymGlobal(o.str(), _type);
  _sym->SetData(_value);
  s->GetSymbolTable()->AddSymbol(_sym);
}

const string CAstStringConstant::GetValue(void) const
{
  return _value->GetData();
}

const string CAstStringConstant::GetValueStr(void) const
{
  return GetValue();
}

bool CAstStringConstant::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstStringConstant::GetType(void) const
{
  return _type;
}

ostream& CAstStringConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << '"' << GetValueStr() << '"' << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstStringConstant::dotAttr(void) const
{
  ostringstream out;
  // the string is already escaped, but dot requires double escaping
  out << " [label=\"\\\"" << CToken::escape(GetValueStr())
      << "\\\"\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstStringConstant::ToTac(CCodeBlock *cb,
                                CTacLabel *ltrue, CTacLabel *lfalse)
{
  return NULL;
}


