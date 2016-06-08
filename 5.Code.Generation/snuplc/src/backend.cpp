//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
/// 2016/04/04 Bernhard Egger adapted to SnuPL/1
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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern DIM" << endl
       << _ind << ".extern DOFS" << endl
       << _ind << ".extern ReadInt" << endl
       << _ind << ".extern WriteInt" << endl
       << _ind << ".extern WriteStr" << endl
       << _ind << ".extern WriteChar" << endl
       << _ind << ".extern WriteLn" << endl
       << endl;

  // TODO
	SetScope(_m);
	const vector<CScope*>& cs =_m->GetSubscopes();
	int child_size=cs.size();
	for(int i=0; i<child_size; i++){
		SetScope(cs[i]);
		EmitScope(cs[i]);
	}
	SetScope(_m);
	EmitScope(_m);

		// forall s in subscopes do
		//   EmitScope(s)
		// EmitScope(program)

		_out << _ind << "# end of text section" << endl
			<< _ind << "#-----------------------------------------" << endl
			<< endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

	
	cout<<scope->GetName()<<endl;
	int size=	ComputeStackOffsets(scope->GetSymbolTable(), 8, 12);
	size-=12;

	
	//
	//emit function prologue
	//
	_out<<endl;
	_out<< _ind <<"# prologue"<<endl;

	//ebp
	EmitInstruction("pushl", "%ebp");
	EmitInstruction("movl", "%esp, %ebp");
	//calling convension
	EmitInstruction("pushl", "%ebx", "save callee saved registers");
	EmitInstruction("pushl", "%esi");
	EmitInstruction("pushl", "%edi");
	EmitInstruction("subl", "$"+to_string(size)+", %esp", "make room for locals");

	if(size==0){

	}
	else if(size<=16){
		_out << endl;
		EmitInstruction("xorl", "%eax, %eax", "memset local stack area to 0");
		for(int i=size-4; i>=0; i-=4){
			EmitInstruction("movl", "%eax, "+to_string(i)+"(%esp)");
		}
	}
	else{
		_out << endl;
		EmitInstruction("cld", "",  "memset local stack area to 0");
		EmitInstruction("xorl", "%eax, %eax");
		EmitInstruction("movl", "$"+to_string(size/4)+", %ecx");
		EmitInstruction("mov", "%esp, %edi");
		EmitInstruction("rep", "stosl");
	}
	vector<CSymbol*> sym_vec, slist;
	slist = scope->GetSymbolTable()->GetSymbols();
	vector<CSymbol*>::iterator it = slist.begin();
	for(; it != slist.end(); ++it){
		if((*it)->GetSymbolType()==stLocal&&(*it)->GetDataType()->IsArray()){
			sym_vec.push_back(*it);
		}
	}
	for(it = sym_vec.begin(); it != sym_vec.end(); ++it){
		const CArrayType* arr = dynamic_cast<const CArrayType*>((*it)->GetDataType());
//		assert(arr!=NULL);
		for(int i=1; i<=arr->GetNDim(); i++){
			EmitInstruction("movl", "$"+to_string(i)+","+to_string((*it)->GetOffset())+"("+(*it)->GetBaseRegister()+")", "local array '"+(*it)->GetName()+"': "+to_string(i)+" dimensions");
			EmitInstruction("movl", "$"+to_string(arr->GetNElem())+","+to_string((*it)->GetOffset()+4)+"("+(*it)->GetBaseRegister()+")", "  dimension "+to_string(i)+": "+to_string(arr->GetNElem())+" elements");
		}
	}
	

	
	//
	// middle
	//
	_out<<endl;
	_out << _ind << "# function body"<<endl;
	EmitCodeBlock(scope->GetCodeBlock());

	//
  // emit function epilogue
	//
	_out<< endl << "l_"+ scope->GetName()+"_exit:"<<endl;
	_out<< _ind << "# epilogue"<<endl;
	EmitInstruction("addl", "$"+to_string(size)+", %esp", "remove locals");
	EmitInstruction("popl", "%edi");
	EmitInstruction("popl", "%esi");
	EmitInstruction("popl", "%ebx");
	EmitInstruction("popl", "%ebp");
	EmitInstruction("ret");

	



  // TODO
  // ComputeStackOffsets(scope)
  //
  // emit function prologue
  //
  // forall i in instructions do
  //   EmitInstruction(i)
  //
  // emit function epilogue

  _out << endl;
}


void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  // emit the globals for the current scope
  CSymtab *st = scope->GetSymbolTable();
  assert(st != NULL);

  bool header = false;

  vector<CSymbol*> slist = st->GetSymbols();

  _out << dec;

  size_t size = 0;

  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];
    const CType *t = s->GetDataType();

    if (s->GetSymbolType() == stGlobal) {
      if (!header) {
        _out << _ind << "# scope: " << scope->GetName() << endl;
        header = true;
      }

      // insert alignment only when necessary
      if ((t->GetAlign() > 1) && (size % t->GetAlign() != 0)) {
        size += t->GetAlign() - size % t->GetAlign();
        _out << setw(4) << " " << ".align "
             << right << setw(3) << t->GetAlign() << endl;
      }

      _out << left << setw(36) << s->GetName() + ":" << "# " << t << endl;

      if (t->IsArray()) {
        const CArrayType *a = dynamic_cast<const CArrayType*>(t);
        assert(a != NULL);
        int dim = a->GetNDim();

        _out << setw(4) << " "
          << ".long " << right << setw(4) << dim << endl;

        for (int d=0; d<dim; d++) {
          assert(a != NULL);

          _out << setw(4) << " "
            << ".long " << right << setw(4) << a->GetNElem() << endl;

          a = dynamic_cast<const CArrayType*>(a->GetInnerType());
        }
      }

      const CDataInitializer *di = s->GetData();
      if (di != NULL) {
        const CDataInitString *sdi = dynamic_cast<const CDataInitString*>(di);
        assert(sdi != NULL);  // only support string data initializers for now

        _out << left << setw(4) << " "
          << ".asciz " << '"' << sdi->GetData() << '"' << endl;
      } else {
        _out  << left << setw(4) << " "
          << ".skip " << dec << right << setw(4) << t->GetDataSize()
          << endl;
      }

      size += t->GetSize();
    }
  }

  _out << endl;

  // emit globals in subscopes (necessary if we support static local variables)
  vector<CScope*>::const_iterator sit = scope->GetSubscopes().begin();
  while (sit != scope->GetSubscopes().end()) EmitGlobalData(*sit++);
}

void CBackendx86::EmitLocalData(CScope *scope)
{
  assert(scope != NULL);

  // TODO TODO!
}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb)
{
  assert(cb != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++);
}

void CBackendx86::EmitInstruction(CTacInstr *i)
{
  assert(i != NULL);

  ostringstream cmt;
  string mnm;
  cmt << i;
	CTacConst* cons;

  EOperation op = i->GetOperation();

  switch (op) {
    // binary operators
    // dst = src1 op src2
    // TODO
		case opAssign:
			Load(i->GetSrc(1), "%eax", cmt.str());
			if( dynamic_cast<const CTacReference*>(i->GetDest()) != NULL){
				//reference
				// load
				Load(dynamic_cast<CTacAddr*>(i->GetDest()), "%edi");

				// make new symbol for %edi
				const CTacReference* ref = dynamic_cast<const CTacReference*>(i->GetDest()); 
				CSymbol *sym = new CSymbol("temp", stLocal, ref->GetDerefSymbol()->GetDataType());
				sym->SetBaseRegister("%edi");
				CTacTemp* temp = new CTacTemp(sym);

				// store
				Store(temp, 'a');
			}
			else{
				Store(i->GetDest(), 'a');
			}
			break;

		case opAdd:
			Load(i->GetSrc(1), "%eax", cmt.str());
			Load(i->GetSrc(2), "%ebx");
			EmitInstruction("addl", "%ebx, %eax");
			Store(i->GetDest(), 'a');
			break;

		case opSub:
			Load(i->GetSrc(1), "%eax", cmt.str());
			Load(i->GetSrc(2), "%ebx");
			EmitInstruction("subl", "%ebx, %eax");
			Store(i->GetDest(), 'a');
			break;

		case opMul:
			Load(i->GetSrc(1), "%eax", cmt.str());
			Load(i->GetSrc(2), "%ebx");
			EmitInstruction("imull", "%ebx");
			Store(i->GetDest(), 'a');
			break;

		case opDiv:
			Load(i->GetSrc(1), "%eax", cmt.str());
			Load(i->GetSrc(2), "%ebx");
			EmitInstruction("cdq");
			EmitInstruction("idivl", "%ebx");
			Store(i->GetDest(), 'a');
			break;

    // unary operators
    // dst = op src1
    // TODO
		case opNeg:
			Load(i->GetSrc(1), "%eax", cmt.str());
			EmitInstruction("negl", "%eax");
			Store(i->GetDest(), 'a');
			break;

    // memory operations
    // dst = src1
    // TODO

    // pointer operations
    // dst = &src1
    // TODO
		case opAddress:
			EmitInstruction("leal", Operand(i->GetSrc(1))+", %eax", cmt.str());
			Store(i->GetDest(), 'a');
			break;

    // dst = *src1
    case opDeref:
      // opDeref not generated for now
      EmitInstruction("# opDeref", "not implemented", cmt.str());
      break;


    // unconditional branching
    // goto dst
    // TODO
		case opGoto:
			EmitInstruction("jmp", Label(dynamic_cast<CTacLabel*>(i->GetDest())), cmt.str());
			break;

    // conditional branching
    // if src1 relOp src2 then goto dst
    // TODO
		case opEqual:
		case opNotEqual:
		case opLessEqual:
		case opLessThan:
		case opBiggerEqual:
		case opBiggerThan:
		{
			const CTacReference* ref = dynamic_cast<const CTacReference*>(i->GetSrc(1)); 
			if(ref != NULL){
				// make new symbol for %edi
				CSymbol *sym = new CSymbol("temp", stLocal, ref->GetDerefSymbol()->GetDataType());
				sym->SetBaseRegister("%edi");
				CTacTemp* temp = new CTacTemp(sym);
				Load(i->GetSrc(1), "%edi");
				Load(temp, "%eax", cmt.str());
			}
			else{
				Load(i->GetSrc(1), "%eax", cmt.str());
			}
			ref = dynamic_cast<const CTacReference*>(i->GetSrc(2));
			if( ref !=NULL){
				// make new symbol for %edi
				CSymbol *sym = new CSymbol("temp", stLocal, ref->GetDerefSymbol()->GetDataType());
				sym->SetBaseRegister("%edi");
				CTacTemp* temp = new CTacTemp(sym);
				Load(i->GetSrc(2), "%edi");
				Load(temp, "%ebx", cmt.str());
			}
			else{
				Load(i->GetSrc(2), "%ebx");
			}
			EmitInstruction("cmpl", "%ebx, %eax");
			EmitInstruction("j"+Condition(op), Label(dynamic_cast<CTacLabel*>(i->GetDest())));
			break;
		}
	

    // function call-related operations
    // TODO
		case opParam:
			if( dynamic_cast<const CTacReference*>(i->GetSrc(1)) != NULL){
				Load(i->GetSrc(1), "%edi");
				EmitInstruction("movl", "(%edi), %eax", cmt.str());
			}
			else{
				Load(i->GetSrc(1), "%eax", cmt.str());
			}
			EmitInstruction("pushl", "%eax");
			break;

		case opCall:
			if(dynamic_cast<CTacLabel*>(i->GetDest())!=NULL){
				// not return
				EmitInstruction("call", Operand(i->GetDest()), cmt.str());
				CTacLabel* lab= dynamic_cast<CTacLabel*>(i->GetDest());
				const CSymbol* sym =_m->GetSymbolTable()->FindSymbol(lab->GetLabel());
				const CSymProc* sym_proc = dynamic_cast<const CSymProc*>(sym);
				int n = sym_proc->GetNParams();
				if(n!=0){
					EmitInstruction("addl", Imm(4*n)+", %esp");
				}
			}
			else{
				// some return
				EmitInstruction("call", Operand(i->GetSrc(1)), cmt.str());
				CTacName* name = dynamic_cast<CTacName*>( i->GetSrc(1));
				const CSymbol* sym = name->GetSymbol();
				const CSymProc* sym_proc = dynamic_cast<const CSymProc*>(sym);
				int n = sym_proc->GetNParams();
				if(n!=0){
					EmitInstruction("addl", Imm(4*n)+", %esp");
				}
				Store(i->GetDest(), 'a');
			}
			break;

		case opReturn:
			Load(i->GetSrc(1), "%eax", cmt.str());
			EmitInstruction("jmp", Label("exit"));
			break;


    // special
    case opLabel:
      _out << Label(dynamic_cast<CTacLabel*>(i)) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;


    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
  }
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

void CBackendx86::Load(CTacAddr *src, string dst, string comment)
{
  assert(src != NULL);

  string mnm = "mov";
  string mod = "l";

  // set operator modifier based on the operand size
  switch (OperandSize(src)) {
    case 1: mod = "zbl"; break;
    case 2: mod = "zwl"; break;
    case 4: mod = "l"; break;
  }

  // emit the load instruction
  EmitInstruction(mnm + mod, Operand(src) + ", " + dst, comment);
}

void CBackendx86::Store(CTac *dst, char src_base, string comment)
{
  assert(dst != NULL);

  string mnm = "mov";
  string mod = "l";
  string src = "%";

  // compose the source register name based on the operand size
  switch (OperandSize(dst)) {
    case 1: mod = "b"; src += string(1, src_base) + "l"; break;
    case 2: mod = "w"; src += string(1, src_base) + "x"; break;
    case 4: mod = "l"; src += "e" + string(1, src_base) + "x"; break;
  }

  // emit the store instruction
  EmitInstruction(mnm + mod, src + ", " + Operand(dst), comment);
}

string CBackendx86::Operand(const CTac *op)
{
  string operand;

  // TODO
  // return a string representing op
  // hint: take special care of references (op of type CTacReference)

	// label
	const CTacLabel* lab = dynamic_cast<const CTacLabel*>(op);
	if(lab !=NULL){
		operand = lab->GetLabel();
		return operand;
	}
	
	// const
	const CTacConst* cons = dynamic_cast<const CTacConst*>(op);
	if(cons != NULL){
		operand = "$"+to_string(cons->GetValue());
		return operand;
	}

	
	// name
	const CTacName* name = dynamic_cast<const CTacName*>(op);
	if(name != NULL){
		// temp
		/*
		const CTacReference* ref = dynamic_cast<const CTacReference*>(name);
		if( ref != NULL){
			//ref
			if( ref->GetSymbol()->GetOffset() != 0){
				operand = to_string(ref->GetSymbol()->GetOffset());
			}
			operand += "("+ref->GetSymbol()->GetBaseRegister()+")";
			return operand;
		}

		const CTacTemp* temp = dynamic_cast<const CTacTemp*>(name);
		if( (temp != NULL)&&(name->GetSymbol()->GetSymbolType()!=stGlobal)){
			// temp
			if( name->GetSymbol()->GetOffset() != 0){
				operand = to_string(name->GetSymbol()->GetOffset());
			}
			operand += "("+name->GetSymbol()->GetBaseRegister()+")";
		}
		else{
			// name
				operand = name->GetSymbol()->GetName();
		}
		*/
		if( (name->GetSymbol()->GetSymbolType()==stLocal)||(name->GetSymbol()->GetSymbolType()==stParam)){
			if( name->GetSymbol()->GetOffset() != 0){
				operand = to_string(name->GetSymbol()->GetOffset());
			}
			operand += "("+name->GetSymbol()->GetBaseRegister()+")";
		}
		else{
			operand = name->GetSymbol()->GetName();
		}

			return operand;
	}
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(const CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
  return "l_" + cs->GetName() + "_" + label->GetLabel();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}

int CBackendx86::OperandSize(CTac *t) const
{
  int size = 4;
	const CType* ct;
	const CTacName* name = dynamic_cast<const CTacName*>(t);
	if(name != NULL){
		const CTacReference* ref= dynamic_cast<const CTacReference*>(name);
		if(ref != NULL){
			//reference
			size =4;
		}
		else{
			ct = name->GetSymbol()->GetDataType();
			if( ct->IsArray()){
				//array
				const CArrayType* arr = dynamic_cast<const CArrayType*>(ct);
				ct = arr->GetBaseType();
			}
			size = ct->GetDataSize();
		}
	}




  // TODO
  // compute the size for operand t of type CTacName
  // Hint: you need to take special care of references (incl. references to pointers!)
  //       and arrays. Compare your output to that of the reference implementation
  //       if you are not sure.

  return size;
}

size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab,
                                        int param_ofs,int local_ofs)
{
  assert(symtab != NULL);
  vector<CSymbol*> slist = symtab->GetSymbols();

	int size = 0;
	vector<CSymbol*> param_list ;

	_out<< _ind << "# stack offsets:"<<endl;
	vector<CSymbol*>::iterator it=slist.begin();
	bool a=true;
	for(; it != slist.end(); ++it){
		// local variable
		if((*it)->GetSymbolType()==stLocal){		
			local_ofs += (*it)->GetDataType()->GetSize();
			if( ((*it)->GetDataType()->GetSize()>1) && (local_ofs % 4!=0)) {
				local_ofs -= local_ofs%4;
				local_ofs += 4;
			}
			(*it)->SetBaseRegister("%ebp");
			(*it)->SetOffset(-local_ofs);
			_out<< _ind<< "#"<< right <<setw(7)<< (*it)->GetOffset()<<"(%ebp)"<<setw(4) << (*it)->GetDataType()->GetSize()<< "  " << *it<<endl;
		}
		// parameter vaiable
		else if( (*it)->GetSymbolType() == stParam){
			(*it)->SetBaseRegister("%ebp");
			CSymParam* pam = dynamic_cast<CSymParam*>(*it);
			(*it)->SetOffset(param_ofs+4*(pam->GetIndex()));
			cout<<(*it)->GetName()<< " "<< pam->GetIndex()<<endl;
			_out<< _ind<< "#"<< right << setw(7) << (*it)->GetOffset()<<"(%ebp)" <<setw(4) << (*it)->GetDataType()->GetDataSize()<< "  " << *it<<endl;
		}
	}

  // TODO
  // foreach local symbol l in slist do
  //   compute aligned offset on stack and store in symbol l
  //   set base register to %ebp
  //
  // foreach parameter p in slist do
  //   compute offset on stack and store in symbol p
  //   set base register to %ebp
  //
  // align size
  //
  // dump stack frame to assembly file

	if(local_ofs % 4 !=0){
		local_ofs -= local_ofs%4;
		local_ofs += 4;
	}


  return local_ofs;
}
