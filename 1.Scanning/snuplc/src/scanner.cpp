//------------------------------------------------------------------------------
/// @brief SnuPL/0 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
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

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 16

char ETokenName[][TOKEN_STRLEN] = {
  "tPlusMinus",                     ///< '+' or '-'
  "tMulDiv",                        ///< '*' or '/'
  "tRelOp",                         ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tDot",                           ///< a dot
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket

	"tIdent",
	"tNumber",
	"tCharacter",
	"tString",

	"tComment",

	"tAnd",
	"tOr",
	"tNot",
	"tLSqBrak",
	"tRSqBrak",

	"tComma",
	"tColon",

	//keyword
	"tModule",
	"tProcedure",
	"tFunction",
	"tVar",
	"tInteger",
	"tBoolean",
	"tChar",
	"tBegin",
	"tEnd",
	"tIf",
	"tThen",
	"tElse",
	"tWhile",
	"tDo",
	"tReturn",
	"tTrue",
	"tFalse",
	
  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined",                     ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tPlusMinus (%s)",                ///< '+' or '-'
  "tMulDiv (%s)",                   ///< '*' or '/'
  "tRelOp (%s)",                    ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tDot",                           ///< a dot
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket

	"tIdent (%s)",
	"tNumber (%s)",
	"tCharacter (%s)",
	"tString (%s)",
	
	"tComment",
	
	"tAnd",
	"tOr",
	"tNot",
	"tLSqBrak",
	"tRSqBrak",
	
	"tComma",
	"tColon",

	//keyword
	"tModule",
	"tProcedure",
	"tFunction",
	"tVar",
	"tInteger",
	"tBoolean",
	"tChar",
	"tBegin",
	"tEnd",
	"tIf",
	"tThen",
	"tElse",
	"tWhile",
	"tDo",
	"tReturn",
	"tTrue",
	"tFalse",

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)",                ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
{
	make_pair("module", tModule),
	make_pair("procedure", tProcedure),
	make_pair("function", tFunction),
	make_pair("var", tVar),
	make_pair("integer", tInteger),
	make_pair("boolean", tBoolean),
	make_pair("char", tChar),
	make_pair("begin", tBegin),
	make_pair("end", tEnd),
	make_pair("if", tIf),
	make_pair("then", tThen),
	make_pair("else", tElse),
	make_pair("while", tWhile),
	make_pair("do", tDo),
	make_pair("return", tReturn),
	make_pair("true", tTrue),
	make_pair("false", tFalse)
};



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  _value = escape(value);
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const
{
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(const string text)
{
  const char *t = text.c_str();
  string s;

  while (*t != '\0') {
    switch (*t) {
      case '\n': s += "\\n";  break;
      case '\t': s += "\\t";  break;
      case '\0': s += "\\0";  break;
      case '\'': s += "\\'";  break;
      case '\"': s += "\\\""; break;
      case '\\': s += "\\\\"; break;
      default :  s += *t;
    }
    t++;
  }

  return s;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
	while(_token->GetType()==tComment){
		_token = Scan();
	}
}

void CScanner::RecordStreamPosition()
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

/// @brief check if a character is alphabet or '_'
///
/// @param c character
/// @retval true character is alphabet or '_'
/// @retval false character is not alphabet or '_'
bool isletter(char c){
	return (('A'<=c) && (c <= 'Z')) || (('a' <= c) && (c <= 'z')) || (c == '_');
}

/// @brief check if a character is digit
///
/// @param c character
/// @retval true character is digit
/// @retval false character is not digit 
bool isdigit(char c){
	return (('0' <= c) && (c <= '9'));
}

/// @brief check if a character is correct after '\'
///
/// @param c character
/// @retval true character is 'n', 't', '\', ''', '"', '0'
/// @retval false character is other character
bool isAfterBS(char c){
	return (c =='n')||(c=='t')||(c=='"')||(c=='\'')||(c=='\\')||(c=='0');
}

CToken* CScanner::Scan()
{
  EToken token;
  string tokval;
  char c;

  while (_in->good() && IsWhite(_in->peek())) GetChar();

  RecordStreamPosition();

  if (_in->eof()) return NewToken(tEOF);
  if (!_in->good()) return NewToken(tIOError);

  c = GetChar();
  tokval = c;
  token = tUndefined;

  switch (c) {
    case ':':
      if (_in->peek() == '=') {
        tokval += GetChar();
        token = tAssign;
      }
			else{
				token = tColon;
			}
      break;

    case '+':
    case '-':
      token = tPlusMinus;
      break;

    case '*':
			token = tMulDiv;
			break;
    case '/':
			c = _in->peek();
			if(c=='/'){
				//comment
				GetChar();
				c=_in->peek();
				while(c!='\n'){
					if(c=='^'){
						GetChar();
					}
					GetChar();
					c=_in->peek();
				}
				token = tComment;

				break;
				//continue
			}
			else{
				token = tMulDiv;
				break;
			}

    case '=':
    case '#':
      token = tRelOp;
      break;

		case '<':
		case '>':
			token = tRelOp;
			c =_in->peek();
			if(c=='='){
				tokval += GetChar();
			}
			break;

		case '&':
			c = _in->peek();
			if(c == '&'){
				tokval += GetChar();
				token = tAnd;
			}
			break;

		case '|':
			c = _in->peek();
			if(c == '|'){
				tokval += GetChar();
				token = tOr;
			}
			break;

		case '!':
			token = tNot;
			break;

		case '[':
			token = tLSqBrak;
			break;

		case ']':
			token = tRSqBrak;
			break;

    case ';':
      token = tSemicolon;
      break;

    case '.':
      token = tDot;
      break;

		case ',':
			token = tComma;
			break;

    case '(':
      token = tLBrak;
      break;

    case ')':
      token = tRBrak;
      break;

		case '\'':
		{
			string temp_tokval;
			c = _in->peek();
			// case for \n, \t ...
			if(c == '\\'){
				temp_tokval = '\\';
				tokval += c;

				// check char after '\'
				GetChar();
				c = _in->peek();
				if(c=='\''){
					// input is '\'
					tokval=temp_tokval;
					token=tCharacter;
					GetChar();
					break;
				}
				else if(isAfterBS(c)){
					temp_tokval += c;
					tokval += c;
				}
				else{
					//undefined for 'not character ..	

					temp_tokval=tokval;
					tokval = "invalid character after left backslash \"";
					tokval += temp_tokval;
					tokval += "\"";

					break;
				}
			}
			// case for ASCII char
			else{
				temp_tokval = c;
				tokval += c;
			}

			//check right '
			GetChar();
			c = _in->peek();
			if(c=='\''){
				tokval=temp_tokval;
				token=tCharacter;
				GetChar();
			}
			else{
				//undefined for not match apostrophe
					temp_tokval=tokval;
					tokval = "not match right single quote \"";
					tokval += temp_tokval;
					tokval += "\"";

					break;

			}
			break;
	}


	case '\"':
	{
		string temp_tokval;
		c = _in->peek();
		while(1){
			if(c=='\"'){
				//end of string
				tokval=temp_tokval;
				token=tString;
				GetChar();
				break;
			}
			else if(c==EOF){
				temp_tokval=tokval;
				tokval = "not match right double quote \"";
				tokval += temp_tokval;
				tokval += "\"";
				break;
			}
			else{
				// meet ASCII character
				tokval +=c;
				temp_tokval+=c;
			}
			GetChar();
			c = _in->peek();

		}
		break;
	}



    default:
			if(isletter(c)){
				//id or keyword
				// find until inputstream meet not letter or digit
				c = _in->peek();
				while(isletter(c)||isdigit(c)){
					tokval += c;
					GetChar();
					c = _in->peek();
				}

				//check string that it is a keyword
				map<string, EToken>::iterator it;
				it = keywords.find(tokval);
				if(it != keywords.end()){
					token=it->second;
				}
				else{
					token = tIdent;
				//	keywords[tokval]=token;
				}
			}
			else if(isdigit(c)){
				//number
				c = _in->peek();
				while(isdigit(c)){
					tokval += c;
					GetChar();
					c = _in->peek();
				}
				token = tNumber;

			}
			else{
				//undefined
        tokval = "invalid character '";
        tokval += c;
        tokval += "'";
			}

		/* previous code
      if (('0' <= c) && (c <= '9')) {
        token = tDigit;
      } else
      if (('a' <= c) && (c <= 'z')) {
        token = tLetter;
      } else {
        tokval = "invalid character '";
        tokval += c;
        tokval += "'";
      }
      break;
			*/
  }

  return NewToken(token, tokval);
}

char CScanner::GetChar()
{
  char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsWhite(char c) const
{
  return ((c == ' ') || (c == '\n') || (c == '\t'));
}
