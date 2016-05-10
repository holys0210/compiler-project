//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2016/03/09 Bernhard Egger adapted to SnuPL/!
/// 2016/04/08 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#ifndef __SnuPL_PARSER_H__
#define __SnuPL_PARSER_H__

#include "scanner.h"
#include "symtab.h"
#include "ast.h"


//------------------------------------------------------------------------------
/// @brief parser
///
/// parses a module
///
class CParser {
  public:
    /// @brief constructor
    ///
    /// @param scanner  CScanner from which the input stream is read
    CParser(CScanner *scanner);

    /// @brief parse a module
    /// @retval CAstNode program node
    CAstNode* Parse(void);

    /// @name error handling
    ///@{

    /// @brief indicates whether there was an error while parsing the source
    /// @retval true if the parser detected an error
    /// @retval false otherwise
    bool HasError(void) const { return _abort; };

    /// @brief returns the token that caused the error
    /// @retval CToken containing the error token
    const CToken* GetErrorToken(void) const;

    /// @brief returns a human-readable error message
    /// @retval error message
    string GetErrorMessage(void) const;
    ///@}

  private:
    /// @brief sets the token causing a parse error along with a message
    /// @param t token causing the error
    /// @param message human-readable error message
    void SetError(CToken t, const string message);

    /// @brief consume a token given type and optionally store the token
    /// @param type expected token type
    /// @param token If not null, the consumed token is stored in 'token'
    /// @retval true if a token has been consumed
    /// @retval false otherwise
    bool Consume(EToken type, CToken *token=NULL);


    /// @brief initialize symbol table @a s with predefined procedures and
    ///        global variables
    void InitSymbolTable(CSymtab *s);

    /// @name methods for recursive-descent parsing
    /// @{


		/// @brief module = "module" ident ";" varDeclation { subroutineDecl } "begin" statSequence "end" ident "."
		/// @retval CAstModule module node
    CAstModule*       module(void);

		/// @brief statSequence = [ statement { "," statement } ]
		/// @param s current scope
		/// @retval CAstStatement statement sequence
    CAstStatement*    statSequence(CAstScope *s);

		/// @brief assignment = qualident ":=" expression
		/// @param s current scope
		/// @param name token name (lhs)
		/// @retval CAstStatAssign assignment statement node
    CAstStatAssign*   assignment(CAstScope *s, CToken name);

		/// @brief expression = simpleexpr [ relOp simpleexpr ]
		/// @param s current scope
		/// @retval CAstExpression expression node
    CAstExpression*   expression(CAstScope *s);
		
		/// @brief simpleexpr = ["+"|"-"] term { termOp term }
		/// @param s current scope
		/// @retval CAstExpression expression node
    CAstExpression*   simpleexpr(CAstScope *s);

		/// @brief term = factor { factOp factor }
		/// @param s current scope
		/// @retval CAstExpression expression node
    CAstExpression*   term(CAstScope *s);

		/// @brief factor = qualident | number | boolean | char | string | "(" expression ")" | subroutineCall | "!" factor
		/// @param s current scope
		/// @retval CAstExpression expression node
    CAstExpression*   factor(CAstScope *s);

		/// @brief number = digit { digit }
		/// @retval CAstConstant positive long long number node
    CAstConstant*     number(void);

		/// @brief boolean = "true" | "false"
		/// @retval CAstConstant containing 1 if true
		/// @retval CAstConstant containing 0 if false
		CAstConstant*			boolean(void);

		/// @brief character
		/// @retval CAstConstant containing ASCII code value corresponding character
		CAstConstant*			character(void);

		/// @brief varDeclation = [ "var" varDeclSequence ";" ]
		/// @param CAstScope current scope
		void 							varDeclaration(CAstScope* m);

		/// @brief type = basetype | type "[" [ number ] "]"
		/// @retval CType
		const CType*			type(bool no_open_addr);

		/// @brief subroutineDecl = ( procedureDecl | functionDecl ) subroutineBody ident ";"
		/// @param CAstModule current module
		void 							subroutineDecl(CAstModule* m);

		/// @brief procedureDecl = "procedure" ident [ formalParam ] ";"
		/// @param CAstModule current module
		/// @retval CAstProcedure procedure node
		CAstProcedure*		procedureDecl(CAstModule* m);

		/// @brief functionDecl = "function" ident [ formalParam ] ":" type ";"
		/// @param CAstModule current module
		/// @retval CAstProcedure function node
		CAstProcedure*		functionDecl(CAstModule* m);

		/// @brief formalParam = "(" [ varDeclSequence ] ")"
		/// @param CAstProcedure current subroutine
		void 							formalParam(CAstProcedure* proc);

		/// @brief varDeclSequence = varDecl { ";" varDecl }
		/// @param proc current subroutine
		/// @param is_para true if parameter false otherwise
		void 							varDeclSequence(CAstProcedure* proc, bool is_para);

		/// @brief varDecl = ident { "," ident } ":" type
		/// @param proc current subroutine
		/// @param is_para true if parameter false otherwise
		void 							varDecl(CAstProcedure* proc, bool is_para);

		/// @brief subroutineBody = varDeclaration "begin" statSequence "end"
		/// @param proc current subroutine
		/// @retval CAstStatement subroutine statement node
		CAstStatement*			subroutineBody(CAstProcedure* proc);

		/// @brief assignment_or_subroutineCall = assignment | subroutineCall
		/// @param s current scope
		/// @retval CAstStatement assignment or subroutineCall statement node
		CAstStatement*				assignment_or_subroutineCall(CAstScope* s);

		/// @brief subroutineCall_stat : statement node by calling subroutineCall_expr
		/// @param s current scope
		/// @param name subroutine name
		/// @retval CAstStatement subroutineCall statment node
		CAstStatement*		subroutineCall_stat(CAstScope* s, CToken name);

		/// @brief qualident = ident { "[" expression "]" }
		/// @param s current scope
		/// @param name lhs
		/// @retval CAstDesignator designator node
		CAstDesignator*		qualident(CAstScope* s, CToken name);

		/// @brief subroutineCall_expr = ident "(" [ expression { "," expression } ] ")"
		/// @param s current scope
		/// @param name subroutine name
		/// @retval CAstFunctionCall subroutineCall expression node
		CAstFunctionCall*	subroutineCall_expr(CAstScope* s, CToken name);

		/// @brief ifStatement = "if" "(" expression ")" "then" statSequence [ "else" statSequence ] "end"
		/// @param s current scope
		/// @retval CAstStatement ifstatement node
		CAstStatement*		ifStatement(CAstScope* s);

		/// @brief whileStatement = "while" "(" expression ")" "do" statSequence "end"
		/// @param s current scope
		/// @retval CAstStatement whilestatment node
		CAstStatement*		whileStatement(CAstScope* s);

		/// @brief returnStatment = "return" [ expression ]
		/// @param s current scope
		/// @return CAstStatement returnstatement node
		CAstStatement*		returnStatment(CAstScope* s);

    /// @}


    CScanner     *_scanner;       ///< CScanner instance
    CAstModule   *_module;        ///< root node of the program
    CToken        _token;         ///< current token

    /// @name error handling
    CToken        _error_token;   ///< error token
    string        _message;       ///< error message
    bool          _abort;         ///< error flag

};

#endif // __SnuPL_PARSER_H__
