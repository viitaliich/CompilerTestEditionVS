Decl* parse_decl_opt(void);
Decl* parse_decl(void);
Typespec* parse_type(void);
Stmt* parse_stmt(void);
Expr* parse_expr(void);

Typespec* parse_type_func(void) {
	Typespec** args = NULL;
	expect_token(TOKEN_LPAREN);
	expect_token(TOKEN_RPAREN);
	Typespec* ret = NULL;
	if (match_token(TOKEN_COLON)) {
		ret = parse_type();
	}
	return typespec_func(args, buf_len(args), ret);
}

Typespec* parse_type_base(void) {
	if (is_token(TOKEN_NAME)) {
		const char* name = token.name;
		next_token();
		return typespec_name(name);
	}
	else if (match_keyword(func_keyword)) {
		return parse_type_func();
	}
	else if (match_token(TOKEN_LPAREN)) {
		Typespec* type = parse_type();
		expect_token(TOKEN_RPAREN);
		return type;
	}
	else {
		fatal_syntax_error("Unexpected token %s in type", token_info());
		return NULL;
	}
}

Typespec* parse_type(void) {
	Typespec* type = parse_type_base();
	return type;
}

Expr* parse_expr_operand(void) {
	if (is_token(TOKEN_INT)) {
		int64_t val = token.int_val;
		next_token();
		return expr_int(val);
	}
	else if (is_token(TOKEN_FLOAT)) {
		double val = token.float_val;
		next_token();
		return expr_float(val);
	}
	else {
		fatal_syntax_error("Unexpected token %s in expression", token_info());
		return NULL;
	}
}

Expr* parse_expr_base(void) {
	Expr* expr = parse_expr_operand();
	return expr;
}

Expr* parse_expr_unary(void) {
		return parse_expr_base();
	}
Expr* parse_expr_mul(void) {
	Expr* expr = parse_expr_unary();
	return expr;
}
Expr* parse_expr_add(void) {
	Expr* expr = parse_expr_mul();
	return expr;
}

Expr* parse_expr_cmp(void) {
	Expr* expr = parse_expr_add();
	return expr;
}
Expr* parse_expr_and(void) {
	Expr* expr = parse_expr_cmp();
	return expr;
}
Expr* parse_expr_or(void) {
	Expr* expr = parse_expr_and();
	return expr;
}

Expr* parse_expr_ternary(void) {
	Expr* expr = parse_expr_or();
	return expr;
}

Expr* parse_expr(void) {
	return parse_expr_ternary();
}

Expr* parse_paren_expr(void) {
	expect_token(TOKEN_LPAREN);
	Expr* expr = parse_expr();
	expect_token(TOKEN_RPAREN);
	return expr;
}

StmtList parse_stmt_block(void) {
	expect_token(TOKEN_LBRACE);
	Stmt** stmts = NULL;
	while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
		buf_push(stmts, parse_stmt());
	}
	expect_token(TOKEN_RBRACE);
	return stmt_list(stmts, buf_len(stmts));
}

Stmt* parse_stmt(void) {
	if (is_token(TOKEN_LBRACE)) {
		return stmt_block(parse_stmt_block());
	}
	
	else if (match_keyword(return_keyword)) {
		Expr* expr = NULL;
		if (!is_token(TOKEN_SEMICOLON)) {
			expr = parse_expr();
		}
		expect_token(TOKEN_SEMICOLON);
		return stmt_return(expr);
	}
}

const char* parse_name(void) {
	const char* name = token.name;
	expect_token(TOKEN_NAME);
	return name;
}

FuncParam parse_decl_func_param(void) {
	const char* name = parse_name();
	expect_token(TOKEN_COLON);
	Typespec* type = parse_type();
	return (FuncParam) { name, type };
}

Decl* parse_decl_func(void) {
	const char* name = parse_name();
	expect_token(TOKEN_LPAREN);
	FuncParam* params = NULL;
	if (!is_token(TOKEN_RPAREN)) {
		buf_push(params, parse_decl_func_param());
	}
	expect_token(TOKEN_RPAREN);
	Typespec* ret_type = NULL;
	if (match_token(TOKEN_COLON)) {
		ret_type = parse_type();
	}
	StmtList block = parse_stmt_block();
	return decl_func(name, params, buf_len(params), ret_type, block);
}

Decl* parse_decl_opt(void) {
if (match_keyword(func_keyword)) {
		return parse_decl_func();
	}
	else {
		return NULL;
	}
}

Decl* parse_decl(void) {
	Decl* decl = parse_decl_opt();
	if (!decl) {
		fatal_syntax_error("Expected declaration keyword, got %s", token_info());
	}
	return decl;
}

DeclSet* parse_file(void) {
	Decl** decls = NULL;
	while (!is_token(TOKEN_EOF)) {
		buf_push(decls, parse_decl());
	}
	return decl_set(decls, buf_len(decls));
}
