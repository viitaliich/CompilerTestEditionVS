char* gen_buf = NULL;

#define genf(...) buf_printf(gen_buf, __VA_ARGS__)
#define genlnf(...) (genln(), genf(__VA_ARGS__))

int gen_indent;

void genln(void) {
	genf("\n%.*s", gen_indent * 4, "                                                                  ");
}

const char* cdecl_paren(const char* str, bool b) {
	return b ? strf("(%s)", str) : str;
}

const char* cdecl_name(Type* type) {
	switch (type->kind) {
	case TYPE_CHAR:
		return "char";
	case TYPE_INT:
		return "int";
	case TYPE_FLOAT:
		return "float";
	default:
		assert(0);
		return NULL;
	}
}

char* type_to_cdecl(Type* type, const char* str) {
	switch (type->kind) {
	case TYPE_CHAR:
	case TYPE_INT:
	case TYPE_FLOAT:
	case TYPE_FUNC: {
		char* result = NULL;
		buf_printf(result, "%s(", cdecl_paren(strf("*%s", str), *str));
		if (type->func.num_params == 0) {
			buf_printf(result, "void");
		}
		else {
			for (size_t i = 0; i < type->func.num_params; i++) {
				buf_printf(result, "%s%s", i == 0 ? "" : ", ", type_to_cdecl(type->func.params[i], ""));
			}
		}
		buf_printf(result, ")");
		return type_to_cdecl(type->func.ret, result);
	}
	default:
		assert(0);
		return NULL;
	}
}

void gen_expr(Expr* expr);

const char* gen_expr_str(Expr* expr) {
	char* temp = gen_buf;
	gen_buf = NULL;
	gen_expr(expr);
	const char* result = gen_buf;
	gen_buf = temp;
	return result;
}

char* typespec_to_cdecl(Typespec* typespec, const char* str) {
	// TODO: Figure out how to handle type vs typespec in C gen for inferred types. How to prevent "flattened" const values?
	switch (typespec->kind) {
	case TYPESPEC_NAME:
		return strf("%s%s%s", typespec->name, *str ? " " : "", str);
	case TYPESPEC_FUNC: {
		char* result = NULL;
		buf_printf(result, "%s(", cdecl_paren(strf("*%s", str), *str));
		if (typespec->func.num_args == 0) {
			buf_printf(result, "void");
		}
		else {
			for (size_t i = 0; i < typespec->func.num_args; i++) {
				buf_printf(result, "%s%s", i == 0 ? "" : ", ", typespec_to_cdecl(typespec->func.args[i], ""));
			}
		}
		buf_printf(result, ")");
		return typespec_to_cdecl(typespec->func.ret, result);
	}
	default:
		assert(0);
		return NULL;
	}
}

void gen_func_decl(Decl* decl) {
	assert(decl->kind == DECL_FUNC);
	if (decl->func.ret_type) {
		genlnf("%s(", typespec_to_cdecl(decl->func.ret_type, decl->name));
	}
	else {
		genlnf("void %s(", decl->name);
	}
	if (decl->func.num_params == 0) {
		genf("void");
	}
	else {
		for (size_t i = 0; i < decl->func.num_params; i++) {
			FuncParam param = decl->func.params[i];
			if (i != 0) {
				genf(", ");
			}
			genf("%s", typespec_to_cdecl(param.type, param.name));
		}
	}
	genf(")");
}

void gen_forward_decls(void) {
	for (size_t i = 0; i < global_syms.cap; i++) {
		if (!global_syms.keys[i]) {
			continue;
		}
		Sym* sym = global_syms.vals[i];
		Decl* decl = sym->decl;
		if (!decl) {
			continue;
		}
		switch (decl->kind) {
		case DECL_FUNC:
			gen_func_decl(sym->decl);
			genf(";");
			break;
		default:
			// Do nothing.
			break;
		}
	}
}

void gen_expr(Expr* expr) {
	switch (expr->kind) {
	case EXPR_INT:
		genf("%lld", expr->int_val);
		break;
	case EXPR_FLOAT:
		genf("%f", expr->float_val);
		break;
	case EXPR_NAME:
		genf("%s", expr->name);
		break;
	case EXPR_CALL:
		gen_expr(expr->call.expr);
		genf("(");
		for (size_t i = 0; i < expr->call.num_args; i++) {
			if (i != 0) {
				genf(", ");
			}
			gen_expr(expr->call.args[i]);
		}
		genf(")");
		break;
	case EXPR_FIELD:
		gen_expr(expr->field.expr);
		genf(".%s", expr->field.name);
		break;
	default:
		assert(0);
	}
}

void gen_stmt(Stmt* stmt);

void gen_stmt_block(StmtList block) {
	genf("{");
	gen_indent++;
	for (size_t i = 0; i < block.num_stmts; i++) {
		gen_stmt(block.stmts[i]);
	}
	gen_indent--;
	genlnf("}");
}

void gen_simple_stmt(Stmt* stmt) {
	switch (stmt->kind) {
	case STMT_EXPR:
		gen_expr(stmt->expr);
		break;
	case STMT_INIT:
		genf("%s = ", type_to_cdecl(stmt->init.expr->type, stmt->init.name));
		gen_expr(stmt->init.expr);
		break;
	default:
		assert(0);
	}
}

void gen_stmt(Stmt* stmt) {
	switch (stmt->kind) {
	case STMT_RETURN:
		genlnf("return");
		if (stmt->expr) {
			genf(" ");
			gen_expr(stmt->expr);
		}
		genf(";");
		break;
	case STMT_BLOCK:
		genln();
		gen_stmt_block(stmt->block);
		break;
	}
}

void gen_func(Decl* decl) {
	assert(decl->kind == DECL_FUNC);
	gen_func_decl(decl);
	genf(" ");
	gen_stmt_block(decl->func.block);
}

void gen_sym(Sym* sym) {
	Decl* decl = sym->decl;
	if (!decl) {
		return;
	}
	switch (decl->kind) {
	case DECL_FUNC:
		gen_func(sym->decl);
		break;
	default:
		assert(0);
		break;
	}
}

void gen_ordered_decls(void) {
	for (size_t i = 0; i < buf_len(ordered_syms); i++) {
		gen_sym(ordered_syms[i]);
	}
}

void gen_all(void) {
	gen_buf = NULL;
	genf("// Forward declarations");
	gen_forward_decls();
	genln();
	genlnf("// Ordered declarations");
	gen_ordered_decls();
}
