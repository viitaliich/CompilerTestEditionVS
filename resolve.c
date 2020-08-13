typedef enum TypeKind {
	TYPE_NONE,
	TYPE_INCOMPLETE,
	TYPE_COMPLETING,
	TYPE_VOID,
	TYPE_CHAR,
	TYPE_INT,
	TYPE_FLOAT,
	TYPE_FUNC,
} TypeKind;

typedef struct Type Type;
typedef struct Sym Sym;

typedef struct TypeField {
	const char* name;
	Type* type;
} TypeField;

struct Type {
	TypeKind kind;
	size_t size;
	size_t align;
	Sym* sym;
	union {
		struct {
			Type** params;
			size_t num_params;
			Type* ret;
		} func;
	};
};

void complete_type(Type* type);

Type* type_alloc(TypeKind kind) {
	Type* type = xcalloc(1, sizeof(Type));
	type->kind = kind;
	return type;
}

Type* type_void = &(Type) { TYPE_VOID, 0 };
Type* type_char = &(Type) { TYPE_CHAR, 1, 1 };
Type* type_int = &(Type) { TYPE_INT, 4, 4 };
Type* type_float = &(Type) { TYPE_FLOAT, 4, 4 };

const size_t PTR_SIZE = 8;
const size_t PTR_ALIGN = 8;

size_t type_sizeof(Type* type) {
	assert(type->kind > TYPE_COMPLETING);
	assert(type->size != 0);
	return type->size;
}

size_t type_alignof(Type* type) {
	assert(type->kind > TYPE_COMPLETING);
	return type->align;
}

Map cached_ptr_types;

typedef struct CachedFuncType {
	Type** params;
	size_t num_params;
	Type* ret;
	Type* func;
} CachedFuncType;

CachedFuncType* cached_func_types;

Type* type_func(Type** params, size_t num_params, Type* ret) {
	for (CachedFuncType* it = cached_func_types; it != buf_end(cached_func_types); it++) {
		if (it->num_params == num_params && it->ret == ret) {
			bool match = true;
			for (size_t i = 0; i < num_params; i++) {
				if (it->params[i] != params[i]) {
					match = false;
					break;
				}
			}
			if (match) {
				return it->func;
			}
		}
	}
	Type* type = type_alloc(TYPE_FUNC);
	type->size = PTR_SIZE;
	type->align = PTR_ALIGN;
	type->func.params = memdup(params, num_params * sizeof(*params));
	type->func.num_params = num_params;
	type->func.ret = ret;
	buf_push(cached_func_types, (CachedFuncType) { params, num_params, ret, type });
	return type;
}

// TODO: This probably shouldn't use an O(n^2) algorithm
bool duplicate_fields(TypeField* fields, size_t num_fields) {
	for (size_t i = 0; i < num_fields; i++) {
		for (size_t j = i + 1; j < num_fields; j++) {
			if (fields[i].name == fields[j].name) {
				return true;
			}
		}
	}
	return false;
}

Type* type_incomplete(Sym* sym) {
	Type* type = type_alloc(TYPE_INCOMPLETE);
	type->sym = sym;
	return type;
}

typedef enum SymKind {
	SYM_NONE,
	SYM_CONST,
	SYM_FUNC,
	SYM_TYPE,
} SymKind;

typedef enum SymState {
	SYM_UNRESOLVED,
	SYM_RESOLVING,
	SYM_RESOLVED,
} SymState;

typedef struct Sym {
	const char* name;
	SymKind kind;
	SymState state;
	Decl* decl;
	Type* type;
	int64_t val;
} Sym;

enum {
	MAX_LOCAL_SYMS = 1024
};

Map global_syms;
Sym local_syms[MAX_LOCAL_SYMS];
Sym* local_syms_end = local_syms;

Sym* sym_new(SymKind kind, const char* name, Decl* decl) {
	Sym* sym = xcalloc(1, sizeof(Sym));
	sym->kind = kind;
	sym->name = name;
	sym->decl = decl;
	return sym;
}

Sym* sym_decl(Decl* decl) {
	SymKind kind = SYM_NONE;
	switch (decl->kind) {
	case DECL_CONST:
		kind = SYM_CONST;
		break;
	case DECL_FUNC:
		kind = SYM_FUNC;
		break;
	default:
		assert(0);
		break;
	}
	Sym* sym = sym_new(kind, decl->name, decl);
	return sym;
}

Sym* sym_get(const char* name) {
	// for (size_t i = 0; i < n; i++)
	// for (size_t i = n; i > 0; i--)
	for (Sym* it = local_syms_end; it != local_syms; it--) {
		Sym* sym = it - 1;
		if (sym->name == name) {
			return sym;
		}
	}
	return map_get(&global_syms, (void*)name);
}

Sym* sym_enter(void) {
	return local_syms_end;
}

void sym_leave(Sym* sym) {
	local_syms_end = sym;
}

void sym_global_put(Sym* sym) {
	map_put(&global_syms, (void*)sym->name, sym);
}

Sym* sym_global_decl(Decl* decl) {
	Sym* sym = sym_decl(decl);
	sym_global_put(sym);
	decl->sym = sym;
	return sym;
}

Sym* sym_global_type(const char* name, Type* type) {
	Sym* sym = sym_new(SYM_TYPE, name, NULL);
	sym->state = SYM_RESOLVED;
	sym->type = type;
	sym_global_put(sym);
	return sym;
}

typedef struct Operand {
	Type* type;
	bool is_lvalue;
	bool is_const;
	int64_t val;
} Operand;

Operand operand_null;

Operand operand_rvalue(Type* type) {
	return (Operand) {
		.type = type,
	};
}

Operand operand_lvalue(Type* type) {
	return (Operand) {
		.type = type,
			.is_lvalue = true,
	};
}

Operand operand_const(int64_t val) {
	return (Operand) {
		.type = type_int,
			.is_const = true,
			.val = val,
	};
}

Sym* resolve_name(const char* name);
int64_t resolve_const_expr(Expr* expr);
Operand resolve_expr(Expr* expr);
Operand resolve_expected_expr(Expr* expr, Type* expected_type);

Type* resolve_typespec(Typespec* typespec) {
	Type* result = NULL;
	switch (typespec->kind) {
	case TYPESPEC_NAME: {
		Sym* sym = resolve_name(typespec->name);
		if (sym->kind != SYM_TYPE) {
			fatal_error(typespec->loc, "%s must denote a type", typespec->name);
			return NULL;
		}
		result = sym->type;
		break;
	}
	case TYPESPEC_FUNC: {
		Type** args = NULL;
		Type* ret = type_void;
		if (typespec->func.ret) {
			ret = resolve_typespec(typespec->func.ret);
		}
		result = type_func(args, buf_len(args), ret);
		break;
	}
	default:
		assert(0);
		return NULL;
	}
	assert(!typespec->type || typespec->type == result);
	typespec->type = result;
	return result;
}

Sym** ordered_syms;

Type* resolve_decl_func(Decl* decl) {
	assert(decl->kind == DECL_FUNC);
	Type** params = NULL;
	Type* ret_type = type_void;
	if (decl->func.ret_type) {
		ret_type = resolve_typespec(decl->func.ret_type);
	}
	return type_func(params, buf_len(params), ret_type);
}

void resolve_stmt(Stmt* stmt, Type* ret_type);

void resolve_stmt_block(StmtList block, Type* ret_type) {
	Sym* scope = sym_enter();
	for (size_t i = 0; i < block.num_stmts; i++) {
		resolve_stmt(block.stmts[i], ret_type);
	}
	sym_leave(scope);
}

void resolve_stmt(Stmt* stmt, Type* ret_type) {
	switch (stmt->kind) {
	case STMT_RETURN:
		if (stmt->expr) {
			if (resolve_expected_expr(stmt->expr, ret_type).type != ret_type) {
				fatal("Return type mismatch");
			}
		}
		else if (ret_type != type_void) {
			fatal("Empty return expression for function with non-void return type");
		}
		break;
	case STMT_BLOCK:
		resolve_stmt_block(stmt->block, ret_type);
		break;
	default:
		assert(0);
		break;
	}
}

void resolve_func_body(Sym* sym) {
	Decl* decl = sym->decl;
	assert(decl->kind == DECL_FUNC);
	assert(sym->state == SYM_RESOLVED);
	Sym* scope = sym_enter();
	resolve_stmt_block(decl->func.block, resolve_typespec(decl->func.ret_type));
	sym_leave(scope);
}

void resolve_sym(Sym* sym) {
	if (sym->state == SYM_RESOLVED) {
		return;
	}
	else if (sym->state == SYM_RESOLVING) {
		fatal("Cyclic dependency");
		return;
	}
	assert(sym->state == SYM_UNRESOLVED);
	sym->state = SYM_RESOLVING;
	switch (sym->kind) {
	case SYM_FUNC:
		sym->type = resolve_decl_func(sym->decl);
		break;
	default:
		assert(0);
		break;
	}
	sym->state = SYM_RESOLVED;
	buf_push(ordered_syms, sym);
}

void finalize_sym(Sym* sym) {
	resolve_sym(sym);
	if (sym->kind == SYM_FUNC) {
		resolve_func_body(sym);
	}
}

Sym* resolve_name(const char* name) {
	Sym* sym = sym_get(name);
	if (!sym) {
		fatal("Undeclared name '%s'", name);
		return NULL;
	}
	resolve_sym(sym);
	return sym;
}

Operand resolve_expr_name(Expr* expr) {
	assert(expr->kind == EXPR_NAME);
	Sym* sym = resolve_name(expr->name);
	if (sym->kind == SYM_FUNC) {
		return operand_rvalue(sym->type);
	}
	else {
		fatal("%s must be a var or const", expr->name);
		return operand_null;
	}
}

// Good case for expected types as well. 
// Can propagate down on the 2 branches of a ternary

Operand resolve_expected_expr(Expr* expr, Type* expected_type) {
	Operand result;
	switch (expr->kind) {
	case EXPR_INT:
		result = operand_const(expr->int_val);
		break;
	case EXPR_FLOAT:
		result = operand_rvalue(type_float);
		break;
	case EXPR_NAME:
		result = resolve_expr_name(expr);
		break;
	default:
		assert(0);
		result = operand_null;
		break;
	}
	if (result.type) {
		assert(!expr->type || expr->type == result.type);
		expr->type = result.type;
	}
	return result;
}

Operand resolve_expr(Expr* expr) {
	return resolve_expected_expr(expr, NULL);
}

int64_t resolve_const_expr(Expr* expr) {
	Operand result = resolve_expr(expr);
	if (!result.is_const) {
		fatal("Expected constant expression");
	}
	return result.val;
}

void init_global_syms(void) {
	sym_global_type(str_intern("char"), type_char);
	sym_global_type(str_intern("int"), type_int);
	sym_global_type(str_intern("float"), type_float);
}

void sym_global_decls(DeclSet* declset) {
	for (size_t i = 0; i < declset->num_decls; i++) {
		sym_global_decl(declset->decls[i]);
	}
}

void finalize_syms(void) {
	for (size_t i = 0; i < global_syms.cap; i++) {
		if (!global_syms.keys[i]) {
			continue;
		}
		Sym* sym = global_syms.vals[i];
		finalize_sym(sym);
	}
}

// We need a way to construct some sort of representations of types in a type system. 
// It'll also be used in code generator backend and so on.
// In str_intern we had the same idea: we take a value we interned 
// and then we expect for the same value to always get the same result back which is a canonical represent of that value. 
// Here we can do exact thing and check poiners for equality. 

// Caching here is like string interning. 
// Make sure by checking the cache first that you never create a new instance of some thing that you already have and match for in the cache.
// Structs and unions are always resolved in a new instance.

// Nice to separete the ordering from everything else so when you go to compute sizes of all the types,
// they are already in an order. 
// All other tings will be more straigtforward. They don't have to be worried about ordering on demand, only that ordering is consistent.

// LL1 resolver

// That's sort of what C does [Day 8. 28:00]

// sizeof pointer = 4 or 8 (32 or 64 bit)

// global symbol tables are hash table. 
// and local are like stack - linear search for the nested scopes		(for functions maybe?)

// Day 9

// statement list = statement block