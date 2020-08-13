Arena ast_arena;

void* ast_alloc(size_t size) {
	assert(size != 0);
	void* ptr = arena_alloc(&ast_arena, size);
	memset(ptr, 0, size);		// set 0 in given memory chunk		// why do we do this?	???
	return ptr;
}

// where is it using ???
void* ast_dup(const void* src, size_t size) {
	if (size == 0) {
		return NULL;
	}
	void* ptr = arena_alloc(&ast_arena, size);
	memcpy(ptr, src, size);
	return ptr;
}

#define AST_DUP(x) ast_dup(x, num_##x * sizeof(*x))

StmtList stmt_list(Stmt** stmts, size_t num_stmts) {
	return (StmtList) { AST_DUP(stmts), num_stmts };
}

// Constructors

Typespec* typespec_new(TypespecKind kind) {
	Typespec* t = ast_alloc(sizeof(Typespec));
	t->loc = (SrcLoc){ src_name, src_line };
	t->kind = kind;
	return t;
}

Typespec* typespec_name(const char* name) {
	Typespec* t = typespec_new(TYPESPEC_NAME);
	t->name = name;
	return t;

}

Typespec* typespec_func(Typespec** args, size_t num_args, Typespec* ret) {
	Typespec* t = typespec_new(TYPESPEC_FUNC);
	t->func.ret = ret;
	return t;
}

DeclSet* decl_set(Decl** decls, size_t num_decls) {
	DeclSet* declset = ast_alloc(sizeof(DeclSet));
	declset->decls = AST_DUP(decls);
	declset->num_decls = num_decls;
	return declset;
}

Decl* decl_new(DeclKind kind, const char* name) {
	Decl* d = ast_alloc(sizeof(Decl));
	d->loc = (SrcLoc){ src_name, src_line };
	d->kind = kind;
	d->name = name;
	return d;
}

Decl* decl_func(const char* name, Typespec* ret_type, StmtList block) {
	Decl* d = decl_new(DECL_FUNC, name);
	d->func.ret_type = ret_type;
	d->func.block = block;
	return d;
}

Expr* expr_new(ExprKind kind) {
	Expr* e = ast_alloc(sizeof(Expr));
	e->loc = (SrcLoc){ src_name, src_line };
	e->kind = kind;
	return e;
}

Expr* expr_int(int64_t int_val) {
	Expr* e = expr_new(EXPR_INT);
	e->int_val = int_val;
	return e;
}

Expr* expr_float(double float_val) {
	Expr* e = expr_new(EXPR_FLOAT);
	e->float_val = float_val;
	return e;
}

Expr* expr_name(const char* name) {
	Expr* e = expr_new(EXPR_NAME);
	e->name = name;
	return e;
}

Stmt* stmt_new(StmtKind kind) {
	Stmt* s = ast_alloc(sizeof(Stmt));
	s->loc = (SrcLoc){ src_name, src_line };
	s->kind = kind;
	return s;
}

Stmt* stmt_return(Expr* expr) {
	Stmt* s = stmt_new(STMT_RETURN);
	s->expr = expr;
	return s;
}

Stmt* stmt_block(StmtList block) {
	Stmt* s = stmt_new(STMT_BLOCK);
	s->block = block;
	return s;
}

Stmt* stmt_expr(Expr* expr) {
	Stmt* s = stmt_new(STMT_EXPR);
	s->expr = expr;
	return s;
}

#undef AST_DUP

// Linked lists are very good for parsers

// In many languages 
//	function calls a(x), 
//	field access a.x, 
//	array element access a[x]
// are processed like left assosiative binary operator