typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;
typedef struct Typespec Typespec;

struct Type;

typedef struct StmtList {
	Stmt** stmts;
	size_t num_stmts;
} StmtList;

typedef enum TypespecKind {
	TYPESPEC_NONE,
	TYPESPEC_NAME,
	TYPESPEC_FUNC,
} TypespecKind;

struct Typespec {
	TypespecKind kind;
	SrcLoc loc;
	struct Type* type;
	union {
		const char* name;
		struct {
			Typespec* ret;
		} func;
	};
};

typedef enum DeclKind {
	DECL_NONE,
	DECL_CONST,
	DECL_FUNC,
} DeclKind;

struct Decl {
	DeclKind kind;
	SrcLoc loc;
	const char* name;
	struct Sym* sym;
	union {
		struct {
			Typespec* ret_type;
			StmtList block;
		} func;
	};
};

typedef struct DeclSet {
	Decl** decls;
	size_t num_decls;
} DeclSet;

typedef enum ExprKind {
	EXPR_NONE,
	EXPR_INT,
	EXPR_FLOAT,
	EXPR_NAME,
} ExprKind;

struct Expr {
	ExprKind kind;
	SrcLoc loc;
	struct Type* type;
	union {
		int64_t int_val;
		double float_val;
		const char* name;
	};
};

typedef enum StmtKind {
	STMT_NONE,
	STMT_RETURN,
	STMT_BLOCK,
	STMT_EXPR,x
} StmtKind;

struct Stmt {
	StmtKind kind;
	SrcLoc loc;
	union {
		Expr* expr;
		Decl* decl;
		StmtList block;
		struct {
			const char* name;
			Expr* expr;
		} init;
	};
};