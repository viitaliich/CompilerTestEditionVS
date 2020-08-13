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
			Typespec** args;
			size_t num_args;
			Typespec* ret;
		} func;
	};
};

typedef struct FuncParam {
	const char* name;
	Typespec* type;
} FuncParam;

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
			FuncParam* params;
			size_t num_params;
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
	EXPR_CALL,
	EXPR_FIELD,
} ExprKind;

typedef enum CompoundFieldKind {
	FIELD_DEFAULT,
	FIELD_NAME,
} CompoundFieldKind;

typedef struct CompoundField {
	CompoundFieldKind kind;
	Expr* init;
	union {
		const char* name;
		Expr* index;
	};
} CompoundField;

struct Expr {
	ExprKind kind;
	SrcLoc loc;
	struct Type* type;
	union {
		int64_t int_val;
		double float_val;
		const char* str_val;
		const char* name;
		Expr* sizeof_expr;
		Typespec* sizeof_type;
		struct {
			Expr* expr;
			Expr** args;
			size_t num_args;
		} call;
		struct {
			Expr* expr;
			const char* name;
		} field;
	};
};

typedef enum StmtKind {
	STMT_NONE,
	STMT_RETURN,
	STMT_BLOCK,
	STMT_INIT,
	STMT_EXPR,
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
