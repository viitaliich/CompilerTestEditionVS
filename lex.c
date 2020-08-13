const char* func_keyword;
const char* return_keyword;

const char* first_keyword;
const char* last_keyword;
const char** keywords;

#define KEYWORD(name) name##_keyword = str_intern(#name); buf_push(keywords, name##_keyword)

void init_keywords(void) {
	static bool inited;
	if (inited) {
		return;
	}
	KEYWORD(func);
	char* arena_end = intern_arena.end;
	KEYWORD(return);
	assert(intern_arena.end == arena_end);
	first_keyword = func_keyword;  //first_keyword = typedef_keyword;
	last_keyword = return_keyword;  //last_keyword = default_keyword;
	inited = true;
}

#undef KEYWORD

bool is_keyword_name(const char* name) {
	return first_keyword <= name && name <= last_keyword;
}

typedef enum TokenKind {
	TOKEN_EOF,
	TOKEN_COLON,
	TOKEN_LPAREN,
	TOKEN_RPAREN,
	TOKEN_LBRACE,
	TOKEN_RBRACE,
	TOKEN_DOT,
	TOKEN_SEMICOLON,
	TOKEN_KEYWORD,
	TOKEN_INT,
	TOKEN_FLOAT,
	TOKEN_NAME,
} TokenKind;

typedef enum TokenMod {
	TOKENMOD_NONE,
	TOKENMOD_HEX,
	TOKENMOD_BIN,
	TOKENMOD_OCT,
	TOKENMOD_CHAR,
} TokenMod;

const char* token_kind_names[] = {
	[TOKEN_EOF] = "EOF",
	[TOKEN_COLON] = ":",
	[TOKEN_LPAREN] = "(",
	[TOKEN_RPAREN] = ")",
	[TOKEN_LBRACE] = "{",
	[TOKEN_RBRACE] = "}",
	[TOKEN_DOT] = ".",
	[TOKEN_SEMICOLON] = ";",
	[TOKEN_KEYWORD] = "keyword",
	[TOKEN_INT] = "int",
	[TOKEN_FLOAT] = "float",
	[TOKEN_NAME] = "name",
};

const char* token_kind_name(TokenKind kind) {
	if (kind < sizeof(token_kind_names) / sizeof(*token_kind_names)) {	// ???
		return token_kind_names[kind];
	}
	else {
		return "<unknown>";
	}
}

typedef struct Token {
	TokenKind kind;
	TokenMod mod;
	const char* start;
	const char* end;
	union {
		int64_t int_val;
		double float_val;
		const char* name;
	};
} Token;

Token token;	// global token, corresponds to the current token.
const char* stream;
const char* line_start;

int src_line;
const char* src_name;

const char* token_info(void) {
	if (token.kind == TOKEN_NAME || token.kind == TOKEN_KEYWORD) {
		return token.name;
	}
	else {
		return token_kind_name(token.kind);
	}
}

uint8_t char_to_digit[256] = {
	['0'] = 0,
	['1'] = 1,
	['2'] = 2,
	['3'] = 3,
	['4'] = 4,
	['5'] = 5,
	['6'] = 6,
	['7'] = 7,
	['8'] = 8,
	['9'] = 9,
	['a'] = 10,['A'] = 10,
	['b'] = 11,['B'] = 11,
	['c'] = 12,['C'] = 12,
	['d'] = 13,['D'] = 13,
	['e'] = 14,['E'] = 14,
	['f'] = 15,['F'] = 15,
};

void scan_int(void) {
	uint64_t base = 10;		// why uint64_t ???		uint8_t
	if (*stream == '0') {
		stream++;
		if (tolower(*stream) == 'x') {
			stream++;
			token.mod = TOKENMOD_HEX;
			base = 16;
		}
		else if (tolower(*stream) == 'b') {
			stream++;
			token.mod = TOKENMOD_BIN;
			base = 2;
		}
		// integer digit preceded by 0 is interpreted as an octal number (different in python)		???
		else if (isdigit(*stream)) {
			token.mod = TOKENMOD_OCT;
			base = 8;		// tell in labs about this method
		}
	}
	uint64_t val = 0;
	for (;;) {
		uint64_t digit = char_to_digit[*(unsigned char*)stream];
		if (digit == 0 && *stream != '0') {
			break;
		}
		if (digit >= base) {
			syntax_error("Digit '%c' out of range for base %" PRIu64, *stream, base);
			digit = 0;
		}
		if (val > (UINT64_MAX - digit) / base) {
			syntax_error("Integer literal overflow");
			while (isdigit(*stream)) {
				stream++;
			}
			val = 0;
			break;
		}
		val = val * base + digit;	// shifts everything when you see a new digit
		stream++;
	}
	token.kind = TOKEN_INT;
	token.int_val = val;
}

void scan_float(void) {
	const char* start = stream;
	while (isdigit(*stream)) {
		stream++;
	}
	if (*stream == '.') {
		stream++;
	}
	while (isdigit(*stream)) {
		stream++;
	}
	if (tolower(*stream) == 'e') {
		stream++;
		if (*stream == '+' || *stream == '-') {		// don't need + for positive power ???
			stream++;
		}
		if (!isdigit(*stream)) {
			syntax_error("Expected digit after float literal exponent, found '%c'.", *stream);
		}
		while (isdigit(*stream)) {
			stream++;
		}
	}
	double val = strtod(start, NULL);
	//if (val == HUGE_VAL || val == -HUGE_VAL) {
	if (val == HUGE_VAL) {
		syntax_error("Float literal overflow");
	}
	token.kind = TOKEN_FLOAT;
	token.float_val = val;
}

char escape_to_char[256] = {		// ???
	['n'] = '\n',
	['r'] = '\r',
	['t'] = '\t',
	['v'] = '\v',
	['b'] = '\b',
	['a'] = '\a',
	['0'] = 0,
};

void scan_char(void) {
	assert(*stream == '\'');		// start with single quote
	stream++;

	char val = 0;
	if (*stream == '\'') {
		syntax_error("Char literal cannot be empty");		// why not ???
		stream++;
	}
	else if (*stream == '\n') {						// use for comments ???
		syntax_error("Char literal cannot contain newline");			// ???
	}
	else if (*stream == '\\') {
		stream++;
		val = escape_to_char[*(unsigned char*)stream];
		if (val == 0 && *stream != '0') {
			syntax_error("Invalid char literal escape '\\%c'.", *stream);
		}
		stream++;
	}
	else {
		val = *stream;
		stream++;
	}
	if (*stream != '\'') {
		syntax_error("Expected closing char quote, got '%c'", *stream);
	}
	else {
		stream++;
	}
	token.kind = TOKEN_INT;
	token.int_val = val;
	token.mod = TOKENMOD_CHAR;
}

// These macros are very localised and not really reusable. 
// They are intrnded for a very specific purpose. 
// So they are pretty straightforward in a sence of syntax. 
//
// This is pretty common technique
#define CASE1(c1, k1) \
    case c1: \
        token.kind = k1; \
        stream++; \
        break;

#define CASE2(c1, k1, c2, k2) \
    case c1: \
        token.kind = k1; \
        stream++; \
        if (*stream == c2) { \
            token.kind = k2; \
            stream++; \
        } \
        break;

#define CASE3(c1, k1, c2, k2, c3, k3) \
    case c1: \
        token.kind = k1; \
        stream++; \
        if (*stream == c2) { \
            token.kind = k2; \
            stream++; \
        } else if (*stream == c3) { \
            token.kind = k3; \
            stream++; \
        } \
        break;

void next_token(void) {
repeat:								// that's not good ???
	token.start = stream;
	token.mod = 0;
	switch (*stream) {
		// could be (but not recomended!) if(isdigit(*stream)...)
	case ' ': case '\n': case '\r': case '\t': case '\v':
		while (isspace(*stream)) {
			if (*stream++ == '\n') {
				line_start = stream;
				src_line++;
			}
		}
		goto repeat;
		break;
	case '\'':
		scan_char();
		break;
	
	case '.':		// float numbers can start with point			// ???
		if (isdigit(stream[1])) {
			scan_float();
		}
		else {
			token.kind = TOKEN_DOT;
			stream++;
		}
		break;
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
		while (isdigit(*stream)) {
			stream++;
		}
		char c = *stream;
		stream = token.start;
		if (c == '.' || tolower(c) == 'e') {
			scan_float();
		}
		else {
			scan_int();
		}
		break;
	}
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
	case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case '_':
		while (isalnum(*stream) || *stream == '_') {
			stream++;
		}
		token.name = str_intern_range(token.start, stream);
		token.kind = is_keyword_name(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
		break;
	
		CASE1('\0', TOKEN_EOF)
			CASE1('(', TOKEN_LPAREN)
			CASE1(')', TOKEN_RPAREN)
			CASE1('{', TOKEN_LBRACE)
			CASE1('}', TOKEN_RBRACE)
			
			CASE1(';', TOKEN_SEMICOLON)
			
			CASE1(':', TOKEN_COLON)	//CASE2(':', TOKEN_COLON, '=', TOKEN_COLON_ASSIGN)
			
	default:
		syntax_error("Invalid '%c' token, skipping", *stream);
		stream++;
		goto repeat;
	}
	token.end = stream;
}

#undef CASE1
#undef CASE2
#undef CASE3

void init_stream(const char* name, const char* buf) {
	stream = buf;
	src_name = name ? name : "<anonymous>";
	src_line = 1;
	line_start = stream;
	next_token();
}

bool is_token(TokenKind kind) {
	return token.kind == kind;
}

bool is_token_eof(void) {
	return token.kind == TOKEN_EOF;
}

bool is_token_name(const char* name) {
	return token.kind == TOKEN_NAME && token.name == name;
}

bool is_keyword(const char* name) {
	return is_token(TOKEN_KEYWORD) && token.name == name;
}

bool match_keyword(const char* name) {
	if (is_keyword(name)) {
		next_token();
		return true;
	}
	else {
		return false;
	}
}

bool match_token(TokenKind kind) {
	if (is_token(kind)) {
		next_token();
		return true;
	}
	else {
		return false;
	}
}

bool expect_token(TokenKind kind) {
	if (is_token(kind)) {
		next_token();
		return true;
	}
	else {
		fatal("expected token %s, got %s", token_kind_name(kind), token_info());
		return false;
	}
}

void keyword_test(void) {
	init_keywords();
	assert(is_keyword_name(first_keyword));
	assert(is_keyword_name(last_keyword));
	for (const char** it = keywords; it != buf_end(keywords); it++) {
		assert(is_keyword_name(*it));
	}
	assert(!is_keyword_name(str_intern("foo")));
}

#define assert_token(x) assert(match_token(x))
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_str(x) assert(strcmp(token.str_val, (x)) == 0 && match_token(TOKEN_STR))
#define assert_token_eof() assert(is_token(0))

#undef assert_token
#undef assert_token_name
#undef assert_token_int
#undef assert_token_float
#undef assert_token_str
#undef assert_token_eof
