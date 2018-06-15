#ifndef kvh_h
#define kvh_h

#include <string>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// type declarations
typedef struct {
    string key;
    RObject val;
    bool tab_found;
} keyval;

typedef struct {
    List res;
    string line;
} list_line;

// function declarations
string unescape(string s);
bool indent_lacking(string& buf, size_t& lev);
bool escaped_eol(string& buf);
string kvh_get_line(ifstream& fin, const string& comment_str);
inline keyval kvh_parse_kv(string& line, size_t& lev, const bool strip_white, const string& split_str);
list_line kvh_read(ifstream& fin, size_t lev, const string& comment_str, const bool strip_white, const bool skip_blank, const string& split_str, const bool follow_url);
RObject kvh_read(string fn, const string& comment_str, const bool strip_white, const bool skip_blank, const string& split_str, const bool follow_url);

#endif
