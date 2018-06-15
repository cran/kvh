#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include <iostream>
#include <fstream>

#include "../inst/include/kvh.h"

static string whitespaces(" \t\f\v\n\r");

// auxiliary functions
string unescape(string s) {
    // unescape tab, newline and backslash
    string res=s;
    size_t i,j;
    char c;
//Rcout << "s.size()=" << s.size() << endl;
    for (i=0,j=0; i < s.size(); i++) {
//Rcout << "s[" << i << "]='" << s[i] << "'" << endl;
        if (s[i] == '\\') {
            if (i < s.size()-1) {
                c=s[i+1];
                // unescape only these three chars
                res[j++]=(c == '\\' || c == '\t' || c == '\n') ? s[++i] : s[i];
            }
        } else {
            res[j++]=s[i];
        }
//Rcout << "res[" << j << "]='" << res[j] << "'" << endl;
    }
    return res.substr(0, j);
}
bool indent_lacking(string& buf, size_t& lev) {
    // check if number of starting tab corresponds to lev
    if (lev == 0)
        return false; // at level 0 no lacking tabs
    if (buf.size() < lev)
        return true; // too short to have sufficient number of tabs
    for (size_t i=0; i < lev; i++) {
        if (buf[i] != '\t')
            return true;
    }
    return false;
}
bool escaped_eol(string& buf) {
    // test if end_of_line is escaped or not in buf
//Rcout << "escaped_eol: testing '" << buf << "'\n";
    int i;
    if (buf.size() == 0)
        return false;
    for (i=buf.size()-1; i >= 0 && buf[i] == '\\'; i--) {
        ; // find out the position of the last backslash series
    }
    i=buf.size()-i-1;
//Rcout << "result=" << (bool) i%2 << "\n";
    return i%2;
}
inline void strip_wh(string &s) {
//Rcout << "strip_wh: stripping '" << s << "'\n";
    if (s.size() == 0)
        return;
    size_t pstr;
    pstr=s.find_first_not_of(whitespaces);
    if (pstr != string::npos) {
        s.erase(0, pstr);
//Rcout << "strip_wh: striped left '" << s << "'\n";
    } else {
        s.clear(); // s is all whitespace
        return;
    }
    pstr=s.find_last_not_of(whitespaces);
    if (pstr != string::npos)
        s.erase(pstr+1);
//Rcout << "strip_wh: striped right '" << s << "'\n";
    return;
}

string kvh_get_line(ifstream& fin, size_t* ln, const string& comment_str) {
    // get a string from stream that ends without escaped eol character and increment ln[0]
    string b, res;
    size_t pstr;
    res="";
    bool first_read=true;
    while ((first_read || escaped_eol(res)) && getline(fin, b) && ++ln[0]) {
        if (!first_read && !fin.eof())
            res += '\n';
        res += b;
        first_read=false;
    }
    if (comment_str.size() > 0) {
        pstr=res.find(comment_str);
        if (pstr != string::npos && (b=res.substr(0, pstr), true) && !escaped_eol(b)) // stip out non escaped comments
            res=b;
    }
    return res;
}
keyval kvh_parse_kv(string& line, size_t& lev, const bool strip_white, const string &split_str) {
    // get key-value pair from the line
    keyval kv;
    size_t i, bs; // count backslashes;
    string s;
//Rcout << "lev=" << lev << ";\tline=" << line << endl;
    for (i=lev, bs=0; i < line.size(); i++) {
        if (line[i] == '\\') {
            bs++;
            continue;
        }
        if (line[i] == '\t' && (bs+1)%2) {
//Rcout << "line[" << i << "]=" << line[i] << "; bs=" << bs << endl;
            kv.key=unescape(line.substr(lev, i-lev));
            if (strip_white) {
                strip_wh(kv.key);
            }
            s=line.substr(i+1);
            if (split_str.size() > 0) {
                // split s
                vector<string> vs; // will have results of split
                size_t pos, splpos; // position of the split string in s
                string subs;
                for (pos=0; pos < s.size() && (splpos=s.find(split_str, pos)) != string::npos;) {
                    for (subs=s.substr(pos, splpos-pos); escaped_eol(subs) && splpos != string::npos; splpos=s.find(split_str,  splpos+split_str.size()), subs=s.substr(pos, splpos-pos))
                        ; // find first unescaped splpos
//Rcout << "pos=" << pos << "\n";
                    if (strip_white)
                        strip_wh(subs);
                    vs.push_back(unescape(subs));
                    pos=splpos+split_str.size();
//Rcout << "new pos=" << pos << "\n";
                }
//Rcout << "last pos=" << pos << "\n";
                subs=s.substr(pos);
                if (strip_white)
                    strip_wh(subs);
                vs.push_back(unescape(subs)); // add the last field at the end of s
                StringVector vs_r(vs.size());
                for (size_t i=0; i < vs.size(); i++)
                    vs_r[i]=vs[i];
                kv.val=wrap(vs_r);
            } else {
                if (strip_white)
                    strip_wh(s);
                kv.val=unescape(s);
            }
            kv.tab_found=true;
            break;
        }
        bs=0;
    }
    if (i == line.size()) {
        // no tab found => the whole string goes to the key
        kv.key=unescape(line.substr(lev));
        if (strip_white) 
            strip_wh(kv.key);
        kv.val="";
        kv.tab_found=false;
    }
    return(kv);
}
list_line kvh_read(ifstream& fin, size_t lev, size_t* ln, const string& comment_str, const bool strip_white, const bool skip_blank, const string& split_str, const bool follow_url) {
    // recursively read kvh file and return its content in a nested named list of character vectors
    List res=List::create() ;
    keyval kv;
    string line;
    list_line ll;
    bool read_stream=true;
    size_t ln_save;
    CharacterVector nm(0);
    while (!fin.eof()) { // && i++ < 5) {
        // get full line (i.e. concat lines with escaped end_of_line)
        if (read_stream)
            line=kvh_get_line(fin, ln, comment_str);
//print(wrap(line));
//print(wrap(fin.eof()));
        if (skip_blank && (line.size() == 0 || (strip_white && line.find_first_not_of(whitespaces) == string::npos)) && !fin.eof())
            continue; // skip white line
        if ((line.size() == 0 && fin.eof()) || (lev && indent_lacking(line, lev))) {
            // current level is ended => go upper and let treat the line (already read) there
            res.attr("ln")=(int) ln[0];
            res.attr("names")=nm;
            ll.res=res;
            ll.line=line;
            return ll;
        }
        kv=kvh_parse_kv(line, lev, strip_white, split_str);
        ln_save=ln[0];
//print(wrap(List::create(_["l"]=line, _["k"]=kv.key, _["v"]=kv.val)));
        read_stream=kv.tab_found;
        if (!kv.tab_found) {
            // tab is absent => we have to go deeper in the hierarchy level
            ll=kvh_read(fin, lev+1, ln, comment_str, strip_white, skip_blank, split_str, follow_url);
            kv.val=(ll.res.size() == 0 ? "" : ll.res);
            line=ll.line;
        } // else simple key-value pair
        if (follow_url && kv.val.sexp_type() == STRSXP) {
//Rcout << "follow_url\n";
            CharacterVector cval(kv.val);
            if (cval.size() == 1) {
                string sval=as<string>(cval[0]);
                if (sval.substr(0, 7) == "file://") {
                    sval=sval.substr(7);
//Rcout << "trying to kvh_read '" << sval << "'\n";
                    kv.val=kvh_read(sval, comment_str, strip_white, skip_blank, split_str, follow_url);
                    if (kv.val.isNULL()) {
//Rcout << "set kv.val back to file://...\n";
                        kv.val=cval;
                    }
                }
            }
        } // else if (kv.val.sexp_type() == STRSXP) { CharacterVector cval(kv.val); string sval=as<string>(cval[0]); if (sval.substr(0, 7) == "file://") Rcout << "not following '" << sval << "'\n";}
        kv.val.attr("ln")=(int) ln_save;
        res.push_back(kv.val);
        nm.push_back(kv.key);
    }
    res.attr("names")=nm;
    ll.res=res;
    ll.line="";
    return ll;
}
//' Parse file in KVH format
//'
//' Returns a list with names formed form kvh keys and values formed from kvh values
//' If a kvh value has sub-keys, it is returned as a nested list. Otherwise it is
//' returned as a character string.
//'
//' @param fn character kvh file name.
//' @param comment_str character optional comment string (default empty ""). If non empty, the comment
//'   string itself and everything following it on the line is ignored. Note that
//'   lines are first appended if end lines are escaped and then a search for a
//'   comment string is done.
//' @param strip_white logical optional control of white spaces on both ends of keys and values (default FALSE)
//' @param skip_blank logical optional control of lines composed of only white characters after a possible stripping of a comment (default FALSE)
//' @param split_str character optional string by which a value string can be splitted in several strings (default: empty string, i.e. no splitting)
//' @param follow_url logical optional control of recursive kvh reading and parsing. If set to TRUE and a value starts with 'file://' then the path following this prefix will be passed as argument 'fn' to another 'kvh_read()' call. The list returned by this last call will be affected to the corresponding key instead of the value 'file://...'. If a circular reference to some file is detected, a warning is emmited and the faulty value 'file://...' will be left without change. The rest of the file is proceeded as usual.
//' @export
// [[Rcpp::export]]
RObject kvh_read(string fn, const string& comment_str="", const bool strip_white=false, const bool skip_blank=false, const string& split_str="", const bool follow_url=false) {
    // read kvh file and return its content in a nested named list of character vectors
    if (comment_str.find('\t') < string::npos || comment_str.find('\n') < string::npos)
        stop("parameter 'comment_str' cannot have tabulation or new line characters");
//Rcout << "follow_url=" << follow_url << "\n";
    // check for nested references if follow_url=true
    static set<string> read_files;
    if (follow_url) {
        if (read_files.find(fn) != read_files.end()) {
            warning("kvh_read: detected circular reference to file '%s'", fn);
            return R_NilValue;
        }
        read_files.insert(fn);
    }
    // open file for binary reading
    ifstream fin;
    list_line ll;
    size_t ln=0; // running line number in kvh file
    fin.open(fn.data(), ios::in | ios::binary);
    if (!fin)
        stop("cannot open file '%s' for reading", fn);
    ll=kvh_read(fin, 0, &ln, comment_str, strip_white, skip_blank, split_str, follow_url);
    fin.close();
    if (follow_url)
        read_files.erase(fn);
    ll.res.attr("file")=fn;
    return ll.res;
}
