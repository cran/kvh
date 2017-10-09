#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include <iostream>
#include <fstream>

#include "../inst/include/kvh.h"

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
        return true; // too short to have sufficient number of bits
    for (size_t i=0; i < lev; i++) {
        if (buf[i] != '\t')
            return true;
    }
    return false;
}
bool escaped_eol(string& buf) {
    // test if end_of_line is escaped or not in buf
    int i;
    if (buf.size() == 0)
        return false;
    for (i=buf.size()-1; i >= 0 && buf[i] == '\\'; i--) {
        ; // find out the position of the last backslash series
    }
    i=buf.size()-i-1;
    return i%2;
}
string kvh_get_line(ifstream& fin) {
    // get a string from stream that ends without escaped eol character
    string b, res;
    res="";
    bool first_read=true;
    while ((first_read || escaped_eol(res)) && getline(fin, b)) {
        if (!first_read && !fin.eof())
            res += '\n';
        res += b;
        first_read=false;
    }
    return res;
}
keyval kvh_parse_kv(string& line, size_t& lev) {
    // get key-value pair from the line
    keyval kv;
    size_t i, bs; // count backslashes;
//Rcout << "lev=" << lev << ";\tline=" << line << endl;
    for (i=lev, bs=0; i < line.size(); i++) {
        if (line[i] == '\\') {
            bs++;
            continue;
        }
        if (line[i] == '\t' && (bs+1)%2) {
//Rcout << "line[" << i << "]=" << line[i] << "; bs=" << bs << endl;
            kv.key=unescape(line.substr(lev, i-lev));
            kv.val=unescape(line.substr(i+1));
            kv.tab_found=true;
            break;
        }
        bs=0;
    }
    if (i == line.size()) {
        // no tab found => all the string goes to the key
        kv.key=unescape(line.substr(lev));
        kv.val="";
        kv.tab_found=false;
    }
    return(kv);
}
list_line kvh_read(ifstream& fin, size_t lev) {
    // recursively read kvh file and return its content in a nested named list of character vectors
    List res=List::create() ;
    keyval kv;
    string line;
    list_line ll;
    bool read_fin=true;
    //size_t i=0;
    CharacterVector nm(0);
    while (!fin.eof()) { // && i++ < 5) {
        // get full line (i.e. concat lines with escaped end_of_line)
        if (read_fin)
            line=kvh_get_line(fin);
//print(wrap(line));
//print(wrap(fin.eof()));
        if ((line.size() == 0 && fin.eof()) || (lev && indent_lacking(line, lev))) {
            // current level is ended => go upper and let treat the line (already read) there
            res.attr("names")=nm;
            ll.res=res;
            ll.line=line;
            return ll;
        }
        kv=kvh_parse_kv(line, lev);
//print(wrap(List::create(_["l"]=line, _["k"]=kv.key, _["v"]=kv.val)));
        if (!kv.tab_found) {
            // tab is absent => we have to go deeper in the hierarchy level
            ll=kvh_read(fin, lev+1);
            kv.val=ll.res;
            line=ll.line;
            read_fin=false; // don't read next line from fin as it was read in deeper level
        } else {
            // simple key-value pair
            read_fin=true;
        }
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
//' @export
// [[Rcpp::export]]
List kvh_read(string& fn) {
    // read kvh file and return its content in a nested named list of character vectors
    
    // open file for binary reading
    ifstream fin;
    list_line ll;
    fin.open(fn.data(), ios::in | ios::binary);
    if (!fin)
        stop("cannot open file '%s' for reading", fn);
    ll=kvh_read(fin, 0);
    fin.close();
    return ll.res;
}
