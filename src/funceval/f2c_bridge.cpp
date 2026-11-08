// A legacy Fortran user function, translated to a nested_fit device source at run time.
// f2c does the translation; this prepares the Fortran so that f2c takes it (tabs, several statements per line,
// lines beyond column 72, intrinsics f2c does not know), keeps only the functions the model calls, and turns the C
// into device code: device qualifiers, DATA tables in constant memory, the math prototypes of the host dropped, and
// nf_user(x, params) on top. The shim (nf_f2c_shim.h, cached with the system objects) supplies f2c's runtime and the
// legacy library entry points (WOFZ, VOIGT_IF, ...) over the device library. Called from auto_func.f90.
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <map>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace {

using Lines = std::vector<std::string>;

Lines split_lines(const std::string& s) {
    Lines out; std::string cur;
    for (char c : s) { if (c == '\n') { out.push_back(cur); cur.clear(); } else cur += c; }
    if (!cur.empty()) out.push_back(cur);
    return out;
}
std::string join(const Lines& l) { std::string s; for (const auto& x : l) { s += x; s += '\n'; } return s; }
std::string upper(std::string s) { for (auto& c : s) c = std::toupper(static_cast<unsigned char>(c)); return s; }
std::string lower(std::string s) { for (auto& c : s) c = std::tolower(static_cast<unsigned char>(c)); return s; }
std::string rstrip(std::string s) { while (!s.empty() && std::isspace(static_cast<unsigned char>(s.back()))) s.pop_back(); return s; }
bool is_comment(const std::string& l) { return !l.empty() && (l[0] == 'C' || l[0] == 'c' || l[0] == '*' || l[0] == '!'); }
bool icontains_word(const std::string& text, const std::string& word) {
    return std::regex_search(text, std::regex("\\b" + word + "\\s*\\(", std::regex::icase));
}

// fixed form as f2c reads it: a leading tab means column 7 (a digit after it, a continuation line), tabs elsewhere
// become spaces, several statements on one line separated by ";" (Fortran 90) become one statement per line
Lines normalise(const Lines& in) {
    Lines out;
    for (std::string line : in) {
        line = rstrip(line);
        if (!line.empty() && line[0] == '\t') {
            std::string rest = line.substr(1);
            line = (!rest.empty() && rest[0] >= '1' && rest[0] <= '9') ? "     &" + rest.substr(1) : "      " + rest;
        }
        std::replace(line.begin(), line.end(), '\t', ' ');
        if (is_comment(line) || line.find(';') == std::string::npos) { out.push_back(line); continue; }
        std::vector<std::string> pieces; std::string cur; char q = 0; bool rest_as_is = false;
        for (size_t i = 6; i < line.size(); ++i) {
            char ch = line[i];
            if (q) { cur += ch; if (ch == q) q = 0; }
            else if (ch == '\'' || ch == '"') { q = ch; cur += ch; }
            else if (ch == ';') { pieces.push_back(cur); cur.clear(); }
            else if (ch == '!') { cur += line.substr(i); rest_as_is = true; break; }   // an inline comment: kept as it is
            else cur += ch;
        }
        (void)rest_as_is;
        pieces.push_back(cur);
        out.push_back(line.substr(0, 6) + pieces[0]);
        for (size_t k = 1; k < pieces.size(); ++k) {
            std::string p = pieces[k]; size_t b = p.find_first_not_of(' ');
            if (b != std::string::npos) out.push_back("      " + p.substr(b));
        }
    }
    return out;
}

// f2c ignores the columns beyond 72, gfortran's -ffixed-line-length-0 does not; fixed form concatenates the columns
// 7-72 of a continuation line to the statement, string literals included, so a long line can be cut at 72 anywhere
Lines wrap72(const Lines& in) {
    Lines out;
    for (std::string line : in) {
        line = rstrip(line);
        if (line.size() <= 72 || is_comment(line)) { out.push_back(line); continue; }
        out.push_back(line.substr(0, 72));
        for (size_t p = 72; p < line.size(); p += 66) out.push_back("     &" + line.substr(p, 66));
    }
    return out;
}

const std::regex unit_re("^\\s+(?:(?:REAL\\*8|DOUBLE PRECISION|INTEGER(?:\\*\\d)?|LOGICAL|REAL)\\s+)?(FUNCTION|SUBROUTINE)\\s+(\\w+)", std::regex::icase);
const std::regex end_re("^\\s+END(\\s+(FUNCTION|SUBROUTINE)\\b.*)?\\s*$", std::regex::icase);

struct Unit { std::string name; Lines lines; };

// every program unit, by name; what precedes the first one is dropped
std::vector<Unit> split_units(const Lines& in) {
    std::vector<Unit> units; Unit cur; bool open = false; std::smatch m;
    for (const auto& line : in) {
        if (!open) {
            if (!is_comment(line) && std::regex_search(line, m, unit_re)) { cur = Unit{ upper(m[2].str()), { line } }; open = true; }
        } else {
            cur.lines.push_back(line);
            if (std::regex_match(line, end_re)) { units.push_back(cur); open = false; }
        }
    }
    return units;
}

// the functions the model calls, transitively, by name
std::vector<Unit> closure(const std::vector<Unit>& units, const std::string& target, std::string& err) {
    std::map<std::string, const Unit*> byname; for (const auto& u : units) byname[u.name] = &u;
    if (!byname.count(upper(target))) {
        err = target + " is not defined in the file; functions found: ";
        for (const auto& u : units) err += u.name + " ";
        return {};
    }
    std::vector<std::string> keep, todo{ upper(target) };
    const std::regex call_re("\\b([A-Za-z]\\w*)\\s*\\(");
    while (!todo.empty()) {
        std::string n = todo.back(); todo.pop_back();
        if (std::find(keep.begin(), keep.end(), n) != keep.end()) continue;
        keep.push_back(n);
        for (const auto& line : byname[n]->lines) {
            if (is_comment(line)) continue;
            for (auto it = std::sregex_iterator(line.begin(), line.end(), call_re); it != std::sregex_iterator(); ++it) {
                std::string c = upper((*it)[1].str());
                if (byname.count(c) && c != n && std::find(keep.begin(), keep.end(), c) == keep.end()) todo.push_back(c);
            }
        }
    }
    std::vector<Unit> out; for (const auto& n : keep) out.push_back(*byname[n]);
    return out;
}

// gfortran knows DERF, DERFC and GAMMA as intrinsics, f2c does not: declared as external functions in every unit
// that uses them (the shim provides derf_, derfc_, gamma_), right after IMPLICIT NONE or the unit's header
Lines declare_intrinsics(Lines unit) {
    std::string body; for (const auto& l : unit) if (!is_comment(l)) body += l + "\n";
    std::vector<std::string> used;
    for (const char* f : { "DERF", "DERFC", "GAMMA" }) {
        if (!icontains_word(body, f)) continue;
        if (std::regex_search(body, std::regex(std::string("^\\s+(REAL\\*8|DOUBLE PRECISION|EXTERNAL)[^!\\n]*\\b") + f + "\\b", std::regex::icase | std::regex::multiline))) continue;
        used.push_back(f);
    }
    if (used.empty()) return unit;
    std::string decl = "      REAL*8 "; for (size_t i = 0; i < used.size(); ++i) decl += (i ? ", " : "") + used[i];
    for (size_t k = 0; k < unit.size(); ++k) {
        if (std::regex_search(unit[k], std::regex("^\\s+IMPLICIT\\s+NONE", std::regex::icase)) || std::regex_search(unit[k], unit_re)) {
            unit.insert(unit.begin() + k + 1, decl); break;
        }
    }
    return unit;
}

// f2c names: lower case, one trailing underscore, two when the name holds one
std::string f2c_name(const std::string& n) { return lower(n) + (n.find('_') != std::string::npos ? "__" : "_"); }

std::string read_file(const std::string& p) { std::ifstream f(p); std::stringstream b; b << f.rdbuf(); return b.str(); }

// f2c's C, line by line, as device code
std::string to_device(const Lines& c, const std::string& shim, const std::string& root, const std::string& file, const std::string& target) {
    const std::regex def_re("^(?:/\\* Subroutine \\*/ )?(doublereal|real|integer|logical|int|shortint|longint|double|float|char) (\\w+)\\(");
    const std::regex table_re("^\\s+static (doublereal|real|integer|logical) (\\w+)(\\[\\d+\\])? = \\{(.*)$");
    std::string header, functions; std::vector<std::string> hoisted;
    std::string fn; Lines body; bool in_fn = false, skip_builtins = false;
    std::vector<std::pair<std::string, std::string>> renames;   // table name -> hoisted name, for the current function
    auto flush_fn = [&]() {
        for (auto& l : body)
            for (const auto& r : renames) l = std::regex_replace(l, std::regex("\\b" + r.first + "\\b"), r.second);
        functions += join(body); body.clear(); renames.clear();
    };
    for (size_t i = 0; i < c.size(); ++i) {
        const std::string& line = c[i]; std::smatch m;
        if (!in_fn) {
            if (std::regex_search(line, m, def_re)) {
                in_fn = true; fn = m[2].str();
                body.push_back("NF_DEV " + std::regex_replace(line, std::regex("^/\\* Subroutine \\*/ "), ""));
            } else if (line.find("#include \"f2c.h\"") != std::string::npos) {
                continue;
            } else {
                std::string h = std::regex_replace(line, std::regex("^static (integer|doublereal|real|logical) "), "static NF_DEVCONST $1 ");
                h = std::regex_replace(h, std::regex("^struct \\{"), "NF_DEVVAR struct {");
                header += h + "\n";
            }
            continue;
        }
        if (skip_builtins) { if (line.empty() || line.find("/*") != std::string::npos) skip_builtins = false; else continue; }
        if (line.find("/* Builtin functions */") != std::string::npos) { skip_builtins = true; continue; }
        if (std::regex_search(line, m, table_re)) {
            // a DATA table: moved to constant memory at namespace scope under the function's name
            std::string init = m[4].str();
            size_t j = i;
            while (init.find("};") == std::string::npos && ++j < c.size()) init += "\n" + c[j];
            i = j;
            init = init.substr(0, init.rfind("};"));
            std::string hname = fn + "__" + m[2].str();
            hoisted.push_back("static NF_DEVCONST " + m[1].str() + " " + hname + m[3].str() + " = {" + init + "};");
            renames.emplace_back(m[2].str(), hname);
            continue;
        }
        std::string l = std::regex_replace(line, std::regex("^(\\s+)extern (?:/\\* Subroutine \\*/ )?(doublereal|real|integer|logical|int) "), "$1extern NF_DEVDECL $2 ");
        body.push_back(l);
        if (line.rfind("} /* " + fn + " */", 0) == 0) { in_fn = false; flush_fn(); }
    }
    if (in_fn) flush_fn();
    std::string o;
    o += "// Device source made by nested_fit from " + file + " with f2c: the functions " + target + " calls, nothing else.\n";
    o += "#if defined(__CUDACC__) || defined(__CUDACC_RTC__) || defined(__HIPCC__)\n#define NF_DEVVAR __device__\n#define NF_DEVDECL __device__\n#define NF_DEVCONST __constant__\n#else\n#define NF_DEVVAR\n#define NF_DEVDECL\n#define NF_DEVCONST\n#endif\n";
    // the shim without its include guard: the backend compiles the source twice (single and double)
    for (const auto& l : split_lines(shim)) if (!std::regex_match(l, std::regex("#(ifndef|define) NF_F2C_SHIM_H|#endif\\s*"))) o += l + "\n";
    o += header;
    for (const auto& h : hoisted) o += h + "\n";
    o += functions;
    o += "#undef abs\n#undef dabs\n#undef min\n#undef max\n#undef dmin\n#undef dmax\n";
    o += "NF_DEV double nf_user(double x, const double* params) { double xx = x; integer np = 4096; return " + root + "(&xx, &np, (doublereal*)params); }\n";
    return o;
}

std::string shell(const std::string& cmd, int* status) {
    std::string out; FILE* p = popen((cmd + " 2>&1").c_str(), "r");
    if (!p) { *status = -1; return out; }
    char buf[4096]; while (fgets(buf, sizeof buf, p)) out += buf;
    *status = pclose(p); return out;
}

} // namespace

// Translate `fortran_file`'s `function` into the device source `out_path`. `work_dir` holds the intermediate files
// (the reduced Fortran, f2c's C), `shim_path` is the cached nf_f2c_shim.h. Returns 0, or 1 with a one-line reason in err.
extern "C" int nf_f2c_translate(const char* fortran_file, const char* function, const char* shim_path, const char* work_dir, const char* out_path, char* err, int errlen) {
    auto fail = [&](const std::string& m) { std::snprintf(err, errlen, "%s", m.c_str()); return 1; };
    int st = 0;
    shell("f2c -v", &st);
    if (st != 0 && WEXITSTATUS(st) == 127) return fail("f2c is not installed (Ubuntu/Debian: sudo apt install f2c; Fedora: sudo dnf install f2c); a C file through function.gpu_source works without it");
    std::string src = read_file(fortran_file);
    if (src.empty()) return fail(std::string("cannot read ") + fortran_file);
    std::string e;
    std::vector<Unit> needed = closure(split_units(normalise(split_lines(src))), function, e);
    if (!e.empty()) return fail(e);
    Lines reduced; for (const auto& u : needed) { Lines d = declare_intrinsics(u.lines); reduced.insert(reduced.end(), d.begin(), d.end()); }
    std::string base = std::string(work_dir) + "/f2c_input";
    { std::ofstream f(base + ".f"); f << join(wrap72(reduced)); }
    std::remove((base + ".c").c_str());
    std::string log = shell("cd '" + std::string(work_dir) + "' && f2c -a -A f2c_input.f", &st);
    std::string c = read_file(base + ".c");
    if (c.empty()) {
        std::string why; std::istringstream is(log); std::string l;
        while (std::getline(is, l)) if (l.rfind("Error", 0) == 0) { why += (why.empty() ? "" : "; ") + l; if (why.size() > 300) break; }
        return fail("f2c could not translate it: " + (why.empty() ? log.substr(0, 300) : why));
    }
    std::string shim = read_file(shim_path);
    if (shim.empty()) return fail(std::string("the f2c shim is missing from the cache: ") + shim_path);
    std::string root = f2c_name(function);
    std::string dev = to_device(split_lines(c), shim, root, fortran_file, function);
    if (dev.find("NF_DEV doublereal " + root + "(") == std::string::npos && dev.find("NF_DEV real " + root + "(") == std::string::npos)
        return fail(std::string(function) + " came out of f2c under no recognised form");
    std::ofstream o(out_path); o << dev;
    return 0;
}
