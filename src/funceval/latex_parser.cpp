// Brief  : Module for parsing latex math input (does not support all the features).
//          This is a very simple 'parser' that uses regex
// Author : César Godinho
// Date   : 26/07/2023

// Supported features :
// - Params need to be 1 letter (+1 optional subscript letter)
// - Function calls need to be under \mathrm{func_name}(other_func(x)) or \texttt{func_name}(other_func(x))
// - Supported built-ins : \frac, \sqrt, \exp, \log, \sin, \cos, \tan
// - Supported contants  : \pi
// - Superscript is not allowed (only for powers)

// Lets keep C++11 compliant
#include <memory>
#include <regex>
#include <string>
#include <cstring>
#include <iostream>
#include <vector>
#include <map>
#include <cassert>
#include <fstream>

extern "C" struct ParseOutput
{
    // ShortVarName* constants;  // The detected constants in the expression (Automatically converted for now)
    char* function_name;         // The detected function name
    char* parameter_names;       // The detected parameter names in the expression
    char* parameter_identifiers; // The detected parameter identifiers in the expression
    int   num_params;            // The number of parameters
    char* functions;             // The detected function calls in the expression
    int   num_funcs;             // The number of function calls
    int*  func_argc;             // The detected number of parameters for each called user function in 'functions'
    char* builtin_custom;        // The detected custom built in functions (declared inside internal_funcs.f90)
    int   num_builtin;           // The number of custom built in calls
    char* infixcode_f90;         // The latex code converted to f90 for compilation
    int   error;                 // The error code of the last error
};

#define LTXP_ERR_NOERR              0
#define LTXP_ERR_UNKOWN_PARAM       1
#define LTXP_ERR_INVALID_PARAMCOUNT 2
#define LTXP_ERR_INVALID_FUNCTION   3
#define LTXP_ERR_INVALID_PARAM_NAME 4
#define LTXP_ERR_NO_FUNCTION_NAME   5
#define LTXP_ERR_NO_X_ARGUMENT      6
#define LTXP_ERR_UNUSED_PARAMETER   7

#define ERR_MAP_ENTRY(x) {x, #x}
static const std::map<int, std::string> error_map = {
    ERR_MAP_ENTRY(LTXP_ERR_NOERR),
    ERR_MAP_ENTRY(LTXP_ERR_UNKOWN_PARAM),
    ERR_MAP_ENTRY(LTXP_ERR_INVALID_PARAMCOUNT),
    ERR_MAP_ENTRY(LTXP_ERR_INVALID_FUNCTION),
    ERR_MAP_ENTRY(LTXP_ERR_INVALID_PARAM_NAME),
    ERR_MAP_ENTRY(LTXP_ERR_NO_FUNCTION_NAME),
    ERR_MAP_ENTRY(LTXP_ERR_NO_X_ARGUMENT),
    ERR_MAP_ENTRY(LTXP_ERR_UNUSED_PARAMETER)
};

static std::vector<std::pair<std::string, std::string>> FindArgs(
    const char ref1[3],
    const char ref2[3],
    const std::string& expression,
    const std::ptrdiff_t offset_start,
    std::string& input,
    std::function<std::string(const std::string&, const std::string&)> replace_template,
    std::function<bool(const std::string&, const std::string&)> replace_check = std::function<bool(const std::string&, const std::string&)>([](const std::string&, const std::string&) -> bool { return true; })
)
{
    std::vector<std::pair<std::string, std::string>> args;
    std::smatch match;
    std::string::const_iterator searchStart(input.cbegin());
    std::vector<std::pair<std::ptrdiff_t, std::size_t>> replace;
    while(std::regex_search(searchStart, input.cend(), match, std::regex(expression)))
    {
        int ref_count = 1;
        std::string::const_iterator it = searchStart + match.position(0) + offset_start;
        while(ref_count > 0)
        {
            if(*++it == ref1[0]) ref_count++;
            else if(*it == ref1[1]) ref_count--;
        }

        std::string arg1(searchStart + match.position(0) + offset_start, it);
        // std::cout << arg1 << std::endl;

        ref_count = 1;
        std::string::const_iterator it_den_start = ++++it;
        while(ref_count > 0)
        {
            if(*++it == ref2[0]) ref_count++;
            else if(*it == ref2[1]) ref_count--;
        }

        std::string arg2(it_den_start, it++);
        // std::cout << arg2 << std::endl;
        args.push_back(std::make_pair(arg1, arg2));

        auto replace_p = std::make_pair(match.position(0) + (searchStart - input.cbegin()), it - (searchStart + match.position(0)));

        if(replace_check(arg1, arg2))
        {
            input.replace(replace_p.first, replace_p.second, replace_template(arg1, arg2));
            searchStart = input.cbegin();
        }
        else
        {
            searchStart = match.suffix().first;
        }
    }

    return args;
}

static std::vector<std::string> FindArg(
    const char ref[3],
    const std::string& expression,
    const std::ptrdiff_t offset_start,
    std::string& input,
    std::function<std::string(const std::string&)> replace_template
)
{
    std::vector<std::string> args;
    std::smatch match;
    std::string::const_iterator searchStart(input.cbegin());
    std::vector<std::pair<std::ptrdiff_t, std::size_t>> replace;
    std::vector<std::string> replace_what;
    while(std::regex_search(searchStart, input.cend(), match, std::regex(expression)))
    {
        int ref_count = 1;
        std::string::const_iterator it = searchStart + match.position(0) + offset_start;
        while(ref_count > 0)
        {
            if(*++it == ref[0]) ref_count++;
            else if(*it == ref[1]) ref_count--;
        }

        std::string arg1(searchStart + match.position(0) + offset_start, it++);
        // std::cout << arg1 << std::endl;
        args.push_back(arg1);

        replace.push_back(std::make_pair(match.position(0) + (searchStart - input.cbegin()), it - (searchStart + match.position(0))));
        replace_what.push_back(replace_template(arg1));
        searchStart = match.suffix().first;
    }

    std::ptrdiff_t offset = 0;
    for(size_t i = 0; i < replace.size(); i++)
    {
        input.replace(replace[i].first + offset, replace[i].second, replace_what[i]);
        offset += (replace_what[i].size() - replace[i].second);
    }

    return args;
}

static std::vector<std::string> FindStrings(const std::string& input)
{
    std::vector<std::string> strings;
    std::smatch match;
    std::string::const_iterator searchStart(input.cbegin());
    while(std::regex_search(searchStart, input.cend(), match, std::regex("\".+?\"")))
    {
        strings.push_back(match[0]);
        searchStart = match.suffix().first;
    }

    return strings;
}

static void ReplaceToken(const std::string& token, const std::string& replacement, std::string& input)
{
    std::smatch match;
    std::string::const_iterator searchStart(input.cbegin());
    std::vector<std::pair<std::ptrdiff_t, std::size_t>> replace;
    std::vector<std::string> replace_what;
    while(std::regex_search(searchStart, input.cend(), match, std::regex(token)))
    {
        replace.push_back(std::make_pair(match.position(0) + (searchStart - input.cbegin()), match.str().length()));
        replace_what.push_back(replacement);
        searchStart = match.suffix().first;
    }

    std::ptrdiff_t offset = 0;
    for(size_t i = 0; i < replace.size(); i++)
    {
        input.replace(replace[i].first + offset, replace[i].second, replace_what[i]);
        offset += (replace_what[i].size() - replace[i].second);
    }
}

static void InsertAfterToken(
    const std::string& token,
    const std::string& what,
    std::string& input,
    std::function<bool(const std::ptrdiff_t&, const std::string&)> condition
)
{
    std::smatch match;
    std::string::const_iterator searchStart(input.cbegin());
    std::vector<std::ptrdiff_t> position;
    std::vector<std::string> insert_what;
    while(std::regex_search(searchStart, input.cend(), match, std::regex(token)))
    {
        position.push_back(match.position(0) + (searchStart - input.cbegin()) + match.str().length());
        insert_what.push_back(what);
        searchStart = match.suffix().first;
    }

    std::ptrdiff_t offset = 0;
    for(size_t i = 0; i < position.size(); i++)
    {
        if(condition(position[i] + offset, input))
        {
            input.insert(position[i] + offset, insert_what[i]);
            offset += insert_what[i].size();
        }
    }
}

static std::string RemoveAll(const std::string& input, const std::string& kw)
{
    std::string output;
    output.reserve(input.length());
    std::string::size_type lastPos = 0;
    std::string::size_type findPos;

    while(std::string::npos != (findPos = input.find(kw, lastPos)))
    {
        output.append(input, lastPos, findPos - lastPos);
        // output += to;
        lastPos = findPos + kw.length();
    }

    // Care for the rest after last occurrence
    output += input.substr(lastPos);

    return output;
}

static std::string StripInput(const std::string& input, const std::vector<std::string>& keywords, std::string extra_regex = std::string())
{
    std::string output = input;
    
    for(const auto& kw : keywords)
    {
        output = RemoveAll(output, kw);
    }

    // Extra regex strip
    if(!extra_regex.empty())
    {
        ReplaceToken(extra_regex, "", output);
    }

    return output;
}

static void AddMultiplicationSignsReduceVars(std::string& input, const std::vector<std::pair<std::string, std::string>>& user_funcs, const std::vector<std::string>& builtin_funcs)
{
    if(user_funcs.size() > ('Z' - 'A' + 1))
    {
        std::cout << "Parsing : For now, only " << ('Z' - 'A' + 1) << " function calls are allowed per expression" << std::endl;
    }

    // Replace user defined function names on this stage just to ignore the rules on them
    const std::string dummy_name_U("USERFCN");
    char id = 'A';
    for(const auto& f : user_funcs)
    {
        ReplaceToken(f.first, dummy_name_U + id++, input);
    }

    // Replace builtin function names on this stage just to ignore the rules on them
    const std::string dummy_name_B("BUILTIN");
    id = 'A';
    for(const auto& f : builtin_funcs)
    {
        ReplaceToken(f, dummy_name_B + id++, input);
    }

    // Detect and replace strings before evaluating
    std::vector<std::string> strings = FindStrings(input);
    const std::string dummy_name_S("STRING");
    id = 'A';
    for(const auto& s : strings)
    {
        ReplaceToken(s, dummy_name_S + id++, input);
    }

    // Rule for closing braces
    InsertAfterToken("\\)", "*", input, [](const std::ptrdiff_t& i, const std::string& input) -> bool {
        return input[i] != ')' &&
               input[i] != '+' &&
               input[i] != '-' &&
               input[i] != '*' &&
               input[i] != '/' &&
               input[i] != ',' &&
               i != static_cast<std::ptrdiff_t>(input.length());
    });

    // Rule for digits/numbers
    InsertAfterToken("\\d", "*", input, [](const std::ptrdiff_t& i, const std::string& input) -> bool {
        return input[i] != ')' &&
               input[i] != '+' &&
               input[i] != '-' &&
               input[i] != '*' &&
               input[i] != '/' &&
               input[i] != ',' &&
               input[i] != '.' &&
               (input[i]  < '0' || input[i]  > '9') && i != static_cast<std::ptrdiff_t>(input.length());
    });

    // Rule for variable x
    InsertAfterToken("x", "*", input, [](const std::ptrdiff_t& i, const std::string& input) -> bool {
        return input[i] != ')' &&
               input[i] != '+' &&
               input[i] != '-' &&
               input[i] != '*' &&
               input[i] != '/' &&
               input[i] != ',' &&
               input[i] != '_' &&
               i != static_cast<std::ptrdiff_t>(input.length());
    });

    // Rule for identifiers
    ReplaceToken("\\\\pi", "PI", input); // Just so we don't match \\\\pi with the [a-z] rule
    InsertAfterToken("\\w_.|[a-z]", "*", input, [](const std::ptrdiff_t& i, const std::string& input) -> bool {
        return input[i] != ')' &&
               input[i] != '+' &&
               input[i] != '-' &&
               input[i] != '*' &&
               input[i] != '/' &&
               input[i] != ',' &&
               input[i] != '_' &&
               i != static_cast<std::ptrdiff_t>(input.length());
    });
    ReplaceToken("PI", "\\pi", input);

    // Rule for constants (\pi)
    InsertAfterToken("\\\\pi", "*", input, [](const std::ptrdiff_t& i, const std::string& input) -> bool {
        return input[i] != ')' &&
               input[i] != '+' &&
               input[i] != '-' &&
               input[i] != '*' &&
               input[i] != '/' &&
               input[i] != ',' &&
               i != static_cast<std::ptrdiff_t>(input.length());
    });

    // Find all capital 1 char tokens (for that -> Lowercase all of the userfcns and builtins placeholders previously set)
    const std::string dummy_name_U_Lower("userfcn");
    id = 'A';
    for(const auto& f : user_funcs)
    {
        (void)f;
        std::string by = dummy_name_U_Lower + (char)std::tolower(id);
        ReplaceToken(dummy_name_U + id++, by, input);
    }

    const std::string dummy_name_B_Lower("builtin");
    id = 'A';
    for(const auto& f : builtin_funcs)
    {
        (void)f;
        std::string by = dummy_name_B_Lower + (char)std::tolower(id);
        ReplaceToken(dummy_name_B + id++, by, input);
    }

    const std::string dummy_name_S_Lower("string");
    id = 'A';
    for(const auto& s : strings)
    {
        (void)s;
        std::string by = dummy_name_S_Lower + (char)std::tolower(id);
        ReplaceToken(dummy_name_S + id++, by, input);
    }

    // Rule for [A-Z] identifiers (these are tricky)
    InsertAfterToken("[A-Z]", "*", input, [](const std::ptrdiff_t& i, const std::string& input) -> bool {
        return input[i] != ')' &&
               input[i] != '+' &&
               input[i] != '-' &&
               input[i] != '*' &&
               input[i] != '/' &&
               input[i] != ',' &&
               input[i] != '_' &&
               i != static_cast<std::ptrdiff_t>(input.length());
    });

    // Reduce identifier names before putting back function names to avoid collision
    input = RemoveAll(input, "_");

    // Put back the user defined functions with their names
    id = 'a';
    for(const auto& f : user_funcs)
    {
        ReplaceToken(dummy_name_U_Lower + id++, f.first, input);
    }

    // Put back the builtin functions with their names
    id = 'a';
    for(const auto& f : builtin_funcs)
    {
        ReplaceToken(dummy_name_B_Lower + id++, f, input);
    }

    // Put back the strings with their names
    id = 'a';
    for(const auto& s : strings)
    {
        ReplaceToken(dummy_name_S_Lower + id++, s, input);
    }
}

static std::vector<std::string> ExtractParameters(const std::string& stripped_input)
{
    std::vector<std::string> parameters;
    std::smatch match;
    std::string::const_iterator searchStart(stripped_input.cbegin());
    while(std::regex_search(searchStart, stripped_input.cend(), match, std::regex("\\w_.")))
    {
        parameters.push_back(match.str());
        searchStart = match.suffix().first;
    }

    std::string new_input = StripInput(stripped_input, parameters);

    // Strip x or digit or spaces or digit.digit
    searchStart = new_input.cbegin();
    while(std::regex_search(searchStart, new_input.cend(), match, std::regex("(?!x|\\d| |\\.|,).{1}")))
    {
        parameters.push_back(match.str());
        searchStart = match.suffix().first;
    }

    std::sort(parameters.begin(), parameters.end());
    parameters.erase(std::unique(parameters.begin(), parameters.end()), parameters.end());

    return parameters;
}

static std::vector<std::string> StringSplit(const std::string& input, const char delimiter)
{
    std::vector<std::string> output;
    std::stringstream ss(input);
    std::string token;

    while(std::getline(ss, token, delimiter))
    {
        output.push_back(token);
    }

    return output;
}

static std::string ArrayifyFunctionCall(const std::string& argument_string)
{
    auto arguments = StringSplit(argument_string, ',');
    
    // Force the array elements to be real
    std::string output = arguments[0] + "," + std::to_string(arguments.size() - 1) + ",[real::";

    for(size_t i = 1; i < arguments.size() - 1; i++)
    {
        output += arguments[i] + ",";
    }

    return output + arguments.back() + "]";
}

static std::string TrimLR(const std::string& input)
{
    std::string output(input);
    output.erase(output.begin(), std::find_if(output.begin(), output.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
    output.erase(std::find_if(output.rbegin(), output.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), output.end());
    return output;
}

static int ArgumentCount(const std::string& input)
{
    if(TrimLR(input).size() == 0) return 0;

    int argc = 1;
    for(int i = 0; i < static_cast<int>(input.size()); i++)
    {
        if(input[i] == '(')
        {
            while(input[++i] != ')');
        }
        if(input[i] == ',') argc++;
    }
    return argc;
}

static std::tuple<std::string, std::vector<std::string>, std::vector<std::string>, std::vector<int>, std::vector<std::string>> SimplifyInput(const std::string& input_stream, const std::vector<std::string>& declared_args, int* error)
{
    const std::string start_string(input_stream);
    std::string input(input_stream);
    std::smatch match;

    // Fractions
    (void)FindArgs("{}", "{}", "\\\\frac\\{", 6, input, [](const std::string& a1, const std::string& a2) { return "((" + a1 + ")/(" + a2 + "))"; });

    // Built-ins
    // Internal functions (fortran native generic ones)
    const std::map<std::string, std::string> f90_builtins = {
        { "sqrt",   "SQRT"  },

        { "exp",    "EXP"   },
        { "log",    "LOG"   },

        { "sin",    "SIN"   },
        { "cos",    "COS"   },
        { "tan",    "TAN"   },
        { "arcsin", "ASIN"  },
        { "arccos", "ACOS"  },
        { "arctan", "ATAN"  },
        
        { "Gamma",  "GAMMA" },
        { "abs",    "ABS"   },
        { "erf",    "ERF"   },
        { "sign",   "SIGN"  }
    };

    // Internal functions (basically custom built-ins) (values come from cmake)
    const std::vector<std::string> custom_builtins = StringSplit("@Internal_CustomBuiltins@", ';');

    std::vector<std::string> built_ins;
    for(const auto& function : f90_builtins)
    {
        // Match '(' start
        auto match0 = FindArg("()", "\\\\" + function.first + "\\(", function.first.length() + 2, input, [=](const std::string& a) { return function.second + "(" + a + ")"; });
        if(match0.size() > 0) built_ins.insert(built_ins.end(), 1, function.second);
        
        // Match '{' start
        auto match1 = FindArg("{}", "\\\\" + function.first + "\\{", function.first.length() + 2, input, [=](const std::string& a) { return function.second + "(" + a + ")"; });
        if(match1.size() > 0) built_ins.insert(built_ins.end(), 1, function.second);
    }

    // Match '{}()' user function like built-in functions
    (void)FindArgs("{}", "()", "\\\\texttt\\{|mathrm\\{", 8, input, [&](const std::string& a1, const std::string& a2) {
        if(f90_builtins.find(a1) != f90_builtins.end())
        {
            const std::string builtin_name = f90_builtins.at(a1);
            built_ins.insert(built_ins.end(), 1, builtin_name);

            // TODO(César) : Use our own sign function instead
            if(builtin_name == "SIGN") // Special rule for the fortran "stupid" sign function
            {
                built_ins.insert(built_ins.end(), 1, "REAL"); // Small workaround
                return "SIGN(REAL(1.0, 8)," + a2 + ")";
            }

            return builtin_name + "(" + a2 + ")";
        }

        auto it = std::find(custom_builtins.begin(), custom_builtins.end(), a1);
        if(it != custom_builtins.end())
        {
            const std::string builtin_name = *it;
            built_ins.insert(built_ins.end(), 1, builtin_name);

            return builtin_name + "(" + a2 + ")";
        }
        
        return a1 + "(" + a2 + ")"; // NOTE(César) : This never runs
    },
    [=](const std::string& a1, const std::string& a2) { // Skip if this is not a built-in
        if(f90_builtins.find(a1) != f90_builtins.end() || std::find(custom_builtins.begin(), custom_builtins.end(), a1) != custom_builtins.end())
        {
            return true;
        }
        else
        {
            return false;
        }
    });

    std::sort(built_ins.begin(), built_ins.end());
    built_ins.erase(std::unique(built_ins.begin(), built_ins.end()), built_ins.end());

    // User functions
    auto functions = FindArgs("{}", "()", "\\\\texttt\\{|mathrm\\{", 8, input, [](const std::string& a1, const std::string& a2) {
        return a1 + "(" + a2 + ")";
    });

    // Replace ^ by **
    ReplaceToken("\\^", "**", input);

    std::vector<std::string> keywords;
    for(const auto& b : built_ins)
    {
        keywords.push_back(b);
    }
    for(const auto& f : functions)
    {
        keywords.push_back(f.first);
    }
    keywords.push_back("\\pi");
    keywords.push_back("(");
    keywords.push_back(")");
    keywords.push_back("/");
    keywords.push_back("+");
    keywords.push_back("-");
    keywords.push_back("*");
    keywords.push_back("REAL"); // Small workaround

    auto parameters = ExtractParameters(StripInput(input, keywords, "\".+?\""));

    // Remove spaces
    input = RemoveAll(input, " ");

    // Place multiplication signs (*) in the final string
    AddMultiplicationSignsReduceVars(input, functions, built_ins);

    // Replace \pi by pi
    ReplaceToken("\\\\pi", "pi", input);

    // Make functions unique based on their name
    std::sort(functions.begin(), functions.end(), [](auto& left, auto& right) {
        return left.first < right.first; 
    });
    functions.erase(std::unique(functions.begin(), functions.end(), [](auto& left, auto& right) {
        return left.first == right.first; 
    }), functions.end());

    // Make array like function calls for each user defined function
    for(const auto& f : functions)
    {
        FindArg("()", f.first + "\\(", f.first.length() + 1, input, [=](const std::string& a) { return f.first + "(" + ArrayifyFunctionCall(a) + ")"; });
    }

    if(auto idx = input.find("\\") != std::string::npos)
    {
        std::cout << "Error at: " << input.substr(idx+1, 2) <<  ". Unrecognized parameter." << std::endl;
        if(error != nullptr) *error = LTXP_ERR_UNKOWN_PARAM;
        std::cout << input << std::endl;
    }

    if((error != nullptr && *error == LTXP_ERR_NOERR) || error == nullptr)
    {
        std::cout << "========= Parsing result =========" << std::endl;

        std::cout << "     Latex input: ";
        std::cout << start_string << std::endl;

        std::cout << "Infix expression: ";
        std::cout << input << std::endl;
        std::cout << "      Parameters: ";
        if(parameters.size() > 0)
        {
            for(size_t i = 0; i < parameters.size() - 1; i++)
            {
                std::cout << parameters[i] << ", ";
            }
            std::cout << parameters[parameters.size() - 1] << std::endl;
        }
        else
        {
            std::cout << "None" << std::endl;
        }
        std::cout << "==================================" << std::endl;
    }

    // Pass the function identifiers and arg count
    std::vector<std::string> function_names;
    std::vector<int> function_argcount;

    function_names.reserve(functions.size());
    for(const auto& f : functions)
    {
        function_names.push_back(f.first);
        function_argcount.push_back(ArgumentCount(f.second));
    }

    return std::make_tuple(input, parameters, function_names, function_argcount, custom_builtins);
}

static std::pair<std::string, std::string> SplitExpression(const std::string& input, int* error)
{
    auto loc = input.find('=');
    if(loc != std::string::npos)
    {
        return std::make_pair(TrimLR(input.substr(0, loc-1)), TrimLR(input.substr(loc+1)));
    }
    else
    {
        if(error != nullptr) *error = LTXP_ERR_NO_FUNCTION_NAME;
        std::cout << "Error on parsing: Function name not found." << std::endl;
        std::cout << "Error on parsing: Please use the '<func_name>(x, p0, p1, ...)=<expression>' syntax." << std::endl;
        return std::make_pair("", "");
    }
}

static std::pair<std::string, std::vector<std::string>> HandleHeader(const std::string& input, int* error)
{
    std::pair<std::string, std::vector<std::string>> output;
    if(error != nullptr) *error = LTXP_ERR_NOERR;

    auto split_left  = StringSplit(input, '(');
    std::string func_name = split_left[0];
    std::string remainder = split_left[1];
    output.first = TrimLR(func_name);

    // TODO(César) : Deduce 1D or 2D from parameter list
    auto split_right = StringSplit(split_left[1], ')');
    std::string parameters_string = split_right[0];
    auto parameters  = StringSplit(parameters_string, ','); // Declared parameters

    auto x_present = std::find(parameters.begin(), parameters.end(), "x");
    if(x_present == parameters.end())
    {
        if(error != nullptr) *error = LTXP_ERR_NO_X_ARGUMENT;
        return std::make_pair("", std::vector<std::string>());
    }

    // x is not truly a parameter (just an argument)
    parameters.erase(x_present);

    for(const auto& p : parameters)
    {
        const std::string tp = TrimLR(p);
        if(tp.size() > 3)
        {
            std::cout << "Error on parsing: Declared parameter `" << p << "` exceeds maximum allowed size." << std::endl;
            std::cout << "Error on parsing: Parameters must be no larger than 3 chars (and either of the form <char>_<char> or <char>)." << std::endl;
            if(error != nullptr) *error = LTXP_ERR_INVALID_PARAM_NAME;
            return std::make_pair("", std::vector<std::string>());
        }
        output.second.push_back(tp);
    }

    return output;
}

extern "C" ParseOutput ParseLatexToF90(const char* input_stream)
{
    ParseOutput return_val = { 0 };
    return_val.error = LTXP_ERR_NOERR;

    auto split = SplitExpression(input_stream, &return_val.error);
    if(return_val.error != LTXP_ERR_NOERR)
    {
        return return_val;
    }

    // Handle the function name + parameters
    auto header_out = HandleHeader(split.first, &return_val.error);
    if(return_val.error != LTXP_ERR_NOERR)
    {
        return return_val;
    }

    return_val.function_name = (char*)malloc(sizeof(char) * 128);
    return_val.function_name[0] = 0;
    strcpy(return_val.function_name, header_out.first.c_str());

    // Handle the expression per se
    auto output = SimplifyInput(split.second, header_out.second, &return_val.error);
    if(return_val.error != LTXP_ERR_NOERR)
    {
        return return_val;
    }

    const std::string infix_code              = std::get<0>(output);
    const std::vector<std::string> parameters = std::get<1>(output);
    const std::vector<std::string> functions  = std::get<2>(output);
    const std::vector<int>         call_argc  = std::get<3>(output);
    const std::vector<std::string> custom_bi  = std::get<4>(output);

    // Make sure there are no 'rogue' parameters
    for(const auto& p : header_out.second)
    {
        if(std::find(parameters.begin(), parameters.end(), p) == parameters.end())
        {
            std::cout << "Error: Declared parameter `" << p << "` is not used." << std::endl;
            return_val.error = LTXP_ERR_UNUSED_PARAMETER;
        }
    }

    for(const auto& p : parameters)
    {
        if(std::find(header_out.second.begin(), header_out.second.end(), p) == header_out.second.end())
        {
            std::cout << "Error: Unkown parameter `" << p << "`." << std::endl;
            return_val.error = LTXP_ERR_UNKOWN_PARAM;
        }
    }

    if(return_val.error != LTXP_ERR_NOERR)
    {
        return return_val;
    }

    assert(call_argc.size() == functions.size());

    return_val.infixcode_f90 = (char*)malloc(sizeof(char) * (infix_code.size() + 1));
    strcpy(return_val.infixcode_f90, infix_code.c_str());

    return_val.functions = (char*)malloc(sizeof(char) * 4096);
    return_val.functions[0] = 0;
    return_val.num_funcs = static_cast<int>(functions.size());
    return_val.func_argc = (int*)malloc(sizeof(int) * functions.size());
    for(size_t i = 0; i < functions.size(); i++)
    {
        strcat(return_val.functions, (functions[i] + " ").c_str());
        return_val.func_argc[i] = call_argc[i];
    }

    return_val.parameter_names       = (char*)malloc(sizeof(char) * 4096);
    return_val.parameter_identifiers = (char*)malloc(sizeof(char) * 4096);
    return_val.parameter_names[0] = 0;
    return_val.parameter_identifiers[0] = 0;
    return_val.num_params = static_cast<int>(header_out.second.size());
    for(size_t i = 0; i < header_out.second.size(); i++)
    {
        strcat(return_val.parameter_names, (header_out.second[i] + " ").c_str());

        std::string p = RemoveAll(header_out.second[i], "_").c_str();
        strcat(return_val.parameter_identifiers, (p + " ").c_str());
    }

    return_val.num_builtin = static_cast<int>(custom_bi.size());
    return_val.builtin_custom = (char*)malloc(sizeof(char) * 4096);
    for(size_t i = 0; i < custom_bi.size(); i++)
    {
        strcat(return_val.builtin_custom, (custom_bi[i] + " ").c_str());
    }

    return return_val;
}

extern "C" void FreeParseOutput(ParseOutput* output)
{
    if(output != nullptr)
    {
        if(output->function_name)         free(output->function_name);
        if(output->parameter_names)       free(output->parameter_names);
        if(output->parameter_identifiers) free(output->parameter_identifiers);
        if(output->functions)             free(output->functions);
        if(output->func_argc)             free(output->func_argc);
        if(output->builtin_custom)        free(output->builtin_custom);
        // if(output->constants)          free(output->constants);
        if(output->infixcode_f90)         free(output->infixcode_f90);
    }
}

extern "C" void GetErrorMsg(ParseOutput* output, char* buffer)
{
    if(output->error)
    {
        strcpy(buffer, error_map.at(output->error).c_str());
    }
}

/*
* Cache specification:
* funcname - num_params - date
*/
extern "C" void CheckParseValidity(ParseOutput* output, const char* function_cache_path)
{
    std::ifstream input(function_cache_path);
    std::string line;

    std::vector<std::string> names;
    std::vector<int> varcount;

    while(std::getline(input, line))
    {
        auto        cache_line = StringSplit(line, '-');
        std::string name       = TrimLR(cache_line[0]);
        int         num_vars   = std::atoi(cache_line[1].c_str());
        std::string date_mod   = TrimLR(cache_line[2]);
        names.push_back(name);
        varcount.push_back(num_vars);
    }

    auto functions = StringSplit(output->functions, ' ');
    for(int i = 0; i < output->num_funcs; i++)
    {
        bool func_in_cache = false;
        for(int j = 0; j < static_cast<int>(names.size()); j++)
        {
            if(functions[i] == names[j])
            {
                func_in_cache = true;
                if(output->func_argc[i] != varcount[j])
                {
                    std::cout << "Error on function call `" << functions[i] <<  "`: Function takes " << varcount[j] << " parameters, but " << output->func_argc[i] << " were specified." << std::endl;
                    output->error = LTXP_ERR_INVALID_PARAMCOUNT;
                }
            }
        }

        if(!func_in_cache)
        {
            std::cout << "Error on function call `" << functions[i] <<  "`: Function is not declared in nested_fit cache." << std::endl;
            output->error = LTXP_ERR_INVALID_FUNCTION;
        }
    }
}
