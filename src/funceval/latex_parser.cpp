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
#include <iostream>
#include <vector>
#include <map>

typedef char ShortVarName[2];
typedef char LongVarName[64];

struct ParseOutput
{
    ShortVarName* parameters;    // The detected parameters in the expression
    ShortVarName* constants;     // The detected constants in the expression
    LongVarName*  functions;     // The detected function calls in the expression
    char*         infixcode_f90; // The latex code converted to f90 for compilation
};

static std::vector<std::pair<std::string, std::string>> find_args(
    const char ref1[3],
    const char ref2[3],
    const std::string& expression,
    const std::ptrdiff_t offset_start,
    std::string& input,
    std::function<std::string(const std::string&, const std::string&)> replace_template
)
{
    std::vector<std::pair<std::string, std::string>> args;
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
            if(*++it == ref1[0]) ref_count++;
            else if(*it == ref1[1]) ref_count--;
        }

        std::string arg1(searchStart + match.position(0) + offset_start, it);

        ref_count = 1;
        std::string::const_iterator it_den_start = ++++it;
        while(ref_count > 0)
        {
            if(*++it == ref2[0]) ref_count++;
            else if(*it == ref2[1]) ref_count--;
        }

        std::string arg2(it_den_start, it++);

        args.push_back(std::make_pair(arg1, arg2));

        replace.push_back(std::make_pair(match.position(0) + (searchStart - input.cbegin()), it - (searchStart + match.position(0))));
        replace_what.push_back(replace_template(arg1, arg2));
        searchStart = match.suffix().first;
    }

    std::ptrdiff_t offset = 0;
    for(size_t i = 0; i < replace.size(); i++)
    {
        input.replace(replace[i].first + offset, replace[i].second, replace_what[i]);
        offset = (replace_what[i].size() - replace[i].second);
    }

    return args;
}

static std::vector<std::string> find_arg(
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

        args.push_back(arg1);

        replace.push_back(std::make_pair(match.position(0) + (searchStart - input.cbegin()), it - (searchStart + match.position(0))));
        replace_what.push_back(replace_template(arg1));
        searchStart = match.suffix().first;
    }

    std::ptrdiff_t offset = 0;
    for(size_t i = 0; i < replace.size(); i++)
    {
        input.replace(replace[i].first + offset, replace[i].second, replace_what[i]);
        offset = (replace_what[i].size() - replace[i].second);
    }

    return args;
}

static void replace_token(const std::string& token, const std::string& replacement, std::string& input)
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
        offset = (replace_what[i].size() - replace[i].second);
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

static std::string StripInput(const std::string& input, const std::vector<std::string>& keywords)
{
    std::string output = input;

    for(const auto& kw : keywords)
    {
        output = RemoveAll(output, kw);
    }

    return output;
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

    // Strip x or digit
    searchStart = new_input.cbegin();
    while(std::regex_search(searchStart, new_input.cend(), match, std::regex("(?!x|\\d| ).{1}")))
    {
        parameters.push_back(match.str());
        searchStart = match.suffix().first;
    }

    return parameters;
}

static std::string SimplifyInput(const char* input_stream)
{
    std::string input(input_stream);
    std::smatch match;

    std::cout << input << std::endl;

    // Fractions
    (void)find_args("{}", "{}", "\\\\frac\\{", 6, input, [](const std::string& a1, const std::string& a2) { return "((" + a1 + ")/(" + a2 + "))"; });

    // User functions
    auto functions = find_args("{}", "()", "\\\\texttt\\{|mathrm\\{", 8, input, [](const std::string& a1, const std::string& a2) { return a1 + "(" + a2 + ")"; });

    // Built-ins
    const std::map<std::string, std::string> f90_builtins = {
        { "sqrt", "DSQRT" },
        { "exp",   "DEXP" },
        { "log",   "DLOG" },
        { "sin",   "DSIN" },
        { "cos",   "DCOS" },
        { "tan",   "DTAN" }
    };

    std::vector<std::string> built_ins;
    for(const auto& function : f90_builtins)
    {
        // Match '(' start
        auto match0 = find_arg("()", "\\\\" + function.first + "\\(", function.first.length() + 2, input, [=](const std::string& a) { return function.second + "(" + a + ")"; });
        if(match0.size() > 0 ) built_ins.insert(built_ins.end(), match0.begin(), match0.end());
        
        // Match '{' start
        auto match1 = find_arg("{}", "\\\\" + function.first + "\\{", function.first.length() + 2, input, [=](const std::string& a) { return function.second + "(" + a + ")"; });
        if(match1.size() > 0 ) built_ins.insert(built_ins.end(), match1.begin(), match1.end());
    }

    // Replace ^ by **
    replace_token("\\^", "**", input);

    std::vector<std::string> keywords;
    for(const auto& b : f90_builtins)
    {
        keywords.push_back(b.second);
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

    auto parameters = ExtractParameters(StripInput(input, keywords));

    // Place multiplication signs (*) in the final string


    // Replace \pi by pi


    for(const auto& p : parameters)
    {
        std::cout << p << std::endl;
    }
    std::cout << input << std::endl;

    return "";
}

ParseOutput ParseLatexToF90(const char* input_stream)
{
    SimplifyInput(input_stream);
    return {};
}

void FreeParseOutput(ParseOutput* output)
{
    if(output != nullptr)
    {
        if(output->parameters)    free(output->parameters);
        if(output->constants)     free(output->constants);
        if(output->functions)     free(output->functions);
        if(output->infixcode_f90) free(output->infixcode_f90);
    }
}

int main()
{
    ParseLatexToF90("a_0x + \\frac{a_1a_2\\texttt{gauss}(-x^2)}{a_3} + 2^\\frac{b_0 - \\log(b_1x)}{2\\pi\\sqrt{c_0x}+c_1d}");
    return 0;
}
