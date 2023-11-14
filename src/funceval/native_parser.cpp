// Brief  : This finds both fortran and c++ function declaration signatures for the auto_func.f90 module.
//          It is written in c++ solely for the simplicity of implementing regex here.
// Author : César Godinho
// Date   : 10/11/2023

#include <regex>
#include <string>
#include <fstream>
#include <vector>
#include <queue>
#include <iostream>

extern "C" struct NativeOutput
{
    char* function_name;
    char* parameter_names;
    int   num_params;
    int   error;
};

#define NTP_ERR_NOERR               0
#define NTP_ERR_FILE_NOT_FOUND      1
#define NTP_ERR_VAR_IDX_SKIP        2
#define NTP_ERR_MAIN_FUNC_NOT_FOUND 3
#define NTP_ERR_MULTIPLE_MAIN_FUNC  4
#define NTP_ERR_INVALID_SIGNATURE   5
#define NTP_ERR_INVALID_LANG        6

#define ERR_MAP_ENTRY(x) {x, #x}
static const std::map<int, std::string> error_map = {
    ERR_MAP_ENTRY(NTP_ERR_NOERR),
    ERR_MAP_ENTRY(NTP_ERR_FILE_NOT_FOUND),
    ERR_MAP_ENTRY(NTP_ERR_VAR_IDX_SKIP),
    ERR_MAP_ENTRY(NTP_ERR_MAIN_FUNC_NOT_FOUND),
    ERR_MAP_ENTRY(NTP_ERR_MULTIPLE_MAIN_FUNC),
    ERR_MAP_ENTRY(NTP_ERR_INVALID_SIGNATURE),
    ERR_MAP_ENTRY(NTP_ERR_INVALID_LANG)
};

static std::string OpenFile(const char* filename, int* error)
{
    std::ifstream file(filename);
    if(!file.is_open())
    {
        *error = NTP_ERR_FILE_NOT_FOUND;
        return "";
    }

    std::stringstream ss;
    ss << file.rdbuf();
    return ss.str();
}

static void StripCommentsAndDefines(const std::string& lang, std::string& fileData)
{
    if(lang == "cpp")
    {
        // Only strip starting comments
        fileData = std::regex_replace(fileData, std::regex("//.*"), "");
    }
    else if(lang == "f90")
    {
        // Only strip starting comments
        fileData = std::regex_replace(fileData, std::regex("!.*"), "");
    }
    fileData = std::regex_replace(fileData, std::regex("#define .*"), "");
}

static std::vector<std::string> FindFunctionParams(const std::string& lang, const std::string& func, int* error)
{
    const std::string param_dcl_macro = "DCL_VAR"; // TODO(César): This could come from CMake config
    std::string mutable_func = func;

    using QueuePair = std::pair<std::string, size_t>;
    
    auto cmp_func = [](const QueuePair& left, const QueuePair& right) { return left.second > right.second; };
    std::priority_queue<QueuePair, std::vector<QueuePair>, decltype(cmp_func)> queue(cmp_func);

    std::vector<std::string> params;

    for(std::smatch match; std::regex_search(mutable_func, match, std::regex(param_dcl_macro + "\\(\"(\\w+)\" *, *(\\d+)\\)"));)
    {
        queue.push(QueuePair(match[1].str(), std::atoll(match[2].str().c_str())));
        mutable_func = match.suffix();
    }
    
    // Start index for c++ or fortran
    const size_t start_index = lang == "cpp" ? 0 : 1;

    // Check if there is illegal indexing
    for(size_t i = 0; i < queue.size(); i++)
    {
        QueuePair p = queue.top();
        queue.pop();

        if(p.second != i + start_index)
        {
            *error = NTP_ERR_VAR_IDX_SKIP;
            return std::vector<std::string>();
        }
        params.push_back(p.first);
    }
    return params;
}

static std::string FindExportFunction(const std::string& lang, const std::string& filedata, int* error)
{
    std::smatch match;
    const std::string func_dcl_macro = "NF_FUNC_EXPORT"; // TODO(César): This could come from CMake config
    std::regex func_regex(func_dcl_macro + (lang == "cpp" ? " +([\\s\\S]*})" : " +([\\s\\S]*(END FUNCTION|end function))"));
    std::string::const_iterator file_iter = filedata.cbegin();
    std::vector<std::string> functions;
    
    // NOTE(César): Do not break the loop here, maybe we will support multiple main functions in the future
    //              For now, I think this is kind of confusing if one gets many exports in one file
    while(std::regex_search(file_iter, filedata.cend(), match, func_regex))
    {
        functions.push_back(match[1].str());
        file_iter = match[0].second;
    }

    if(functions.size() > 1)
    {
        *error = NTP_ERR_MULTIPLE_MAIN_FUNC;
        return "";
    }

    if(functions.empty())
    {
        *error = NTP_ERR_MAIN_FUNC_NOT_FOUND;
        return "";
    }

    return functions[0];
}

static std::string FindFunctionName(const std::string& lang, const std::string& func, int* error)
{
    std::smatch match;
    if(lang == "cpp")
    {
        if(std::regex_search(func.cbegin(), func.cend(), match, std::regex("(.*?)\\( *(.+?), *(.+?), *(.+?)\\)")))
        {
            // There was a match but signature is wrong
            if(match.size() != 5)
            {
                *error = NTP_ERR_INVALID_SIGNATURE;
                return "Function does not contain the right signature => `extern \"C\" double <fname>(double*, int*, double*)`."; 
            }

            // Check the parameter constraints
            if(!match[1].str().rfind("double*", 0) || !match[2].str().rfind("int*", 0) || !match[3].str().rfind("double*", 0))
            {
                *error = NTP_ERR_INVALID_SIGNATURE;
                return "Function does not contain the right parameter signature => `(double*, int*, double*)`.";
            }
            
            std::string name = match[0].str();
            std::smatch ssmatch;
            if(std::regex_search(name.cbegin(), name.cend(), ssmatch, std::regex("extern +\"C\" +double +(.*?)\\(")))
            {
                *error = NTP_ERR_NOERR;
                return ssmatch[1].str();
            }
          
            *error = NTP_ERR_INVALID_SIGNATURE;
            return "Function does not contain the right signature => `extern \"C\" double <fname>`.";
        
        }
        
        *error = NTP_ERR_INVALID_SIGNATURE;
        return "Function not found or Function does not contain the right signature => `extern \"C\" double <fname>(double*, int*, double*)`.";
    }
    else if(lang == "f90")
    { 
        if(std::regex_search(func.cbegin(), func.cend(), match, std::regex("(function|FUNCTION) +(.*?)\\(.+?,.+?,.+?\\) +bind\\( *c *, *name *= *'(.+?)'\\)")))
        {
            // There was a match but the signature was wrong
            if(match.size() != 4)
            {
                *error = NTP_ERR_INVALID_SIGNATURE;
                return "Function not found or Function does not contain the right signature => `function <fname>(real(8) x, integer(4) npar, real(8) params(npar)) bind(c, name='<fname>')`.";
            }

            std::string name = match[2].str();
            std::string lc_name = match[3].str();

            for(size_t i = 0; i < name.size(); i++)
            {
                if(std::tolower(name[i]) != lc_name[i])
                {
                    *error = NTP_ERR_INVALID_SIGNATURE;
                    return "Function <fname> must match the name in `name=<fname>` (which must be lowercase in the latter).";
                }
            }

            // Match for x
            if(!std::regex_search(func.cbegin(), func.cend(), match, std::regex("(real|REAL)\\( *8 *\\) *, *(intent|INTENT)\\((in|IN)\\) *:: *\\w+ *\\n")))
            {
                *error = NTP_ERR_INVALID_SIGNATURE;
                return "Function requires an `x` argument with signature => `real(8), intent(in) :: x`.";
            }
            
            // Match for npar
            if(!std::regex_search(func.cbegin(), func.cend(), match, std::regex("(integer|INTEGER) *, *(intent|INTENT)\\((in|IN)\\) *:: *(\\w+) *\\n")))
            {
                *error = NTP_ERR_INVALID_SIGNATURE;
                return "Function requires an `npar` argument with signature => `integer, intent(in) :: npar`.";
            }

            std::string npar_name = match[4].str();
            
            // Match for params
            if(!std::regex_search(func.cbegin(), func.cend(), match, std::regex("(real|REAL)\\( *8 *\\) *, *(intent|INTENT)\\((in|IN)\\) *:: *\\w+\\( *" + npar_name + " *\\) *\\n")))
            {
                *error = NTP_ERR_INVALID_SIGNATURE;
                return "Function requires a `params` argument with signature => `real(8), intent(in) :: params(" + npar_name + ")`.";
            }

            // Match for return type
            if(!std::regex_search(func.cbegin(), func.cend(), match, std::regex("(real|REAL)\\(c_double\\) *:: *"+name)))
            {
                *error = NTP_ERR_INVALID_SIGNATURE;
                return "Function requires a return type => `real(c_double) :: <fname>`.";
            }

            *error = NTP_ERR_NOERR;
            return lc_name; // Actually we want to use the lowercase always name
        }

        *error = NTP_ERR_INVALID_SIGNATURE; 
        return "Function not found or Function does not contain the right signature => `function <fname>(real(8) x, integer(4) npar, real(8) params(npar)) bind(c, name='<fname>')`.";
    }
    else
    {
        *error = NTP_ERR_INVALID_LANG;
        return "Language `lang` not recognized. Must be either cpp or f90.";
    }
}

extern "C" NativeOutput ParseNativeFunc(const char* lang, const char* filename)
{
    NativeOutput output;

    output.error = NTP_ERR_NOERR;
    
    std::string fileData = OpenFile(filename, &output.error);
    if(output.error) return output;

    StripCommentsAndDefines(lang, fileData);

    std::string functionData = FindExportFunction(lang, fileData, &output.error);
    if(output.error) return output;

    std::string functionName = FindFunctionName(lang, functionData, &output.error);
    if(output.error)
    {
        std::cout << functionName << std::endl; // NOTE(César) : This is not a good design choice but is what we have for now
        return output;
    }

    std::vector<std::string> functionParams = FindFunctionParams(lang, functionData, &output.error);
    if(output.error) return output;

    output.function_name = (char*)malloc(sizeof(char) * 128);
    strcpy(output.function_name, functionName.c_str());

    output.parameter_names = (char*)malloc(sizeof(char) * 4096);
    output.parameter_names[0] = 0;
    output.num_params = static_cast<int>(functionParams.size());
    for(const auto& p : functionParams)
    {
        strcat(output.parameter_names, (p + " ").c_str());
    }

    return output;
}

extern "C" void FreeNativeOutput(NativeOutput* output)
{
    if(output != nullptr)
    {
        if(output->function_name)   free(output->function_name);
        if(output->parameter_names) free(output->parameter_names);
    }
}

extern "C" void GetNativeErrorMsg(NativeOutput* output, char* buffer)
{
    if(output->error)
    {
        strcpy(buffer, error_map.at(output->error).c_str());
    }
}

