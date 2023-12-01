
set(AUTO_CONF_PROJECT_NAME CACHE INTERNAL "" FORCE)
set(AUTO_CONF_PROJECT_VERSION CACHE INTERNAL "" FORCE)
set(AUTO_CONF_OPTIONS CACHE INTERNAL "" FORCE)
set(AUTO_MAKE_INLINE_CODE CACHE INTERNAL "" FORCE)

function(ac_set_project_name name)
    set(AUTO_CONF_PROJECT_NAME ${name} PARENT_SCOPE)
endfunction(ac_set_project_name)

function(ac_set_project_version version)
    set(AUTO_CONF_PROJECT_VERSION ${version} PARENT_SCOPE)
endfunction(ac_set_project_version)

function(ac_add_option name desc default)
    if(default)
        # The option is default enabled
        list(APPEND AUTO_CONF_OPTIONS "
        AC_ARG_ENABLE(
            [${name}],
            AS_HELP_STRING([--disable-${name}], [${desc}])
        )
        ")
    else()
        # The option is default disabled
        list(APPEND AUTO_CONF_OPTIONS "
        AC_ARG_ENABLE(
            [${name}],
            AS_HELP_STRING([--enable-${name}], [${desc}])
        )
        ") 
    endif()
    set(AUTO_CONF_OPTIONS ${AUTO_CONF_OPTIONS} PARENT_SCOPE)
    # Using CACHE INTERNAL instead
endfunction(ac_add_option)

function(am_inline_code code)
    list(APPEND AUTO_MAKE_INLINE_CODE ${code})
    set(AUTO_MAKE_INLINE_CODE ${AUTO_MAKE_INLINE_CODE} PARENT_SCOPE)
endfunction(am_inline_code)

# function()

