from .nested_res import *
from .nested_run import *

from importlib.metadata import version as imp_version
__version__ = imp_version("nested_fit")

sversion = f'# Current version: {__version__}'
vstring = sversion + ' ' * (58 - len(sversion)) + '#'
a=f"""
###########################################################
#                                                         #
#         WELCOME TO NESTED_FIT RESULT ANALYSIS           #
#                                                         #
{vstring}
#                                                         #
# Start with                                              #
# import nested_res as nr                                 #
# 'an=nr.Analysis()'                                      #
# By default the current path is considered.              #
#                                                         #
# If you want analyze another path:                       #
# 'an=nr.Analysis(path="path")'                           #
#                                                         #
# If you do not want to specify any path:                 #
# 'an=nr.Analysis(path=None)'                             #
#                                                         #
# To reload the data in the present directory:            #
# 'an.load_data()'                                        #
# otherwise specific the path                             #
# 'an.load_data(path="your path")'                        #
#                                                         #
# Uses the different functions                            #
# 'an.function()', (ex: 'an.plot()')                      #
#                                                         #
# To see the available function tape                      #
# 'an.' and then the 'tab' button                         #
#                                                         #
# To see the description of the different functions tape: #
# 'an.function?'                                          #
#                                                         #
# For any problem write to trassinelli@insp.jussieu.fr    #
###########################################################
"""
print(a)
