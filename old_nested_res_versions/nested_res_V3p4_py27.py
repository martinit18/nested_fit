#!/usr/bin/env python
import pandas as pd
import numpy as np
from numpy import log, histogram, zeros, savetxt, shape
import sys
import matplotlib.pyplot as plt
import os

current_version=3.4 # and beyond



a="""
###########################################################
#                                                         #
#         WELCOME TO NESTED_FIT RESULT ANALYSIS           #
#                                                         #
#                                                         #
# Start with                                              #
# 'an=nested_res_V3p4_py27.Analysis()'                    #
# By default the current path is considered.              #
#                                                         #
# If you want analyze another path:                       #
# 'an=nested_res_V3p4_py27.Analysis(path="path")'         #
#                                                         #
# If you do not want to specify any path:                 #
# 'an=nested_res.Analysis(path=None)'                     #
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

print 'Present version '+str(current_version) + '\n'


class Analysis(object):

    currentpath = os.getcwd()
    #print 'currentpath=', currentpath+'\n'
    initialize = False

    def __init__(self,path=currentpath,**kwargs):
        self.path = path
        self.load_data(path=path,**kwargs)


    def load_data(self, path=currentpath, do_load_input=True, do_load_output_results=False, do_load_output_data=True):
        # Adjust the path first
        self.path = path
        print 'Current path = ', path
        if path is None:
            print 'Please indicate a path if needed'
            return
        if do_load_input:
            self.load_input(path)
        if do_load_output_results:
            self.load_output_results(path)
        if do_load_output_data:
            self.load_output_data(path)

    def load_input(self, path):
        self.path = path
        self.input_data, self.input_comment = self.read_input(path=path)
        return self.input_data

    def load_output_results(self, path):
        self.path = path
        self.load_input(path)

        self.parameters = [p[1].replace("'", "") for p in self.input_data['parameters']]
        self.output_results = self.read_output(path=path)

        return self.output_results


    def load_output_data(self, path=currentpath):
        # Adjust the path first
        if path[-1]<>'/' and path <> None:  path = path+'/'
        #
        self.path = path
        self.number_of_values = self.input_data['npar']
        # Check first if is there
        if not os.path.isfile(path+'nf_output_points.txt'):
            print 'Result file nf_output_points.txt not present\n Nothing to load'
            return None
        self.df = pd.read_csv(path+'nf_output_points.txt', delim_whitespace=True, header=0,
                names=["weight","lnlikelihood"] + ["val_%s" % d for d in range(1, self.number_of_values+1)])

        self.df = self.df.rename(columns={'val_%d' % p[0] : p[1].replace("'", "") for p in self.input_data['parameters']})

#        for c in [p[1].replace("'", "") for p in self.input_data['parameters']]:
#            if c in self.df.columns and c != "weight":
#                self.df["w" + c] = self.df["weight"] * self.df[c]

        print 'Available parameters :', list(self.df.columns)

        self.df.head()
        self.data = self.df.values



########################################################################################
    def read_input(self,path=currentpath):
        '''
        Read the parameter file nf_input.dat
        Output are two dictionaries with values and comments:
        input_data, input_comment
        '''
        # Adjust the path first
        if path[-1]<>'/' and path <> None:  path = path+'/'
        #
        # Parametersof the dictionaries: values and comments
        # Initialization
        input_data = {}
        input_comment = {}
        # Adjust path variable if needed
        # Read the file and the different parameters
        input_file = open(path+'nf_input.dat')
        lines = input_file.readlines()
        input_file.close()
        # The parameters are read with their comment and/or definition
        # Program version
        input_data['version']= float(lines[0].split()[0])
        input_comment['version'] = lines[0][lines[0].find('#'):]
        if input_data['version'] < int(current_version):
            sys.exit('Wrong input file version. Please check.')
        # Filename with the data to fit
        input_data['filename'] = lines[1].split()[0]
        input_comment['filename'] = lines[1][lines[1].find('#'):]
        # Errorbars present or not
        input_data['errorbars_yn'] = lines[2].split()[0]
        input_comment['errorbars_yn'] = lines[2][lines[2].find('#'):]
        # Set of files or not
        input_data['set_yn'] = lines[3].split()[0]
        input_comment['set_yn'] = lines[3][lines[3].find('#'):]
        # Number of live points
        input_data['nlive'] = int(lines[4].split()[0])
        input_comment['nlive'] = lines[4][lines[4].find('#'):]
        # Evidence final accuracy
        input_data['evaccuracy'] = float(lines[5].split()[0])
        input_comment['evaccuracy'] = lines[5][lines[5].find('#'):]
        # Fraction of parameter standard deviation and n of jumps, max iterations and groups of iterations
        input_data['sdfraction'] = float(lines[6].split()[0])
        input_data['njump'] = int(lines[6].split()[1])
        input_data['maxtries'] = int(lines[6].split()[2])
        input_data['maxntries'] = int(lines[6].split()[3])
        input_comment['search_algotithm'] = lines[6][lines[6].find('#'):]
        # Cluster analysis: if it is active and its parameters
        input_data['cluster_yn'] = lines[7].split()[0]
        input_data['cluster_method'] = lines[7].split()[1]
        input_data['distance_limit'] = float(lines[7].split()[2])
        input_data['bandwidth'] = float(lines[7].split()[3])
        input_comment['cluster_algotithm'] = lines[7][lines[7].find('#'):]
        # Number of tries and maximuma number of steps per tries
        input_data['ntry'] = int(lines[8].split()[0])
        input_data['maxstep_try'] = int(lines[8].split()[1])
        input_comment['ntry_maxstep'] = lines[8][lines[8].find('#'):]
        # Name of the function choosen for the fit
        input_data['function_name'] = lines[9].split()[0]
        input_comment['function_name'] = lines[9][lines[9].find('#'):]
        # Left-Right asymmetry parameter
        input_data['lr'] = lines[10][:1].split()[0]
        input_comment['lr'] = lines[10][lines[10].find('#'):]
        # Additional data for convolution
        input_data['npoint'] = int(int(lines[11].split()[0]))
        input_data['nwidth'] = int(int(lines[11].split()[1]))
        input_comment['additional data'] = lines[11][lines[11].find('#'):]
        # Limit of the spectra used for the fit
        input_data['xmin'] = float(lines[12].split()[0])
        input_data['xmax'] = float(lines[12].split()[1])
        input_comment['xmin_xmax'] = lines[12][lines[12].find('#'):]
        # Number of parameter used for the fit
        npar = int(lines[13].split()[0])
        input_data['npar'] = npar
        input_comment['npar'] = lines[13][lines[13].find('#'):]
        # Parameter of the fit, names, limits and "fix" flag
        input_comment['parameters'] = lines[14]
        parameters = []
        for index in range(npar):
            parameters.append([int(lines[15+index].split()[0]),
                lines[15+index].split()[1],
                float(lines[15+index].split()[2]),
                float(lines[15+index].split()[3]),
                float(lines[15+index].split()[4]),
                float(lines[15+index].split()[5]),
                int(lines[15+index].split()[6])])
        input_data['parameters'] = parameters


        return input_data, input_comment

########################################################################################
    def read_output(self,path=currentpath):
        '''
        Read the parameter file nf_output_res.dat
        Output are two dictionaries with values and comments:
        output_data, output_comment
        '''
        from numpy import array

        # Adjust the path first
        if path[-1]<>'/' and path <> None:  path = path+'/'
        #

        # Read parameters from input file
        input_data, input_comment = self.read_input(path=path)
        npar = input_data['npar']

        # Initialization
        output_data = {}

        # Read the file and the different parameters
        # Check first if is there
        if not os.path.isfile(path + 'nf_output_res.dat'):
            print 'Result file nf_output_res.dat not present\n Nothing to load'
            return None
        output_file = open(path + 'nf_output_res.dat')
        lines = output_file.readlines()
        output_file.close()

        # Read number of tries
        output_data['ntry'] = int(lines[1].split()[1])
        if input_data['ntry'] <> output_data['ntry'] :
            print 'input ntry', input_data['ntry'], 'output ntry', output_data['ntry']
            sys.exit('Check your input/output files')

        # Number of iteration
        output_data['niter'] = int(lines[2].split()[1])

        # Final evidence
        output_data['evidence'] = float(lines[3].split()[1])

        # Evidence dispersion
        output_data['evidence_sd'] = float(lines[4].split()[1])

        # Max likelihood
        output_data['like_max'] = float(lines[6].split()[1])

        # Max parameter set
        output_data['max'] = [float(lines[8+index].split()[1]) for index in range(npar)]

        # Average and standard deviation of parameters
        #output_data['mean']= [[float(lines[10+npar+index].split()[1]),float(lines[10+npar+index].split()[3])] for index in range(npar)]
        output_data['mean'] = [float(lines[10+npar+index].split()[1]) for index in range(npar)]
        output_data['sd']   = [float(lines[10+npar+index].split()[3]) for index in range(npar)]

        # Confidence levels of parameters
        #output_data['conf_level'] = [[float(lines[12+2*npar+index].split()[1]),float(lines[12+2*npar+index].split()[2]),float(lines[12+2*npar+index].split()[3]),float(lines[12+2*npar+index].split()[5]),float(lines[12+2*npar+index].split()[6]),float(lines[12+2*npar+index].split()[7])] for index in range(npar)]
        output_data['conf_level_m99'] = [float(lines[12+2*npar+index].split()[1]) for index in range(npar)]
        output_data['conf_level_m95'] = [float(lines[12+2*npar+index].split()[2]) for index in range(npar)]
        output_data['conf_level_m68'] = [float(lines[12+2*npar+index].split()[3]) for index in range(npar)]
        output_data['conf_level_p68'] = [float(lines[12+2*npar+index].split()[5]) for index in range(npar)]
        output_data['conf_level_p95'] = [float(lines[12+2*npar+index].split()[6]) for index in range(npar)]
        output_data['conf_level_p99'] = [float(lines[12+2*npar+index].split()[7]) for index in range(npar)]

        # Median of parameters
        output_data['median'] = [float(lines[12+2*npar+index].split()[4]) for index in range(npar)]

        # Information
        output_data['information'] =  float(lines[14+3*npar].split()[1])

        # Complexity
        output_data['complexity'] =  float(lines[16+3*npar].split()[1])

        # Calculation information
        output_data['n_cores'] =  int(lines[18+3*npar].split()[1])
        output_data['cpu_computation_time'] =  float(lines[19+3*npar].split()[1])
        output_data['real_computation_time'] =  float(lines[19+3*npar].split()[2])

        return output_data



####################################################################################################################################
    def plot(self,path=currentpath,xmin=0,xmax=0,ymin=0,ymax=0,typeof='max',
                 logscale=False,nset=0,high_definition=False):
        '''
        Plot the fit results present in the file output_data.dat and eventually to the file output_fit.dat.
        The limit of the plot can be indicated. If not, the maximum and the minimum of the histogram are taken into account.
        If there is a set of profiles to fit, specify which one has to be visualized
        '''

        linestyle = {"markeredgewidth":2, "elinewidth":2, "capsize":4,"markersize":3}
        linestyle2 = {"markeredgewidth":0, "elinewidth":2, "capsize":0,"markersize":0}

        # Adjust the path first
        if path[-1]<>'/' and path <> None:  path = path+'/'
        #

        self.path = path

        from numpy import loadtxt, size
        import matplotlib.pyplot as plt


        print nset, typeof




        # Read the output file(s)
        if nset < 1:
            print 'nf_output_data_'+ typeof + '.dat'
            data = loadtxt(self.path+'nf_output_data_'+ typeof + '.dat')
        else:
            print 'nf_output_data_'+ typeof + '_' + str(nset) +'.dat'
            data = loadtxt(self.path+'nf_output_data_'+ typeof + '_' + str(nset) +'.dat')

        # Assign variables
        x  = data[:,0]
        y  = data[:,1]
        yf = data[:,2]
        r  = data[:,3]
        sy = data[:,4]

        if high_definition:
            # Data from fit results with a higher density
            if nset < 1:
                data_fit = loadtxt(self.path+'nf_output_fit_'+ typeof + '.dat')
            else:
                data_fit = loadtxt(self.path+'nf_output_fit_'+ typeof + '_' + str(nset) +'.dat')

            x_fit = data_fit[:,0]
            y_fit = data_fit[:,1]
        else:
            x_fit = x
            y_fit = yf

        minx = x.min()
        maxx = x.max()
        miny = y.min()
        maxy = y.max()*1.3


        # Plot the residual
        plt.figure()
        plt.clf()
        linestyle = {"markeredgewidth":2, "elinewidth":2, "capsize":4,"markersize":3}
        linestyle2 = {"markeredgewidth":0, "elinewidth":2, "capsize":0,"markersize":0}
        plt.title('Residual')
        plt.xlabel('Channel')
        plt.ylabel('Counts')
        #plt.tight_layout()
        # Border of the graph
        if xmin==0 and xmax==0:
            plt.xlim([minx,maxx])
        else:
            plt.xlim([xmin,xmax])
        plt.errorbar(x,r,yerr=sy,xerr=None,fmt='ob',ecolor='blue',mec='blue',**linestyle)
        plt.errorbar([minx,maxx],[0.,0.],yerr=None,xerr=None,fmt='-k',**linestyle2)
        #plt.axhline(linewidth=2,color='k')

        # Plot the results
        plt.figure()
        plt.clf()
        plt.title('Fit result')
        plt.xlabel('Channel')
        plt.ylabel('Counts')
        # Border of the graph
        if xmin==0 and xmax==0:
            plt.xlim([minx,maxx])
        else:
            plt.xlim([xmin,xmax])
        if ymin==0 and ymax==0:
            plt.ylim(miny,maxy)
        else:
            plt.ylim(ymin,ymax)
        if logscale: plt.yscale('log')
        plt.errorbar(x,y,yerr=sy,xerr=None,fmt='or',ecolor='red',mec='red',**linestyle)
        plt.errorbar(x_fit,y_fit,yerr=None,xerr=None,fmt='-b',**linestyle2)
        #plt.plot(x_fit,y_fit,'-b',label='Fit')


        plt.show()

####################################################################################################################################
    def plot_with_inset(self,path=currentpath,xmin=0,xmax=0,ymin=0,ymax=0,typeof='max',
                 logscale=False,nset=0,high_definition=False,xinset_min=0.2,xinset_width=0.4,yinset_min=0.2,yinset_width=0.3):
        '''
        Plot the fit results present in the file output_data.dat and eventually to the file output_fit.dat.
        The limit of the plot can be indicated. If not, the maximum and the minimum of the histogram are taken into account.
        If there is a set of profiles to fit, specify which one has to be visualized
        '''

        linestyle = {"markeredgewidth":2, "elinewidth":2, "capsize":4,"markersize":3}
        linestyle2 = {"markeredgewidth":0, "elinewidth":2, "capsize":0,"markersize":0}

        # Adjust the path first
        if path[-1]<>'/' and path <> None:  path = path+'/'
        #

        self.path = path

        from numpy import loadtxt, size
        import matplotlib.pyplot as plt


        print nset, typeof


        # Read the output file(s)
        if nset < 1:
            print 'nf_output_data_'+ typeof + '.dat'
            data = loadtxt(self.path+'nf_output_data_'+ typeof + '.dat')
        else:
            print 'nf_output_data_'+ typeof + '_' + str(nset) +'.dat'
            data = loadtxt(self.path+'nf_output_data_'+ typeof + '_' + str(nset) +'.dat')

        # Assign variables
        x  = data[:,0]
        y  = data[:,1]
        yf = data[:,2]
        r  = data[:,3]
        sy = data[:,4]

        if high_definition:
            # Data from fit results with a higher density
            if nset < 1:
                data_fit = loadtxt(self.path+'nf_output_fit_'+ typeof + '.dat')
            else:
                data_fit = loadtxt(self.path+'nf_output_fit_'+ typeof + '_' + str(nset) +'.dat')

            x_fit = data_fit[:,0]
            y_fit = data_fit[:,1]
        else:
            x_fit = x
            y_fit = yf

        minx = x.min()
        maxx = x.max()
        miny = y.min()
        maxy = y.max()*1.3


        # Plot the results
        plt.figure()
        plt.clf()
        plt.title('Fit result')
        plt.xlabel('Channel')
        plt.ylabel('Counts')
        # Border of the graph
        if xmin==0 and xmax==0:
            plt.xlim([minx,maxx])
        else:
            plt.xlim([xmin,xmax])
        if ymin==0 and ymax==0:
            plt.ylim(miny,maxy)
        else:
            plt.ylim(ymin,ymax)
        if logscale: plt.yscale('log')
        plt.errorbar(x,y,yerr=sy,xerr=None,fmt='or',ecolor='red',mec='red',**linestyle)
        plt.errorbar(x_fit,y_fit,yerr=None,xerr=None,fmt='-b',**linestyle2)
        #plt.plot(x_fit,y_fit,'-b',label='Fit')


       # Plot the results in an inset
        # Inset
        inset=plt.axes([xinset_min,yinset_min,xinset_width,yinset_width])
        #plt.xlabel('Channel')
        #plt.ylabel('Counts')
        # Border of the graph
        if xmin==0 and xmax==0:
            plt.xlim([minx,maxx])
        else:
            plt.xlim([xmin,xmax])
        #if ymin==0 and ymax==0:
        #    plt.ylim(miny,maxy)
        #else:
        #    plt.ylim(ymin,ymax)
        #if logscale: plt.yscale('log')
        plt.errorbar(x,r,yerr=sy,xerr=None,fmt='ob',ecolor='blue',mec='blue',**linestyle)
        plt.errorbar([minx,maxx],[0.,0.],yerr=None,xerr=None,fmt='-k',**linestyle2)
        #plt.plot(x_fit,y_fit,'-b',label='Fit')


        plt.show()

#################################################################################################################
    def histo(self,par_number,path=currentpath,bins=200,plotmode='sigma',xmin=None,xmax=None,savedata=False,alpha=1.,clear=True):

        '''Plot histogram relative to one parameter and calculate the different confidence levels.
        If plotmode = 'sigmalog', plot in logarithmic mode.
        If plotmode='lin' or 'log', simple histogram (linear or logarithmic) without confidence levels. '''

        self.path = path

        # Initialize


        # Find the good number of parameter from its number or name
        if type(par_number) == str:
            par_index = list(self.df.columns).index(par_number)
            title = par_number
            print "Set par_number %s to %s" % (par_number, par_index)
        else:
            par_index = par_number + 1
            print "Set par_number %s to %s" % (par_number, par_index)
            title = self.input_data['parameters'][par_number-1][1]

        if bins<10:
            sys.exit('Attention! too fews bins')

        # Read the data
        data = self.data

        # Select range
        if xmin == None: xmin = data[:,par_index].min()
        if xmax == None: xmax = data[:,par_index].max()
        data = data[(data[:,par_index] < xmax) & (data[:,par_index] > xmin)]


        if plotmode == 'lin' :
            plt.hist(data[:,par_index],bins=bins,weights=data[:,2])
        elif plotmode == 'log':
            plt.hist(log(data[:,par_index])+50.,bins=bins,weights=data[:,2])
            print 'not working I do not why'
        else:
            # Make histogram with differnet colors for different confidence levels
            histo, edges = histogram(data[:,par_index],bins=bins,weights=data[:,0])\

            # Make array with histogram values, position, and order
            pos = zeros((bins))
            ipos = range(bins)
            histo_sum = histo.sum()
            for i in ipos: pos[i] = (edges[i]+edges[i+1])/2
            width = edges[1]-edges[0]
            histo_data = zeros((bins,2))
            histo_data[:,0] = histo/histo_sum
            histo_data[:,1] = ipos
            histo_raw = histo_data[:,0]

            # Make confidence level histogram
            histo_data68 = zeros((bins))
            histo_data95 = zeros((bins))
            histo_data99 = zeros((bins))
            histo_data_rest = zeros((bins))

            # Reorder histogram as function of their value
            histo_data=histo_data[histo_data[:,0].argsort()]
            # Scan and fill the confidence level histogram
            histo_part = 0.
            for raw in histo_data[histo_data[:,0]>0]:
                histo_part = histo_part + raw[0]
                if histo_part <= 0.01:
                    histo_data_rest[int(raw[1])] = raw[0]
                elif 0.01 <= histo_part <= 0.05:
                    histo_data99[int(raw[1])] = raw[0]
                elif 0.05 <= histo_part <= 0.32:
                    histo_data95[int(raw[1])] = raw[0]
                elif 0.32 <= histo_part:
                    histo_data68[int(raw[1])] = raw[0]

            # Histogram of one parameter
            if clear: plt.clf()
            plt.xlabel('Value of parameter '+ title)
            # Plot everything
            if plotmode == 'sigma':
                plt.ylabel('Probability')
                plt.bar(edges[:-1],histo_data_rest,width=width,color='blue',edgecolor="blue", linewidth=0.0,alpha=alpha)
                plt.bar(edges[:-1],histo_data99,width=width,color='limegreen',edgecolor="limegreen", linewidth=0.0,alpha=alpha)
                plt.bar(edges[:-1],histo_data95,width=width,color='yellow',edgecolor="yellow", linewidth=0.0,alpha=alpha)
                plt.bar(edges[:-1],histo_data68,width=width,color='red',edgecolor="red", linewidth=0.0,alpha=alpha)
            elif plotmode == 'sigmalog':
                # Logarithimic scale
                plt.ylabel('Log(probability) + 50')
                plt.bar(edges[:-1],log(histo_data_rest)+50.,width=width,color='blue',edgecolor="blue", linewidth=0.0)
                plt.bar(edges[:-1],log(histo_data99)+50.,width=width,color='limegreen',edgecolor="limegreen", linewidth=0.0)
                plt.bar(edges[:-1],log(histo_data95)+50.,width=width,color='yellow',edgecolor="yellow", linewidth=0.0)
                plt.bar(edges[:-1],log(histo_data68)+50.,width=width,color='red',edgecolor="red", linewidth=0.0)

            #plt.tight_layout()

        if savedata:
            data = zeros(shape(histo_data))
            data[:,0] = pos
            data[:,1] = histo_raw
            savetxt(self.path + 'histo.dat',data)

        # TO DO INTERPOLATION WITH scipy.interpolate.UnivariateSpline

############################################################################################
    def plot_par(self,par_number,path=currentpath):
        '''Plot parameter evolution during the nested sampling as
        function of the corresponding value of likelihood (order, xaxis)
        and weight (color)'''
        from numpy import loadtxt, max, log, arange
        import matplotlib.pyplot as plt

        self.path = path


        # Determine if the par_number is a name or a number and convert consequently
        if type(par_number) == str:
            par_index = list(self.df.columns).index(par_number)
            title = par_number
            print "Set par_number %s to %s" % (par_number, par_index)
        else:
            par_index = par_number + 1
            print "Set par_number %s to %s" % (par_number, par_index)
            title = self.input_data['parameters'][par_number-1][1]
            print "Selected parameter = ", title

        # Read the data
        data = self.df.values
        ix = arange(shape(data)[0])

        # Plot of one parameter
        plt.clf()
        plt.xlim(0,shape(data)[0]*1.1)
        plt.xlabel('Sort number')
        plt.ylabel('Value of parameter ' + title)
        cmap=plt.cm.get_cmap('jet')
        plt.scatter(ix,data[:,par_index],c=data[:,0],linewidth=0.,cmap=cmap)
        cbar = plt.colorbar()
        cbar.set_label('Weight')
        #plt.tight_layout()

############################################################################################
    def plot_clusters(self,par_number1,par_number2,label=None,xmin=None,xmax=None,ymin=None,ymax=None,path=currentpath):
        '''Plot the clusters if cluster analysis is on.
        'label' is the timestamp of the file to analyse'''
        from numpy import loadtxt, max, log, shape
        import glob

        self.path = path

        # Find the good number of parameter from its number or name
        if type(par_number1) == str:
            par_index1 = list(self.df.columns).index(par_number1) - 1
            title1 = par_number1
            print "Set par_number %s to %s" % (par_number1, par_index1)
        else:
            par_index1 = par_number1
            print "Set par_number %s to %s" % (par_number1, par_index1)
            title1 = self.input_data['parameters'][par_number1-1][1]

        if type(par_number2) == str:
            par_index2 = list(self.df.columns).index(par_number2) - 1
            title2 = par_number2
            print "Set par_number %s to %s" % (par_number2, par_index2)
        else:
            par_index2 = par_number2
            print "Set par_number %s to %s" % (par_number2, par_index2)
            title2 = self.input_data['parameters'][par_number2-1][1]


        # Read the data
        if label == None:
            available_files = sorted(glob.glob('nf_meanshift_final_*.dat'))
            print 'Taking the last available file'
            label = available_files[-1][19:-4]


        filename = 'nf_meanshift_final_' + label + '.dat'
        print('Showing file ', filename)
        data = loadtxt(filename)

        ncl = int(max(data[:,0]))



        print 'Number of clusters:', ncl

        # Select the data
        if xmin == None: xmin = data[:,par_index1].min()
        if xmax == None: xmax = data[:,par_index1].max()
        if ymin == None: ymin = data[:,par_index2].min()
        if ymax == None: ymax = data[:,par_index2].max()
        data = data[(data[:,par_index1] < xmax) & (data[:,par_index1] > xmin) & (data[:,par_index2] < ymax) & (data[:,par_index2] > ymin)]

        # Plot of one parameter
        plt.clf()
        #plt.xlim(0,max(data[:,0])*1.1)
        plt.xlim(xmin,xmax)
        plt.ylim(ymin,ymax)
        plt.xlabel(title1)
        plt.ylabel(title2)
        #plt.ylabel('Value of parameter ' + title)
        #cmap=plt.cm.get_cmap('jet')
        for i in range(ncl): plt.plot(data[data[:,0]==i][:,par_index1],data[data[:,0]==i][:,par_index2],'o')
        #plt.tight_layout()


############################################################################################
    def plot_clusters3D(self,par_number1,par_number2,par_number3,
                            xmin=None,xmax=None,ymin=None,ymax=None,zmin=None,zmax=None,azm=None,ele=None,
                            figsize=None,label='',filename=None,path=currentpath):
        '''Plot the clusters if cluster analysis is on in 3D.
        'label' is the timestamp of the file to analyse'''
        from mpl_toolkits.mplot3d import Axes3D
        from numpy import loadtxt, max, log, shape
        import matplotlib.pyplot as plt

        self.path = path

        # Find the good number of parameter from its number or name
        if type(par_number1) == str:
            par_index1 = list(self.df.columns).index(par_number1) - 2
            title1 = par_number1
            print "Set par_number %s to %s" % (par_number1, par_index1)
        else:
            par_index1 = par_number1
            print "Set par_number %s to %s" % (par_number1, par_index1)
            title1 = self.input_data['parameters'][par_number1-1][1]

        if type(par_number2) == str:
            par_index2 = list(self.df.columns).index(par_number2) - 2
            title2 = par_number2
            print "Set par_number %s to %s" % (par_number2, par_index2)
        else:
            par_index2 = par_number2
            print "Set par_number %s to %s" % (par_number2, par_index2)
            title2 = self.input_data['parameters'][par_number2-1][1]

        if type(par_number3) == str:
            par_index3 = list(self.df.columns).index(par_number3) - 2
            title3 = par_number3
            print "Set par_number %s to %s" % (par_number3, par_index3)
        else:
            par_index3 = par_number3
            print "Set par_number %s to %s" % (par_number3, par_index3)
            title3 = self.input_data['parameters'][par_number3-1][1]


        # Read the data
        if filename == None:
            filename = 'nf_meanshift_final_' + label + '.dat'
        data = loadtxt(filename)

        ncl = int(max(data[:,0]))



        print 'Number of clusters:', ncl

        # Select the data
        if xmin == None: xmin = data[:,par_index1].min()
        if xmax == None: xmax = data[:,par_index1].max()
        if ymin == None: ymin = data[:,par_index2].min()
        if ymax == None: ymax = data[:,par_index2].max()
        if zmin == None: zmin = data[:,par_index3].min()
        if zmax == None: zmax = data[:,par_index3].max()
        data = data[(data[:,par_index1] < xmax) & (data[:,par_index1] > xmin) & (data[:,par_index2] < ymax) & (data[:,par_index2] > ymin) & (data[:,par_index3] < zmax) & (data[:,par_index3] > zmin)]


        fig = plt.figure(figsize=figsize)
        ax = fig.add_subplot(111, projection='3d')

        ax.view_init(elev=ele, azim=azm)
        ax.set_xlim(xmin,xmax)
        ax.set_ylim(ymin,ymax)
        ax.set_zlim(zmin,zmax)
        ax.set_xlabel(title1)
        ax.set_ylabel(title2)
        ax.set_zlabel(title3)
        for i in range(ncl):
            ax.scatter(data[data[:,0]==i][:,par_index1],data[data[:,0]==i][:,par_index2],data[data[:,0]==i][:,par_index3])
        #ax.scatter(data[:,3],data[:,4],data[:,5])
        #plt.tight_layout()

############################################################################################
    def plot_live(self,par_number1,par_number2,path=currentpath,typeof='final'):
        '''Plot the live points (final, intermediate or initial)'''
        from numpy import loadtxt, max, log, shape
        import matplotlib.pyplot as plt

        self.path = path

        # Find the good number of parameter from its number or name
        if type(par_number1) == str:
            par_index1 = list(self.df.columns).index(par_number1)
            title1 = par_number1
            print "Set par_number %s to %s" % (par_number1, par_index1)
        else:
            par_index1 = par_number1 + 1
            print "Set par_number %s to %s" % (par_number1, par_index1)
            title1 = self.input_data['parameters'][par_number1-1][1]

        if type(par_number2) == str:
            par_index2 = list(self.df.columns).index(par_number2)
            title2 = par_number2
            print "Set par_number %s to %s" % (par_number2, par_index2)
        else:
            par_index2 = par_number2 + 1
            print "Set par_number %s to %s" % (par_number2, par_index2)
            title2 = self.input_data['parameters'][par_number2-1][1]


        # Read the data
        if typeof=='final':
            data = loadtxt('nf_output_last_live_points.dat')
        elif typeof=='initial':
            data = loadtxt('nf_initial_live_points.dat')
        elif typeof=='intermediate':
            data = loadtxt('nf_intermediate_live_points.dat')
        else:
            print('Choose between final, initial or intermediate')

        # Plot of one parameter
        plt.clf()
        #plt.xlim(0,max(data[:,0])*1.1)
        plt.xlabel(title1)
        plt.ylabel(title2)
        #plt.ylabel('Value of parameter ' + title)
        #cmap=plt.cm.get_cmap('jet')
        plt.plot(data[:,par_index1],data[:,par_index2],'ob')
        #plt.tight_layout()

#####################################################################################################################3
    def histo2D(self,par_number1,par_number2,path=currentpath,bins=100,xmin=None,xmax=None,ymin=None,ymax=None,plotmode='sigma',cmap='normal',interp=False,grid_steps=4,s=1E-5):
        '''Plot histogram relative to two parameter values.
        If plotmode='normal' or 'log', colors are proportional to the probability (or log(p)).
        In this case, different cmaps cand be choosen:
        - cmap='normal'       proportional to the real probability
        - cmap='accurate'     (more color variation)
        - cmap='moreaccurate' (series of color variations).

        If plotmode='sigma', colors are related to the confidence levels:
        - reds to 68% confidence interval
        - yellow-organges to 68%-95% confidence interval differenece
        - greens to  95%-99% confidence interval differenece
        - blues for the rest

        If 'interp=True', interpolation is overimposed.
        Play with 'grid_steps' (intermediate steps between bins) and smooth factor 's'
        Default value is '4'.
        '''

        self.path = path


        from numpy import loadtxt, log, histogram2d, zeros
        import matplotlib.pyplot as plt
        import matplotlib.colors as mcolors

        # Find the good number of parameter from its number or name
        if type(par_number1) == str:
            par_index1 = list(self.df.columns).index(par_number1)
            title1 = par_number1
            print "Set par_number %s to %s" % (par_number1, par_index1)
        else:
            par_index1 = par_number1 + 1
            print "Set par_number %s to %s" % (par_number1, par_index1)
            title1 = self.input_data['parameters'][par_number1-1][1]

        if type(par_number2) == str:
            par_index2 = list(self.df.columns).index(par_number2)
            title2 = par_number2
            print "Set par_number %s to %s" % (par_number2, par_index2)
        else:
            par_index2 = par_number2 + 1
            print "Set par_number %s to %s" % (par_number2, par_index2)
            title2 = self.input_data['parameters'][par_number2-1][1]

        # Read the data
        data = self.df.values

        # Select the data
        if xmin == None: xmin = data[:,par_index1].min()
        if xmax == None: xmax = data[:,par_index1].max()
        if ymin == None: ymin = data[:,par_index2].min()
        if ymax == None: ymax = data[:,par_index2].max()
        data = data[(data[:,par_index1] < xmax) & (data[:,par_index1] > xmin) & (data[:,par_index2] < ymax) & (data[:,par_index2] > ymin)]

        # Make histogram
        histo2D, yedges, xedges = histogram2d(data[:,par_index2],data[:,par_index1],bins=bins,weights=data[:,0])
        extent = [ xedges[0], xedges[-1],yedges[0], yedges[-1]]

        '''
        if save_data:
            posx = zeros(size(yedges)-1)
            posy = zeros(size(yedges)-1)
            for i in size(xedges)-1: posx[i] = (xedges[i]+xedges[i+1])/2
            for i in size(yedges)-1: posy[i] = (yedges[i]+yedges[i+1])/2
            datasave = empty
            ... to be continued ....

        '''



        # Choose the cmap for likelihood plots
        if cmap == 'normal':
            cmap=plt.cm.get_cmap('jet')
        elif cmap == 'accurate':
            cmap=plt.cm.get_cmap('gist_ncar')
        elif cmap == 'moreaccurate':
            cmap=plt.cm.get_cmap('prism')

        # Plot histogram
        plt.clf()
        plt.xlabel('Value of parameter ' + title1)
        plt.ylabel('Value of parameter ' + title2)
        #
        if plotmode == 'normal':
            # Plot the 2D histogram
            plt.imshow(histo2D[::-1,],interpolation='nearest',extent=extent,aspect='auto',cmap=cmap)
            cbar = plt.colorbar()
            cbar.set_label('Probability')
        elif plotmode == 'log':
            # Plot in logarithmic scale
            plt.imshow(log(histo2D[::-1,]),interpolation='nearest',extent=extent,aspect='auto',cmap=cmap)
            cbar = plt.colorbar()
            cbar.set_label('Log(Probability)')
        elif plotmode == 'sigma':
            # Make confidence level histogram
            histo2D_data68 = zeros((bins,bins))
            histo2D_data95 = zeros((bins,bins))
            histo2D_data99 = zeros((bins,bins))
            histo2D_data_rest = zeros((bins,bins))
            # Make array with histogram values, position, and order
            histo2D_sum = histo2D.sum()
            histo2D_data = zeros((bins*bins,3))
            ij = 0
            for i in range(bins):
                for j in range(bins):
                    histo2D_data[ij,0] = i
                    histo2D_data[ij,1] = j
                    histo2D_data[ij,2] = histo2D[i,j]
                    ij+=1
            histo2D_sum = histo2D_data[:,2].sum()
            histo2D_data[:,2] = histo2D_data[:,2]/histo2D_sum
            # Resort data in order of probability
            histo2D_data=histo2D_data[histo2D_data[:,2].argsort()]
            # Scan and fill the confidence level histograms
            histo_part = 0.
            for raw in histo2D_data[histo2D_data[:,2]>0]:
                histo_part = histo_part + raw[2]
                if histo_part <= 0.01:
                    histo2D_data_rest[int(raw[0]),int(raw[1])] = raw[2]
                elif 0.01 <= histo_part <= 0.05:
                    histo2D_data99[int(raw[0]),int(raw[1])] = raw[2]
                elif 0.05 <= histo_part <= 0.32:
                    histo2D_data95[int(raw[0]),int(raw[1])] = raw[2]
                elif 0.32 <= histo_part:
                    histo2D_data68[int(raw[0]),int(raw[1])] = raw[2]
            # And then finally plot!!!
            # Stuff to define colors
            c = mcolors.ColorConverter().to_rgb
            rvb68 = make_colormap([c('red'),c('darkred')])
            rvb95 = make_colormap([c('orange'),c('yellow')])
            rvb99 = make_colormap([c('limegreen'),c('green')])
            rvb_rest = make_colormap([c('lightblue'),c('blue')])
            #
            plt.imshow(log(histo2D_data68[::-1,]),interpolation='nearest',extent=extent,aspect='auto',cmap=rvb68)
            plt.imshow(log(histo2D_data95[::-1,]),interpolation='nearest',extent=extent,aspect='auto',cmap=rvb95)
            plt.imshow(log(histo2D_data99[::-1,]),interpolation='nearest',extent=extent,aspect='auto',cmap=rvb99)
            plt.imshow(log(histo2D_data_rest[::-1,]),interpolation='nearest',extent=extent,aspect='auto',cmap=rvb_rest)

        # Plot the contourns
        if interp: self.interp2D(par_number1,par_number2,bins=bins,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,grid_steps=grid_steps,s=s,cmap='k',levels=True,clear=False)

        #plt.tight_layout()

########################################################################################################
    def plot_like(self,path=currentpath):
        ''' Plot the likelihood of the nested sampling points '''
        from numpy import arange, shape
        import matplotlib.pyplot as plt

        self.path = path


        # Read the data
        data = self.df.values
        ix = arange(shape(data)[0])

        # Plot
        plt.clf()
        plt.xlabel('Sort number')
        plt.ylabel('Log(likelihood)')
        plt.plot(ix,data[:,1])
        #plt.tight_layout()

########################################################################################################
    def plot_weights(self,path=currentpath):
        ''' Plot the likelihood of the nested sampling points '''
        from numpy import arange, shape
        import matplotlib.pyplot as plt

        # Read the data
        data = self.df.values
        ix = arange(shape(data)[0])

        self.path = path


        # Plot
        plt.clf()
        plt.xlabel('Sort number')
        plt.ylabel('Nested sampling weights')
        plt.plot(ix,data[:,0])
        #plt.tight_layout()

######################################################################################################## WORKING !!

    def interp2D(self,par_number1,par_number2,path=currentpath,bins=100,xmin=None,xmax=None,ymin=None,ymax=None,grid_steps=4,s=1E-5,cmap='normal',levels=False,clear=True):
        '''
        grid_steps: number of intermediate steps between bins
        s: smooth factor to play with
        '''
        #from histogram import *
        from numpy import loadtxt, log, histogram2d, zeros, linspace, meshgrid, arange, size
        import matplotlib.pyplot as plt
        import matplotlib.colors as mcolors
        import matplotlib.mlab as mlab
        from scipy.interpolate import RectBivariateSpline

        self.path = path


        # Find the good number of parameter from its number or name
        if type(par_number1) == str:
            par_index1 = list(self.df.columns).index(par_number1)
            title1 = par_number1
            print "Set par_number %s to %s" % (par_number1, par_index1)
        else:
            par_index1 = par_number1 + 1
            print "Set par_number %s to %s" % (par_number1, par_index1)
            title1 = self.input_data['parameters'][par_number1-1][1]

        if type(par_number2) == str:
            par_index2 = list(self.df.columns).index(par_number2)
            title2 = par_number2
            print "Set par_number %s to %s" % (par_number2, par_index2)
        else:
            par_index2 = par_number2 + 1
            print "Set par_number %s to %s" % (par_number2, par_index2)
            title2 = self.input_data['parameters'][par_number2-1][1]

        # Read the data
        data = self.df.values

        # Select the data
        if xmin == None: xmin = data[:,par_index1].min()
        if xmax == None: xmax = data[:,par_index1].max()
        if ymin == None: ymin = data[:,par_index2].min()
        if ymax == None: ymax = data[:,par_index2].max()
        data = data[(data[:,par_index1] < xmax) & (data[:,par_index1] > xmin) & (data[:,par_index2] < ymax) & (data[:,par_index2] > ymin)]

        # Make histogram
        histo2D, yedges, xedges  = histogram2d(data[:,par_index2],data[:,par_index1],bins=bins,weights=data[:,2])
        #extent = [ yedges[0], yedges[-1],xedges[0], xedges[-1]]

        #print xmin,xmax,ymin,ymax,bins, grid_steps

        #return histo2D, xedges, yedges, xmin,xmax,ymin,ymax,bins, grid_steps

        # Make interpolation
        # Fix the limits and the steps of the histogram
        ipos = range(bins)
        x = zeros(bins)
        y = zeros(bins)
        for i in ipos: x[i] = (xedges[i]+xedges[i+1])/2
        for i in ipos: y[i] = (yedges[i]+yedges[i+1])/2
        xstep = xedges[1]-xedges[0]
        ystep = yedges[1]-yedges[0]

        interp_spline = RectBivariateSpline(x, y, histo2D,s=s)

        # Finer grid definition
        xstepi = xstep/grid_steps
        ystepi = ystep/grid_steps
        xi = arange(xmin,xmax,xstepi)
        yi = arange(ymin,ymax,ystepi)
        if size(xi) > size(yi):
            xi = xi[:size(yi)]
        elif size(yi) > size(xi):
            yi = yi[:size(xi)]
        zi = interp_spline(xi, yi)


        # Choose the cmap for likelihood plots
        if cmap == 'normal':
            cmap=plt.cm.get_cmap('jet')
        elif cmap == 'accurate':
            cmap=plt.cm.get_cmap('spectral')
        elif cmap == 'moreaccurate':
            cmap=plt.cm.get_cmap('prism')

        # Plot interpolation
        if clear: plt.clf()
        plt.xlabel('Value of parameter ' + title1)
        plt.ylabel('Value of parameter ' + title2)
        # Plot the 2D interpolation
        plt.axis([xmin, xmax, ymin, ymax])
        if levels:
            if cmap == 'k':
                plt.contour(xi, yi, zi,colors='k')
            else:
                plt.contour(xi, yi, zi,cmap=cmap)
        else:
            plt.pcolormesh(xi,yi,zi,cmap=cmap)
        #plt.imshow(histo2D[::-1,],interpolation='nearest',extent=extent,aspect='auto',cmap=cmap)
        cbar = plt.colorbar()
        cbar.set_label('Probability')
        plt.tight_layout()
        '''
        # method with griddata not so good #################################################
        data = zeros((nix*niy,3))
        for ii in range(nix*niy):
            data[ii,:] = [xmin+int(ii/nix)*xstep,
                            ymin +mod(ii,nix)*ystep,
                            histo2D[mod(ii,nix),int(ii/nix)]]
        # Generate a regular grid to interpolate the data.
        f= 10
        xi = linspace(xmin, xmax, f*nix)
        yi = linspace(ymin, ymax, f*niy)
        xi, yi = meshgrid(xi, yi)
        # Interpolate using delaunay triangularization
        zi = mlab.griddata(data[:,0],data[:,1],data[:,2],xi,yi)
        '''


        ################################# GetDist functions ################################################################ !!

    def histo_interp(self,par_name,path=currentpath):
        '''
        Interpolated histogram plot using GetDist package
        '''
        from getdist import plots

        self.path = path

        if type(par_name) <> str:
            print('Use a parameter name and not a number')
            return
        
        g = plots.get_single_plotter()
        g.plot_1d(self.path+'/nf_output_points',par_name)

        #---------------------------------------------------------------------------------------------------------------------

    def histo2D_interp(self,par_name1,par_name2,path=currentpath):
        '''
        Interpolated histogram plot using GetDist package
        '''
        from getdist import plots

        self.path = path

        if type(par_name1) <> str or type(par_name2) <> str:
            print('Use a parameter name and not a number')
            return
        
        g = plots.get_single_plotter()
        g.plot_2d(self.path+'/nf_output_points',par_name1,par_name2,filled=True)

        #---------------------------------------------------------------------------------------------------------------------
        
    def triangle_plot(self,path=currentpath):
        '''
        Triangle plot of all probability distributions using GetDist package
        '''
        from getdist import plots

        self.path = path


        g = plots.get_subplot_plotter()
        g.triangle_plot(self.path+'/nf_output_points',filled=True)



        #---------------------------------------------------------------------------------------------------------------------
    def get_cov(self,path=currentpath):
        '''
        Covariance matrix using GetDist package
        '''
        from getdist import mcsamples

        self.path = path


        g = mcsamples.loadMCSamples(self.path+'/nf_output_points')
        return g.getCov()


#########################################################################################################################################
class Summary(object):
    '''
    Built the dataframe from the results placed in different directories.
    '''

    def __init__(self, **kwargs):
        self.res = pd.DataFrame()

    def add_simulations(self, directories,labels,prefix=""):
        '''
        Built the dataframe from the results placed in different directories.
        Provide the list of directories and the relative label please.
        Additionally, the prefix to be used.
        '''

        # Read all names of parameters availables
        par_tmp = []
        for dir in directories:
            if dir[-1] == '/':
                dirname =  dir
            else:
                dirname = dir+'/'
            print 'Loading directory ', dir
            # Start analysis in this directory if the analysis is done
            if not os.path.isfile(dirname+'nf_output_points.txt'):
                print 'Result file nf_output_points.txt not present\n Nothing to load'
                continue
            an = Analysis(path=dirname)
            print 'Analysis in ' + dirname
            #
            # Read results input and output types
            input_types = an.load_input(dirname).keys()
            output_types = an.load_output_results(dirname).keys()
            #
            input_par = an.load_input(dirname)
            par_tmp = par_tmp + [p[1].replace("'", "") for p in input_par['parameters']]
            #print par_tmp
        parameters = []
        [parameters.append(x) for x in par_tmp if x not in parameters]
        print "Parameters  found ", parameters


        # Exceptions that have to be expanded with the parameter name
        inputs_par = ["value", "step","minl","maxl","fixed"]
        outputs_par = ["max", "mean", "sd", "median", "conf_level_m99", "conf_level_m95", "conf_level_m68", "conf_level_p99", "conf_level_p95", "conf_level_p68" ]

        #  Define the labels expanding lables that depend on the different parameters
        summary = {"labels": []}

        print '\n #### Available input keys ###'
        for k  in input_types:
            if k != 'parameters':
                summary[k] = []
                print k
        for i in inputs_par:
             print i
             for p in parameters:
                    summary[i+'_'+p] = []

        #print(len(summary))

        print '\n #### Available output keys ###'
        for k  in output_types:
            if k in outputs_par:
                print k
                for p in parameters:
                    summary[k+'_'+p] = []
            elif k != 'ntry':
                summary[k] = []
                print k

        # Fill the labels
        for dir in directories:
            if dir[-1] == '/':
                dirname =  dir
            else:
                dirname = dir+'/'
            # Check if the analysis is done in the directory
            if not os.path.isfile(dirname+'nf_output_points.txt'):
                print 'Result file nf_output_points.txt not present\n Nothing to load'
                continue
            input = an.load_input(dirname)
            output = an.load_output_results(dirname)


            dir_output = {}
            # Read and fill the different information

            # From inputs
            for k, v  in input.iteritems():
                if  k != 'parameters':
                    summary[k].append(v)
                    #print k, v
                elif k == 'parameters':
                    for i in v:
                        dd,ll = shape(v)
                        for d in range(dd):
                            p = v[d][1].replace("'", "")
                            for l in range(ll-2):
                                summary[inputs_par[l]+'_'+p] = v[d][l+2]
                                #print inputs_par[l]+'_'+p,  v[d][l+2]





            # From outputs
            for k, v  in output.iteritems():
                if k in outputs_par:
                    for pn, p in enumerate(an.parameters):
                        dir_output[k+'_'+p] = v[pn]
                elif k !=  'ntry':
                    dir_output[k] = v

            for k  in output_types:
                if k in outputs_par:
                    for p in parameters:
                        summary[k+'_'+p].append(dir_output[k+'_'+p] if k+'_'+p in dir_output else np.nan)
                elif k !=  'ntry':
                    summary[k].append(dir_output[k] if k in dir_output else np.nan)


            # Save label of the directory
            summary["labels"].append(labels[directories.index(dir)])

#        del summary["maxl_sigma"]
        #print '\n#### Available keys input ####'
        #for k, v in summary.iteritems():
         #   print k #, len(v)
        summary = pd.DataFrame(summary).set_index("labels")
        summary = summary.rename(columns = {c: prefix + c for c in summary.columns})

        return summary

    def merge_columns(self,df,par1,par2):
        '''
        Eliminate redoundant columns relative to equivalent parameters.
        The first parameter name will be the final parameter name
        '''
        key1 = '_' + par1
        for col1 in df.columns:
            if col1.endswith(key1):
                col2 = col1[:col1.rindex('_')]+'_'+par2
                old_col1 = col1
                old_col2 = col2
                new_col = old_col1
                df[new_col] = df[old_col1].fillna(df[old_col2])
                del df[old_col2]

        return df


#------------- Additional tools

def make_colormap(seq):
    """Return a LinearSegmentedColormap
    seq: a sequence of floats and RGB-tuples. The floats should be increasing
    and in the interval (0,1).
    """
    import matplotlib.colors as mcolors

    seq = [(None,) * 3, 0.0] + list(seq) + [1.0, (None,) * 3]
    cdict = {'red': [], 'green': [], 'blue': []}
    for i, item in enumerate(seq):
        if isinstance(item, float):
            r1, g1, b1 = seq[i - 1]
            r2, g2, b2 = seq[i + 1]
            cdict['red'].append([item, r1, r2])
            cdict['green'].append([item, g1, g2])
            cdict['blue'].append([item, b1, b2])
    return mcolors.LinearSegmentedColormap('CustomMap', cdict)
