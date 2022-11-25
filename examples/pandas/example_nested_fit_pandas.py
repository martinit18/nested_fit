#!/usr/bin/env python
# coding: utf-8

# In[1]:


from matplotlib.pyplot import * 
from numpy import *
import nested_res_V4p0 as nr
import minuit_fit_res as mr
import glob
import pandas as pd
from scipy.optimize import curve_fit

get_ipython().run_line_magic('matplotlib', 'inline')
#%matplotlib widget

linestyle = {"markeredgewidth":2, "elinewidth":2, "capsize":4,"markersize":3}


# In[2]:


pxl_to_eV = 0.0380


# ## Profile study He-like low accuracy 90%

# In[400]:


summary = nr.Summary()
an = nr.Analysis(None)


# In[401]:


directories = ['work_analysis_2021_06_V2/90/res_He-like_GE_90_1024x1026_GAUSS_LA',
               'work_analysis_2021_06_V2/90/res_He-like_GE_90_1024x1026_SUPERGAUSS_LA',
              'work_analysis_2021_06_V2/90/res_He-like_GE_90_1024x1026_ERFPEAK_LA',
              'work_analysis_2021_06_V2/90/old_ERFPEAKs/res_He-like_GE_90_1024x1026_ERFPEAK_LA',
              'work_analysis_2021_06_V2/90/old_ERFPEAKs/res_He-like_GE_90_1024x1026_ERFPEAK_bound_LA']
labels = ['GAUSS','SUPERGAUSS','ERFPEAK','ERFPEAK old','ERFPEAK old with bounds']


# In[402]:


df = summary.add_simulations(directories,labels)
df['binning']=2


# In[403]:


df['a in eV'] = df['mean_a']*pxl_to_eV*df['binning']
df['err a in eV'] = df['sd_a']*pxl_to_eV*df['binning']


# In[404]:


df[['evidence','evidence_err_est','nlive','mean_a','sd_a','mean_amp','mean_sigma','sd_sigma','mean_w','a in eV','err a in eV']]


# In[405]:


df['a in eV'].plot(yerr=df['err a in eV'],fmt='or',linestyle= 'None',**linestyle)
ylabel('Position (eV)')


# In[406]:


df['evidence'].plot(yerr=df['evidence_err_est'],fmt='ob',linestyle= 'None',**linestyle)
ylabel('Evidence (in log)')


# In[407]:


an.plot(path='work_analysis_2021_06_V2/90/res_He-like_GE_90_1024x1026_ERFPEAK_LA',xmin=300,xmax=430)


# In[411]:


an = nr.Analysis(path='work_analysis_2021_06_V2/90/res_He-like_GE_90_1024x1026_ERFPEAK_LA')
get_ipython().run_line_magic('matplotlib', 'inline')
#%matplotlib widget
an.histo('w',path='work_analysis_2021_06_V2/90/res_He-like_GE_90_1024x1026_ERFPEAK_LA',bins=200)


# In[412]:


an.histo2D('w','sigma',ymin=5,ymax=20,bins=100,path='work_analysis_2021_06_V2/90/res_He-like_GE_90_1024x1026_ERFPEAK_LA')

