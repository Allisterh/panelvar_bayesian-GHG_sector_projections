# -*- coding: utf-8 -*-
"""


@author: ZL,CSY
"""
import numpy as np
import pandas as pd
from interval import Interval
from scipy.optimize import leastsq 
import statsmodels.api as sm
import warnings
warnings.simplefilter('error')
warnings.simplefilter('ignore')
pathin = '' #File path

data_all = pd.read_excel(pathin + 'all20210512.xlsx') #Raw data of projrcting countries and areas
data_refe = pd.read_excel(pathin + 'reference20210512.xlsx') #Raw data of reference countries and areas



name_all = data_all.loc[:,'country']
name_refe = data_refe.loc[:,'country']
#-----------Define the fitting function---------------------------------------------------
def func(p,x): 
  return p*x
def error(p,x,y):
  return func(p,x)-y
start = [0]
#------------For the projecting countries and areas: countries and areas with fitting parameter k greater than 0 (i.e., does not fit the logistic curve) are excluded--------------------------------------------
data_delet = []
for i in range(len(name_all)):  #The data for every projecting countries and areas are fitted
  x = np.arange(-49,1,1)
  y0 = np.array(data_all.iloc[i,1:51],dtype='float64')
  beta = 90 if max(y0)<85 else 100
  y= np.log(1/y0-1/beta)-np.log(1/beta)-np.log(beta/y0[-1]-1)
  para_fit = leastsq(error, start, args=(x,y))
  if para_fit[0]>0:  #countries and areas with fitting parameter k greater than 0 are excluded
    data_delet.append(name_all[i])
data_all = data_all.loc[~data_all['country'].isin(data_delet)]  #Get all data after exclusion


#------------For the refernece countries and areas: countries and areas with fitting parameter k greater than 0 are excluded--------------------------------------------
data_delet_refe = []
for i in range(len(name_refe)):  #The data for every reference countries and areas are fitted
  x = np.arange(-49,1,1)
  y0 = np.array(data_refe.iloc[i,1:51],dtype='float64')
  beta = 90 if max(y0)<85 else 100
  y= np.log(1/y0-1/beta)-np.log(1/beta)-np.log(beta/y0[-1]-1)
  para_fit = leastsq(error, start, args=(x,y))

  if para_fit[0]>0:   #countries and areas with fitting parameter k greater than 0 are excluded
    data_delet_refe.append(name_refe[i])
data_refe = data_refe.loc[~data_refe['country'].isin(data_delet_refe)]   #Get all reference data after exclusion
name_refe = data_refe['country']



#Create standard deviation data DataFrame, no standard deviation in 2010, set to 0
country_num = len(data_all['country'])
results_SD = pd.DataFrame(np.zeros((country_num,1)), columns = [2009]) 



###############################################################################
'''-----------------------------Defining functions for urbanization level projection--------------------------------------------------------'''
###############################################################################
def urban_predict(data_all,data_refe,year_base,year_pred,level): 
#level(speed) = 1(fast),2(moderate),3(slow)
#year_base: baseline year
#year_pred: projecting year
  print(year_base)
  #--------------Select reference countries and areas for each projecting countries and areas: Select countries and areas that have experienced urbanization levels within the target country's urbanization level +-5% in baseline year------
  SD_base = results_SD[year_base]
  data_all_base = data_all[year_base]
  name_all = data_all['country']
  name_refe = data_refe['country']
  #For each country or area to be projected, select its eligible reference countries and areas
  for i in range(len(name_all)):
    exec('select_{0} = []'.format(name_all.iloc[i]))   #Place the selected reference countries and areas in a list with select + country names
    all_interval = Interval(data_all_base.iloc[i]-5,data_all_base.iloc[i]+5)    #create the +-5% interval
    for j in range(len(name_refe)):
      refe_interval = Interval(data_refe.iloc[j,1:year_base-1960+2].min(),data_refe.iloc[j,1:year_base-1960+2].max())
      if all_interval.overlaps(refe_interval):
        exec('select_{0}.append(name_refe.iloc[j])'.format(name_all.iloc[i])) # Adds the names of eligible countries and areas to the list
  
  #-----------The fitting parameters of each reference country or area were obtained to calculate the target country's k-----------------------------------------
  size0 = 20000 #Monte Carlo simulation times
  smy = np.zeros((len(name_all),2)) #Place where projected urbanization level (first column) and its standard deviation (second column) are stored
  MC = np.zeros((len(name_all),size0)) #Place where Monte Carlo simulations are stored
  #-----------For each projecting country or area, calculate the fitting parameter k of every selected reference countries and areas--------------------------
  for i in range(len(name_all)):
    print(i)
    refe_country = eval('select_'+name_all.iloc[i])    #reference countries and areas of the projecting target country or area
    refe = data_refe.loc[data_refe['country'].isin(refe_country)]   #Select the corresponding urbanization level data for these reference countries and areas
    refe = np.array(refe.iloc[:,1:2009-1960+2])#转成numpy数据
    coef_refe = np.array([])    #palce to store fitting parameter k 
    parafit_CI_refe = pd.DataFrame([],[],[])   #palce to store fitting parameter k and 95% confidence interval bounds of K
    #First fit the data for the reference countries and areas, each fitted k for the reference countries and areas has a range of uncertainty
    for j in range(len(refe)):
      start_yr = np.argmin(abs(refe[j,:]-data_all_base.iloc[i])) # Find the year in which the reference country or area's urbanization level is closest to the projecting country or area's as the baseline year
      y0 = refe[j,]
      urban_base = y0[int(start_yr)]  #Urbanization level corresponding to the baseline year
      x = np.arange(-start_yr,2009-1960+1-start_yr,1)
      beta = 90 if max(y0)<85 else 100  #Set saturation value
      y= np.log(1/y0-1/beta)-np.log(1/beta)-np.log(beta/urban_base-1)  #Changed the logistic model to a linear model
      para_fit = leastsq(error, start, args=(x,y)) #calculate the fitting parameter k 
      #calculate 95% confidence interval bounds of K
      model = sm.OLS(y,x)
      results = model.fit()
      para_fit_df = pd.DataFrame(para_fit[0])
      CI = results.conf_int()
      CIdf = pd.DataFrame(CI)
      parafit_CI = pd.concat([para_fit_df[0],CIdf],axis=1)
      coef_refe = np.append(coef_refe, para_fit[0])
      parafit_CI_refe = pd.DataFrame.append(parafit_CI_refe, parafit_CI)
    #Calculate the fitting parameter k for the target projecting country or area 
    x = np.arange(-year_base+1960,1,1)
    y0 = np.array(data_all.iloc[i,1:year_base-1960+1+1],dtype='float64')#
    beta = 90 if max(y0)<85 else 100
    y= np.log(1/y0-1/beta)-np.log(1/beta)-np.log(beta/y0[-1]-1)
    para_fit = leastsq(error, start, args=(x,y))
    parafit_CI_refe.columns=['para','CI_high','CI_low']
    parafit_CI_refe=parafit_CI_refe[parafit_CI_refe['para']<0]
    coef_refe = coef_refe[coef_refe<0]



    
    #If the number of reference k is greater than 3
    #Excluding the 30% of refernce k that differ most from the k of the target projecting country or area 
    if len(coef_refe)>3:
        coef_diff = abs(coef_refe-para_fit[0])
        coef_diff = np.sort(coef_diff)
        lower_bound = para_fit[0]-coef_diff[round(len(coef_diff)*0.7)]
        uper_bound = para_fit[0]+coef_diff[round(len(coef_diff)*0.7)]
        coef_right = Interval(lower_bound[0],uper_bound[0])
        coef_refe0 = np.array([])
        for k in range(len(coef_diff)):
          if coef_refe[k] in coef_right:
            coef_refe0 = np.append(coef_refe0,coef_refe[k])
    else:
        coef_refe0 = coef_refe
    coef_refe0 = np.sort(coef_refe0)
    isinlist = coef_refe0.tolist()
    parafit_CI_refe0 = parafit_CI_refe.loc[parafit_CI_refe['para'].isin(isinlist)]
    parafit_CI_refe0 = parafit_CI_refe0.sort_values(by='para')



    
    #If the number of reference k is greater than or equal to 3
    #Sorted from largest to smallest, the average of the first 1/3 is used as the k for fast projection
    if len(coef_refe0)>=3:
      upper = parafit_CI_refe0.iloc[0:int(len(coef_refe0)/3),:]
      SD1 = (upper.iloc[:,2]-upper.iloc[:,0])/1.96  #Calculate the standard deviation of each k
      #Monte Carlo simulation combining the standard deviations of the first 1/3 k
      MCfitK1 = np.array([])
      for up,std in zip(upper['para'],SD1):
          if std>0:
              MCfitK01 = np.random.normal(up,std,size=size0)
              MCfitK1 = np.append(MCfitK1,MCfitK01)
          else:
              std=0
              MCfitK01 = np.random.normal(up,std,size=size0)
              MCfitK1 = np.append(MCfitK1,MCfitK01)              
      SD1 = MCfitK1.std() 
      k1 = [upper.mean(axis=0).iloc[0],SD1]
      


    #Sorted from largest to smallest, the average of the middle 1/3 is used as the k for moderate projection     
      middle = parafit_CI_refe0.iloc[int(len(coef_refe0)/3):2*int(len(coef_refe0)/3),:]
      SD2 = (middle.iloc[:,2]-middle.iloc[:,0])/1.96
      MCfitK2 = np.array([])
      for up,std in zip(middle['para'],SD2):
          if std>0:
              MCfitK02 = np.random.normal(up,std,size=size0)
              MCfitK2 = np.append(MCfitK2,MCfitK02)
          else:
              std=0
              MCfitK02 = np.random.normal(up,std,size=size0)
              MCfitK2 = np.append(MCfitK2,MCfitK02)
      SD2 = MCfitK2.std() #总k的std是多少
      k2 = [middle.mean(axis=0).iloc[0],SD2]      
      

    #Sorted from largest to smallest, the average of the last 1/3 is used as the k for low projection          
      low = parafit_CI_refe0.iloc[2*int(len(coef_refe0)/3):,:]
      SD3 = (low.iloc[:,2]-low.iloc[:,0])/1.96
      MCfitK3 = np.array([])
      for up,std in zip(low['para'],SD3):
          if std>0:
              MCfitK03 = np.random.normal(up,std,size=size0)
              MCfitK3 = np.append(MCfitK3,MCfitK03)
          else:
              std=0
              MCfitK03 = np.random.normal(up,std,size=size0)
              MCfitK3 = np.append(MCfitK3,MCfitK03)
              
      SD3 = MCfitK3.std()
      k3 = [low.mean(axis=0).iloc[0],SD3]  
    #If the number of reference k is less than 3, the three scenario projections use the same k         
    else:
      SD = (parafit_CI_refe0.iloc[:,2] - parafit_CI_refe0.iloc[:,0])/1.96
      MCfitK = np.array([])
      for up,std in zip(parafit_CI_refe0.iloc[:,0],SD):
          MCfitK0 = np.random.normal(up,std,size=size0)
          MCfitK = np.append(MCfitK,MCfitK0)
      SD = MCfitK.std()
      k1 = [parafit_CI_refe0.iloc[:,0].mean(axis=0),SD]
      k2 = k1;k3 =k1

#____________________________________Projections_______________________________________________________________

    #Calculate the baseline of urbanization level: 𝛼_ij (af in line 218) and its uncertainty using Monte Carlo simulation   
    y0_baseyear_MC = np.random.normal(data_all_base.iloc[i],SD_base[i],size0)
    af = np.log(beta/(y0_baseyear_MC)-1)
    
    if level == 1:#fast
      k0=k1
    elif level == 2:#moderate
      k0=k2
    elif level == 3:#slow
      k0=k3
    else:
      print('level is wrong')


    k = np.random.normal(k0[0], k0[1], size0)  #Monte Carlo simulation for k
    MC[i,:] = beta/(1+np.exp(af+k*5))          #Combine the uncertainties of af and k
    smy[i,0] = pd.DataFrame(MC[i,:]).dropna().mean()  #Projected urbanization level
    smy[i,1] = pd.DataFrame(MC[i,:]).dropna().std()   #Standard deviation of projected urbanization level

    
  return smy
#-------------------Define the progress bar--------------------------------------------------
import time
print (time.strftime('%Y-%m-%d %H:%M:%S'))
def bat(i,a):
  i = int(i*100/a)
  k = i + 1
  str = '>'*(i//2)+' '*((100-k)//2)
  print('\r'+str+'[%s%%]'%(i+1),end = '')
  time.sleep(0.01)


'''-------------------Project 2010-2100------------------------------------------'''


for i in range(91):
  smy0 = urban_predict(data_all,data_refe,2009+i,2010+i,level=1)
  data_all = data_all.iloc[:,0:(51+i)]
  data_all[2010+i] = smy0[:,0]


  result_SD = pd.DataFrame(smy0[:,1], columns = [2010+i])
  results_SD = pd.concat([results_SD,result_SD],axis=1)
  
  bat(i,91)
print (time.strftime('%Y-%m-%d %H:%M:%S'))  

data_all.to_excel(pathin + '世行0621urban_pred_fast0.85.xls')
results_SD.to_excel(pathin + '世行0621urban_pred_fast0.85_SD.xls')




results_SD2 = pd.DataFrame(np.zeros((country_num,1)), columns = [2009]) 

for i in range(91):
  smy0 = urban_predict(data_all,data_refe,2009+i,2010+i,level=2)
  data_all = data_all.iloc[:,0:(51+i)]
  data_all[2010+i] = smy0[:,0]

  
  result_SD= pd.DataFrame(smy0[:,1], columns = [2010+i])
  results_SD2 = pd.concat([results_SD2,result_SD],axis=1)
  
  
  bat(i,91)
print (time.strftime('%Y-%m-%d %H:%M:%S')) 
 
data_all.to_excel(pathin + '世行0621urban_pred_middle0.85.xls')
results_SD2.to_excel(pathin + '世行0621urban_pred_middle0.85_SD.xls')


results_SD3 = pd.DataFrame(np.zeros((country_num,1)), columns = [2009]) 

for i in range(91):
  smy0 = urban_predict(data_all,data_refe,2009+i,2010+i,level=3)
  data_all = data_all.iloc[:,0:(51+i)]
  data_all[2010+i] = smy0[:,0]

  
  result_SD= pd.DataFrame(smy0[:,1], columns = [2010+i])
  results_SD3 = pd.concat([results_SD3,result_SD],axis=1)
  

  bat(i,91)
print (time.strftime('%Y-%m-%d %H:%M:%S'))  

data_all.to_excel(pathin + '世行0621urban_pred_low0.85.xls')
results_SD3.to_excel(pathin + '世行0621urban_pred_low0.85_SD.xls')


