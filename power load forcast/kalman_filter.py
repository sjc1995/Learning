# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
from datetime import date as dt
import datetime as dtt
import matplotlib.pyplot as plt
import math

def linear_regression(data,LOAD):
	"""
	simple linear regression with the two attributes (least-square methods)

	Parameters
	-----------
	data : numpy.array
		Input array, with every column representing a series of independent variable

	LOAD : numpy.array
		Input array, with one column representing a series of dependent variable

	Returns
	--------
	beta : numpy.array
		Output array, the first element represents the intercept, 
		others represent the coefficient of every independent bariable.

	Raises
	-------
	BasException("the the length of independent variable is different from the dependent variable")
		If the length of independent variable is different frome the dependent variable
	"""
	data = np.array(data,dtype=float)
	LOAD = np.array(LOAD,dtype=float)
	if data.shape[0] == LOAD.shape[0]:
		data = np.matrix(np.c_[np.ones(data.shape[0]),data])
		beta = np.dot(np.dot(np.linalg.inv(np.dot(data.T,data)),data.T),LOAD)
	else:
		raise BaseException("the the length of independent variable is different from the dependent variable")
	return beta

def kalman_filter_temp(observation,real_value,H,P = np.eye(3),Q = 0.01,R = 0.0001):
	"""
	Do load forecast of tomorrow by kalman filter

	Parameters
	-----------
	observation : numpy.float
		Input float, representing the load of today

	real_value : numpy.array
		Input array, the first element represent the intercept of the
		return of linear regression of the load today with temperture 
		and the load yesterday, the second one represent the temperture
		of today and the third one represent the load yesterday.

	H : numpy.array
		Input array, the first element is 1, the second and the third one
		represent the coefficients of temperture and load yesterday by 
		linear regression of the load today with temperture and the load 
		yesterday.

	P : numpy.matrix
		Input matrix, represent the covariance matrix

	Q : numpy.float
		Input float, representing the forecast error, default value is 0.01

	R : numpy.float
		Input folat, representing the observation error, default value is 0.0001

	Returns
	--------
	pre_value : numpy.float
		Output float, the element is a float representing the forecast load

	real_value : numpy.array
		the second one is the "real_value" will be used in the next time
		
	P : numpy.matrix
		the third one is the "P" will be used in the next time 
	"""
	obervation = np.array(observation)
	real_value = np.array(real_value)
	A = np.eye(3)
	pre_value = np.dot(A , real_value)
	pre_value = np.matrix(pre_value)
	Pminus = np.dot(np.dot(A,P),A.T)+Q    
	K = (np.dot(Pminus,H.T))/((np.dot(np.dot(H,Pminus),H.T)+R))
	real_value = pre_value.T+K*(observation-np.dot(H,pre_value.T))
	P = np.dot((np.eye(3)-np.dot(K,H)),Pminus)
	pre_value = np.dot(H,real_value)
	pre_value = np.array(pre_value,dtype=float)
	real_value = np.array(real_value,dtype=float)
	P = np.array(P,dtype=float)
	return (pre_value,real_value,P)
		
def kf_app(TEMP,LOAD,day1,s,P = np.eye(3),x = np.zeros((1,3))):
	"""
	Do load forecast of tomorrow by kalman filter

	Parameters
	-----------
	TEMP : numpy.array
		The everyday average temperature from this day of last year to yesterday

	LOAD : numpy.array
		The everyday max load from this day of last year to yesterday

	day : numpy.int
		The length of data used to do simple regression

	s : numpy.int
		the forecast load will be revised by the load this number of days ago

	P : numpy.matrix
		Input matrix, represent the covariance matrix

	x : numpy.matrix

	Returns
	--------
	pre_value : numpy.folat
		Output list, the element is a float representing the forecast load

	x : numpy.array
		this one is the "x" will be used in the next time 
		
	P : numpy.matrix
		this one is the "P" will be used in the next time 

	Raises
	-------
	BaseException("the the length of the temperature is different from the load")
	"""
	TEMP = np.matrix(TEMP,dtype=float)
	LOAD = np.matrix(LOAD,dtype=float)
	TEMP_up = []
	LOAD_up = []
	TEMP_down = []
	LOAD_down = []
	for i in range(LOAD.shape[0]):
		if TEMP[i] >= 18/40:
			TEMP_up = TEMP_up + [float(TEMP[i])]
			LOAD_up = LOAD_up + [float(LOAD[i])]
		if TEMP[i] < 18/40:
			TEMP_down = TEMP_down + [float(TEMP[i])]
			LOAD_down = LOAD_down + [float(LOAD[i])]
	TEMP_up = np.array(TEMP_up)
	LOAD_up = np.array(LOAD_up)
	LOAD_down = np.array(LOAD_down)
	TEMP_down = np.array(TEMP_down)
	up = linear_regression(TEMP_up.T,LOAD_up.T)
	down = linear_regression(TEMP_down.T,LOAD_down.T)

	if LOAD.shape[0]+1 == TEMP.shape[0]:
		sz = LOAD.shape
		Q = 0.01
		R = 0.0001
		pre_value = LOAD.copy()
		real_consumption = LOAD.copy()
		data = [np.matrix(np.c_[TEMP[s:],LOAD[s-1:,j]],dtype = float) for j in range(sz[1])]
		H = [np.matrix(linear_regression(data[j][-day1-1:-1,:],LOAD[-day1:,j]),dtype = float) for j in range(sz[1])]
		
		if x.shape[0] != sz[1]:
			x = np.zeros((sz[1],3))

		for j in range(sz[1]):
			H[j][0]=1
			a = kalman_filter_temp(LOAD[-1,j],x[j,:],H[j].T,P,Q,R)
			pre_value = a[0]
			x[j,0] = a[1][0]
			x[j,1] = a[1][1]
			x[j,2] = a[1][2]
			P = a[2]
			pre_value = 1/2*pre_value + 1/2*LOAD[-1,j]
			if abs(TEMP[-1]-TEMP[-1-s]) > 4/40 :
				if TEMP[-1] < 18/40 :
					pre_value = down[:,0] + down[:,1] * float(TEMP[-1])
				elif TEMP[-1-s] > 18/40 :
					pre_value = up[:,0] + up[:,1] * float(TEMP[-1])
		return (pre_value, x, P)
	else:
		raise BaseException("the the length of the temperature is different from the load")

def test(year=0,month=0,day=0):
	"""
	the function which runs everyday, need a csv file with some column called "max_load","average_c"
	"weekday" and "hol"
	we should update the csv file everyday
	"""
	reader = pd.read_csv('dailyDATA_clean.csv', index_col='date',encoding="gbk")
	td = dtt.datetime(2009, 10, 1) - dtt.datetime(2009, 9, 30)
	if year==0:
		today = dtt.date.today()
	else:
		today = dtt.datetime(year, month, day)
	time = [today - td]
	j = 365
	for i in range(j-1):
		time.insert(0,time[0]-td)
	times = [item.strftime('%Y/%m/%d') for item in time]
	today = today.strftime('%Y/%m/%d')
	LOAD = reader.loc[times]['max_load']
	TEMP = reader.loc[times]['average_c']
	LOAD_wd = []
	LOAD_we = []
	TEMP_wd = []
	TEMP_we = []
	for i in range(j):
		if reader.loc[times]['hol'][times[i]] in ["周末",]:
			LOAD_we = LOAD_we + [LOAD.loc[times[i]]]
			TEMP_we = TEMP_we + [TEMP.loc[times[i]]]
		elif reader.loc[times]['hol'][times[i]] in ["工作日",]:	
			LOAD_wd = LOAD_wd + [LOAD.loc[times[i]]]
			TEMP_wd = TEMP_wd + [TEMP.loc[times[i]]]
		elif reader.loc[times]['hol'][times[i]] in ["小长假"]:
			LOAD_wd = LOAD_wd + [LOAD.loc[times[i]]*1.2]
			TEMP_wd = TEMP_wd + [TEMP.loc[times[i]]]
	
	if reader.loc[today]['hol'] in ["周末",]:
		LOAD_we = np.matrix(LOAD_we)
		TEMP_we = TEMP_we + [reader.loc[today]['average_c']]
		TEMP_we = np.matrix(TEMP_we)
		LOAD_we = LOAD_we.T
		TEMP_we = TEMP_we.T
		L2 = (LOAD_we-10000)/15000
		T2 = (TEMP_we-0)/40
		day1 = 10
		try:
			P = np.load("P_we.npy")
			x = np.load("x_we.npy")
			we = kf_app(T2, L2, day1, 2, P, x)
			forecast = we[0]
			x = we[1]
			P = we[2]
			np.save("x_we.npy",x)
			np.save("P_we.npy",P)
			forecast = forecast*15000+10000
			if reader.loc[today]['weekday'] in ["星期六",]:
				forecast = 0.25*forecast + 0.75*(0.91*LOAD[-1])
			elif reader.loc[today]['weekday'] in ["星期日",]:
				forecast = 0.25*forecast + 0.75*(0.95*LOAD[-1])
			else:
				forecast = 0.25*forecast + 0.75*(0.935*LOAD[-1])
			print(forecast)
			return forecast			
		except FileNotFoundError:
			we = kf_app(T2, L2, day1, 2)
			forecast = we[0]
			x = we[1]
			P = we[2]
			np.save("x_we.npy",x)
			np.save("P_we.npy",P)
			forecast = forecast*15000+10000
			if reader.loc[today]['weekday'] in ["星期六",]:
				forecast = 0.25*forecast + 0.75*(0.91*LOAD[-1])
			elif reader.loc[today]['weekday'] in ["星期日",]:
				forecast = 0.25*forecast + 0.75*(0.95*LOAD[-1])
			else:
				forecast = 0.25*forecast + 0.75*(0.935*LOAD[-1])
			print(forecast)
			return forecast
	else:
		TEMP_wd = TEMP_wd + [reader.loc[today]['average_c']]
		LOAD_wd = np.matrix(LOAD_wd)
		TEMP_wd = np.matrix(TEMP_wd)
		LOAD_wd = LOAD_wd.T	
		TEMP_wd = TEMP_wd.T
		L1 = (LOAD_wd-10000)/15000
		T1 = (TEMP_wd-0)/40
		day1 = 10
		try:
			P = np.load("P_wd.npy")
			x = np.load("x_wd.npy")
			wd = kf_app(T1, L1, day1, 1, P, x)
			forecast = wd[0]
			x = wd[1]
			P = wd[2]
			np.save("x_wd.npy",x)
			np.save("P_wd.npy",P)
			if reader.loc[today]['hol'] in ["小长假"]:
				forecast = forecast*1/1.2
			forecast = forecast*15000+10000
			print(forecast)
			return forecast
		except FileNotFoundError:
			wd = kf_app(T1, L1, day1, 1)
			forecast = wd[0]
			x = wd[1]
			P = wd[2]
			np.save("x_wd.npy",x)
			np.save("P_wd.npy",P)
			if reader.loc[today]['hol'] in ["小长假"]:
				forecast = forecast*1/1.2
			forecast = forecast*15000+10000
			print(forecast)
			return forecast


yn = input("today?(y/n)")
if yn in ["y","Y"]:
	test()
else:
	x = input("year:")
	y = input("month:")
	z = input("day:")
	test(int(x),int(y),int(z))
                
input("Press Enter to exit")