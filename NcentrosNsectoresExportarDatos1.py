# -*- coding: utf-8 -*-
"""
Created on Mon Nov  8 16:09:50 2021

@author: Intel
"""
#from itertools import product
from math import sqrt
import pandas as pd
from pandas import DataFrame

import gurobipy as gp
from gurobipy import GRB
import haversine as hs
import os
from geopy.distance import geodesic


#https://towardsdatascience.com/calculating-distance-between-two-geolocations-in-python-26ad3afe287b

#os.chdir("D://DOCUMENTOS//UNIVERSIDAD_DE_LIMA//2021//PROYECTOS_2//2021 2//seccion1025//logisticaHumanitaria//modeloPython")

def compute_distance(loc1, loc2):
    
    #print("el valor de la distancia es : ", geodesic(loc1, loc2).kilometers)  # 23.576805481751613
    #print(geodesic(origin, dist).miles)  # 14.64994773134371
    valor=geodesic(loc1, loc2).kilometers    
    return valor

def solve_sjl_logisticaHumanitaria(c_coordinates, demand,m,n): # recibe 

    #####################################################
    #                    Data
    #####################################################
    
    # Indices for the counties
    counties = [*range(1,m+1)]
    #The * "unpacks" an iterable, so that each element is passed as a separate argument, rather than the function receiving the iterable object as a single argument:
    
    
    # Indices for the facilities
    facilities = [*range(1,n+3)]
    #print("el indice de facilities es :", facilities)
    # Create a dictionary to capture the coordinates of an existing facility and capacity of treating COVID-19 patients
    
    #########################################################
    # aqui construir este diccionario A
    #########################################################
    
    
    #os.chdir("D://DOCUMENTOS//UNIVERSIDAD_DE_LIMA//2021//PROYECTOS_2//2021 2//seccion1025//logisticaHumanitaria//modeloPython")
    df100=pd.read_excel('geolocalizacionAsignacion3Centros.xlsx')#, index_col=5)  
    dicc=list()
    for x in range(n):
        b5=[df100.iloc[x]['lat'],df100.iloc[x]['lng']]
        b5=tuple(b5)
        dicc.append(b5)
    #dicc
    #secuencia=[1,2,3,4]
    k=n+1
    numbers = range(1, k)
    secuencia = [number for number in numbers]
    #print("la secuencia es :" , secuencia)
    # Create a zip object from two lists
    zipbObj = zip(secuencia, dicc)
    # Create a dictionary from zip object
    diccionarioA = dict(zipbObj)
    #diccionarioA
    from collections import defaultdict
    d1 = diccionarioA
    
    
    #d2 = {1: 100, 2: 207, 3:20, 4:100}
    
    dicc2=list()
    for x in range(n):
        b=df100.iloc[x]['kits']
        dicc2.append(b)  
    # Create a zip object from two lists
    zipbObj2 = zip(secuencia, dicc2)
    # Create a dictionary from zip object
    diccionarioB = dict(zipbObj2)
    d2 = diccionarioB  
    dd = defaultdict(list)
    for d in (d1, d2): # you can list as many input dicts as you want here
        for key, value in d.items():
            dd[key].append(value)
    
    #print(dd)
    
    #existing, e_coordinates, e_capacity  = gp.multidict({
    #    1: [(-12.0197003, -76.9551148), 281],
    #    2: [(-12.0167619,-76.9495009), 187],
    #    3: [(-12.0161028, -76.9441662), 200]

    #})
   
    existing, e_coordinates, e_capacity  = gp.multidict(dd)
    
    # Create a dictionary to capture the coordinates of a temporary facility and capacity of treating COVID-19 patients
    
    
    
    
    
    temporary, t_coordinates, t_capacity  = gp.multidict({
        n+1: [(-12.0194141,-76.9526184), 100],
        n+2: [(-12.0186654,-76.9522931), 100]
    })
    
    # Cost of driving 10 miles
    dcost = 50
    
    # Cost of building a temporary facility with capacity of 100 COVID-19
    tfcost = 500000
    
    # Compute key parameters of MIP model formulation
    f_coordinates = {}
    for e in existing:
        f_coordinates[e] = e_coordinates[e]
    
    for t in temporary:
        f_coordinates[t] = t_coordinates[t]
    
    # Cartesian product of counties and facilities
    cf = []
    
    for c in counties:
        for f in facilities:
            tp = c,f
            cf.append(tp)
        
    # Compute distances between counties centroids and facility locations
    distance = {(c,f): compute_distance(c_coordinates[c], f_coordinates[f]) for c, f in cf}
    #df = pd.DataFrame(data=distance, index=[0])
    #df = (df.T)
    #df.to_excel('test3.xlsx', sheet_name='sheet1')#, index=False)
    
    
    #####################################################
    #                    MIP Model Formulation
    #####################################################
    
    m = gp.Model('logisticaHumanitariaSJL')
    
    # Build temporary facility
    y = m.addVars(temporary, vtype=GRB.BINARY, name='temporary')
    
    # Assign COVID-19 patients of county to facility
    x = m.addVars(cf, vtype=GRB.INTEGER, name='Assign')
    
    # Add capacity to temporary facilities
    z = m.addVars(temporary, vtype=GRB.INTEGER, name='addCap' )
    
    # Objective function: Minimize total distance to drive to a COVID-19 facility
    
    # Big penalty for adding capacity at a temporary facility
    bigM = 1e9
    
    m.setObjective(gp.quicksum(dcost*distance[c,f]*x[c,f] for c,f in cf) 
                   + tfcost*y.sum()
                   + bigM*z.sum(), GRB.MINIMIZE)
    
    # Counties demand constraints
    demandConstrs = m.addConstrs((gp.quicksum(x[c,f] for f in facilities) == demand[c] for c in counties), 
                                 name='demandConstrs')
    
    # Existing facilities capacity constraints
    existingCapConstrs = m.addConstrs((gp.quicksum(x[c,e]  for c in counties) <= e_capacity[e] for e in existing ), 
                                      name='existingCapConstrs')
    
    # temporary facilities capacity constraints
    temporaryCapConstrs = m.addConstrs((gp.quicksum(x[c,t]  for c in counties) -z[t] 
                                        <= t_capacity[t]*y[t] for t in temporary ),
                                       name='temporaryCapConstrs')
    # Run optimization engine
    m.optimize()
    
    #####################################################
    #                    Output Reports
    #####################################################
    
    # Total cost of building temporary facility locations
    temporary_facility_cost = 0
    
    print(f"\n\n_____________Optimal costs______________________")
    

    for t in temporary:
        if (y[t].x > 0.5):
            temporary_facility_cost += tfcost*round(y[t].x)

    
    patient_allocation_cost = 0
    for c,f in cf:
        if x[c,f].x > 1e-6:
            patient_allocation_cost += dcost*round(distance[c,f]*x[c,f].x)
            
    print(f"El costo total de implementar centros de respuesta temporal ante la emergencia es ${temporary_facility_cost:,}") 
    print(f"El costo total de implementar en los centros ya establecidos para ese fin es ${patient_allocation_cost:,}")  
    
    # Build temporary facility at location
    
    print(f"\n_____________Plan para centros de respuesta temporal______________________")
    temporalesA={}
    
    for t in temporary:
        if (y[t].x > 0.5):
            # CONSTRUIR DICCIONARIO
            temporalesA.update({t:f"Se requiere implementar un centro temporal en: {t}"})        
            print(f"Se requiere implementar un centro temporal de atención en {t}")
    
    print(temporalesA)            
            
    # Extra capacity at temporary facilities
    print(f"\n_____________Plan para incrementar la capacidad en centros de respuesta temporales______________________")
    for t in temporary:
        if (z[t].x > 1e-6):
            print(f"Incrementar la capacidad del centro de atencion temporal en la ubicacion {t} con {round(z[t].x)} kits de ayuda")

    # Demand satisfied at each facility
    f_demand = {}
    
    print(f"\n_____________Asignación de los danmificados  en el centro de atención ______________________")
    for f in facilities:
        temp = 0
        for c in counties:
            allocation = round(x[c,f].x)
            if allocation > 0:
                print(f"{allocation} Los danmificados del sector {c} deben ser asignados al centro de apoyo {f} ")
            temp += allocation
        f_demand[f] = temp
        #print(f"{temp} es el total de danmificados atendidos en el centro de apoyo {f}. ")
        print(f"\n________________________________________________________________________________")
        
    
    
    cf2 = []
    
    for c in counties:
        for f in facilities:
            tp = c,f
            cf2.append(tp)
        
    # Compute distances between counties centroids and facility locations
    asigna = {(c,f):compute_distance(c_coordinates[c], f_coordinates[f]) for c, f in cf2}
    #dfasigna = pd.DataFrame(data=asigna, index=[0])
    #print(dfasigna)
    #dfasigna = (dfasigna.T)
   # dfasigna.to_excel('test8.xlsx', sheet_name='sheet1')#, index=False)
    
    
    # Test total demand = total demand satisfied by facilities
    total_demand = 0
    
    for c in counties:
        total_demand += demand[c]
        
    demand_satisfied = 0
    for f in facilities:
        demand_satisfied += f_demand[f]
        
   # nivel_servicio = demand_satisfied/total_demand*100
    
    print(f"\n_____________Balance requerimiento = centros atención______________________")
    print(f"El total de la demanda de kits requerido de ayuda es: {total_demand:,} kits")
    #print(f"El total de la demanda de kits de ayuda entregada es: {demand_satisfied:,} kits")
   # print(f"La proporción de la demanda de kits atendida entregada es: {nivel_servicio:,} %")
    
    return temporalesA

def crea_demanda(m,n):
    
    # m : es la cantidad sectores donde se debe ayudar
    #os.chdir("D://DOCUMENTOS//UNIVERSIDAD_DE_LIMA//2021//PROYECTOS_2//2021 2//seccion1025//logisticaHumanitaria//modeloPython")
    df=pd.read_excel('demandaGrupoAsignacion3.xlsx')#, index_col=5)  
    dicc=list()
    for x in range(m):
        b5=[df.iloc[x]['Latitud'],df.iloc[x]['Longitud']]
        b5=tuple(b5)
        dicc.append(b5)
    dicc
    
    
    k=m+1
    numbers = range(1, k)
    secuencia = [number for number in numbers]
    
    #secuencia=[1,2,3,4,5,6,7,8,9,10]
    
    # Create a zip object from two lists
    zipbObj = zip(secuencia, dicc)
    # Create a dictionary from zip object
    diccionarioA = dict(zipbObj)
    #diccionarioA
    from collections import defaultdict
    d1 = diccionarioA
    
    
   #d2 = {1: 1850, 2: 1530, 3:3529,4:1125,5:2529,6:3529, 7:125,8:1596,9:452,10:125}
    
    #df2=pd.read_excel('demandaGrupoAsignacion3.xlsx')#, index_col=5)  
    dicc2=list()
    for x in range(m):
        b=df.iloc[x]['demand']
        dicc2.append(b)  
    # Create a zip object from two lists
    zipbObj2 = zip(secuencia, dicc2)
    # Create a dictionary from zip object
    diccionarioB = dict(zipbObj2)
    d2 = diccionarioB
    
    dd = defaultdict(list)
    for d in (d1, d2): # you can list as many input dicts as you want here
        for key, value in d.items():
            dd[key].append(value)
    
    counties, coordinates, forecast  = gp.multidict(dd)
    #distanc2,dfasigna2= solve_sjl_logisticaHumanitaria(coordinates, forecast,m,n)
    temporalesAA= solve_sjl_logisticaHumanitaria(coordinates, forecast,m,n)
    
    return temporalesAA

#counties, coordinates, forecast  = gp.multidict(dd)


# find the optimal solution of the base scenario
#solve_sjl_logisticaHumanitaria(coordinates, forecast)

m=10  #: m:sectores
n=3   #  n: centros atencion
crea_demanda(m,n)

#print(crea_demanda(m,n))
#df = pd.DataFrame(data=crea_demanda(m,n))#, index=[0])
#df
