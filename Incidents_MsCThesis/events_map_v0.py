from datetime import datetime
from datetime import date
import itertools
import urllib.request
import geojson
import csv
import pandas as pd
import numpy as np
from scipy.stats import wilcoxon
import seaborn as sns
import matplotlib.pyplot as plt

class Event:
    newid=itertools.count()
    def __init__(self, zone: str, country: str, datestart: datetime, dateend=None, datepeak=None, severity=1):
  
        #Assign to self object
        self.event_id=next(self.newid)
        self.zone=zone
        self.country=country
        self.datestart=datetime.strptime(datestart, "%b-%y")
        self.severity=severity
        try:
            self.dateend=datetime.strptime(dateend, "%b-%y")
            self.datepeak=datetime.strptime(datepeak, "%b-%y")
        except:
            pass


    def calculate_events(self):
        #Complete missing attributes date end and date peak if missing
        if hasattr(self, 'dateend'):
            pass
        else:
            self.dateend=datetime.strptime(f'{self.datestart.year}-12-31',"%Y-%m-%d")
            print(f"Date end for Event {self.event_id} is 'None', date set to date start: {self.dateend}")

            
        if hasattr(self, 'datepeak'):
            pass
        else:
            self.datepeak=self.dateend
            print(f"Date peak for Event {self.event_id} is 'None', date set to date end: {self.dateend}")


        return self
    
    def find_locations(self):
        target = self.zone + ',' + self.country
        adress = f"https://nominatim.openstreetmap.org/search.php?q={target}&limit=1&polygon_geojson=1&polygon_threshold=0.01&format=geojson"
        print(adress)
        destination = f"IncidentGeometries/{self.country}_{self.zone}_{self.event_id}.geojson"
        try:
            urllib.request.urlretrieve(adress, destination)
            print("SUCCESS! Features for")
            print(self.event_id)

        except:
            print('WARNING: FAILED TO PROCESS')
            print(self.event_id)

        return self

    def timeseries_from_event(self,datesrange):
        self.ts=[]
        for d in datesrange:
            self.ts.append(self.datestart <= d <= self.dateend)

        return self.ts



with open('Incidents_Chennai_Sample.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    list_events = []
    for row in csv_reader:
        if line_count == 0:
            print(f'Column names are {", ".join(row)}')
            line_count += 1
        else:
            print(f'Event in {row[0]}, {row[1]} starting in {row[2]} and ending in {row[3]} has been processed.')
            line_count += 1
            list_events.append(Event(row[0], row[1], row[2], row[3], row[4]))
    print(f'Processed {line_count} lines.')

df_stor=pd.read_csv('stor.csv', index_col='date')['8675']
df_wg=pd.read_csv('wg.csv', index_col='date')['8675']

datesrange= []
for d in df_stor.index:
    datesrange.append(datetime.strptime(d, "%Y-%m-%d"))

pre_ts=np.full((1, len(datesrange)), False)[0]#[]
es=[]
for e in list_events:
    e=e.calculate_events()
    es.append(e)
    pre_ts=np.logical_or(pre_ts,e.timeseries_from_event(datesrange))
    e.find_locations()

pre_ts
pre_tsls=pre_ts.tolist()

pre_tsdf=pd.DataFrame([df_stor.values,df_wg.values, pre_tsls]).T

pre_tsdf.index=df_stor.index

w_stor, p_stor = wilcoxon(df_stor.values, pre_tsls)
w_wg, p_wg = wilcoxon(df_wg.values, pre_tsls)

fig,ax=plt.subplots()

ax.plot(pd.to_datetime(pre_tsdf.index),df_wg.values)
#ax.plot(pd.to_datetime(pre_tsdf.index),df_stor.values)


for e in es:
    print(e.event_id, e.datestart.strftime("%Y-%m-%d"), e.dateend.strftime("%Y-%m-%d"))
    ax.axvspan(e.datestart.strftime("%Y-%m-%d"), e.dateend.strftime("%Y-%m-%d"), facecolor='red', alpha=.2)
plt.show()

