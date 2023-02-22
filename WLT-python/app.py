# %%
from shiny import App, render, ui

# need to import pyfetch if running in pyodide

# import sys
# if "pyodide" in sys.modules:
#     from pyodide.http import pyfetch
#     response = await pyfetch("https://some_url/myfiles.zip")
#     await response.unpack_archive()
# else

import urllib.request as urllib2

# Import modules for plot rendering
import numpy as np
import matplotlib.pyplot as plt

url = "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv"

# %%.
class WeightData:
    def __init__(self, dates = None, weights = None):
        if dates is None or weights is None:
            self.dates, self.weights = self._getData()
        else:
            self.dates = dates
            self.weights = weights

    def _getData(self):
        print("Getting data from Google Sheets")

        dates = []
        weights = []
        for line in urllib2.urlopen(url):
            date, weight = line.decode("utf-8").split(",")[0:2]
            if date == "date":
                continue
            dates.append(date)
            weights.append(float(weight))

        return (
            np.array(dates, dtype="datetime64"),
            np.array(weights, dtype="float"),
            )

    def filter(self, start, end = None):
        start = np.datetime64(start)
        if end is None:
            end = np.datetime64("today")
        
        inRng = (self.dates >= start) & (self.dates <= end)
        return WeightData(self.dates[inRng], self.weights[inRng])
    
    def last(self, n):
        return WeightData(self.dates[-n:], self.weights[-n:])

    def __repr__(self):
        return f"WeightData Object\n{self.dates.size} data points from {self.dates[0]} to {self.dates[-1]}"
    
    def __len__(self):
        return len(self.dates)

    def __getitem__(self, index):
        return WeightData(self.dates[index], self.weights[index])


# %%

app_ui = ui.page_fluid(
    ui.input_slider("n", "Number of data points", 0, 100, 100),
    ui.output_text_verbatim("data"),
    ui.output_plot("plot"),
)

def server(input, output, session):
    allData = WeightData()

    @output
    @render.plot(alt="A histogram")
    def plot():
        data = allData.last(input.n())
        fig, ax = plt.subplots()
        ax.scatter(data.dates, data.weights)
        return fig

    @output
    @render.text()
    def data():
        data = allData.last(input.n())
        return str(data.last(input.n()))

app = App(app_ui, server, debug=True)
