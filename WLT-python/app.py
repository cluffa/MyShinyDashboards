# %%
from shiny import App, render, ui
import os, sys
import numpy as np
#from scipy.interpolate import UnivariateSpline

# return a list of lines from a url
# if running in pyodide, use pyodide.open_url
def url_open(url):
    if "pyodide" in sys.modules:
        print("Running in pyodide, using pyodide.open_url") 
        import pyodide # type: ignore
        lines = pyodide.http.open_url(url).readlines()
    else:
        from urllib.request import urlopen
        if os.path.exists("weight.csv"):
            print("Using local copy of weight.csv")
            file = open('weight.csv', 'r')
            lines = file.readlines()
        else:
            print("Downloading weight.csv from Google Sheets")
            lines = [line.decode("utf-8").strip() + "\n" for line in urlopen(url)]
            file = open('weight.csv', 'w')
            for line in lines:
                file.write(line)
            file.close()

    print(f"Read {len(lines)} lines")
    return lines

class WeightData:
    def __init__(self, dates = None, weights = None, spline = None, url = None):
        if url is None:
            self.url = "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv"

        if dates is None or weights is None:
            self.dates, self.weights = self._getData()
            order = np.argsort(self.dates)
            self.dates = self.dates[order]
            self.weights = self.weights[order]
        else:
            self.dates = dates
            self.weights = weights
        
        self.spline = spline

    def __repr__(self):
        return f"{self.dates.size} data points from {self.dates[0]} to {self.dates[-1]}"
    
    def __len__(self):
        return len(self.dates)

    def __getitem__(self, index):
        return WeightData(self.dates[index], self.weights[index], self.spline, self.url)

    def _getData(self):
        lines = url_open(self.url)

        dates = []
        weights = []
        for idx, line in enumerate(lines):
            date, weight = line.split(",")[0:2]
            if date == "date":
                continue
            elif len(dates) > 0:
                if date == dates[-1]:
                    if weight == weights[-1]:
                        s = "are the same"
                    else:
                        s = "differ"

                    print(f"Duplicate date {date} at line {idx} and {idx-1} and weights {s}, skipping line {idx}")
                    continue
                
            dates.append(date)
            weights.append(weight)

        return (
            np.array(dates, dtype="datetime64"),
            np.array(weights, dtype="float64"),
            )

    def filter(self, start, end = None):
        start = np.datetime64(start)
        if end is None:
            end = np.datetime64("today")
        
        inRng = (self.dates >= start) & (self.dates <= end)
        return WeightData(self.dates[inRng], self.weights[inRng], self.spline, self.url)
    
    def days(self, days):
        return self.filter(self.dates[-1] - np.timedelta64(days, 'D'))

    def last(self, n):
        return WeightData(self.dates[-n:], self.weights[-n:], self.spline, self.url)

    # def Spline(self):
    #         x = self.dates.astype(np.int64)
    #         order = np.argsort(x)
    #         #x = np.arange(self.dates.size)
    #         y = self.weights

    #         return UnivariateSpline(x[order], y[order], s=0)


# %%

app_ui = ui.page_fluid(
    ui.input_radio_buttons("n", "Number of days", ["7", "30", "90", "180", "365"], selected="90", inline=True),
    ui.output_plot("plot", width="90%", height="600px"),
    ui.output_text_verbatim("data"),
)

def server(input, output, session):
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates

    allData = WeightData()

    @output
    @render.plot(alt="A histogram")
    def plot():
        ndays = int(input.n())
        data = allData.days(ndays)
        fig, ax = plt.subplots()

        formats = {
            7: ("%b %d", mdates.DayLocator(interval=1)),
            30: ("%b %d", mdates.DayLocator(interval=7)),
            90: ("%b %d" ,mdates.DayLocator(bymonthday=(1, 15))),
            180: ("%b", mdates.MonthLocator(interval=1)),
            365: ("%b %Y", mdates.MonthLocator(interval=2)),
        }

        for maxDays in formats:
            if ndays <= maxDays:
                fmt, locator = formats[maxDays]
                ax.axes.xaxis.set_major_locator(locator)
                ax.axes.xaxis.set_major_formatter(mdates.DateFormatter(fmt))
                break
        
        ax.set_title(f"Weight over the last {ndays} days")
        ax.set_xlabel("Date")
        ax.set_ylabel("Weight (lbs)")

        ax.grid(True, zorder=0)

        fig.set_dpi(300)

        ax.scatter(
            data.dates,
            data.weights,
            zorder=10,
            s=10,
        )

        # x = data.dates.astype(np.float64)
        # x = np.linspace(x[0], x[-1], len(x))

        # y = np.datetime64(data.Spline(x))
        # ax.plot(x, y, zorder=5, color="red")
        
        return fig

    @output
    @render.text()
    def data():
        ndays = int(input.n())
        data = allData.days(ndays)
        return str(data)

app = App(app_ui, server)
