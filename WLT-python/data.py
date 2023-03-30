import numpy as np
import plot
from csaps import csaps
from utils import url_open


class WeightData:
    def __init__(self, data=None):
        self.url = "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv"

        if data is None:
            self.data = self._getData()
            order = np.argsort(self.data["date"])
            self.data["date"] = self.data["date"][order]
            self.data["weight"] = self.data["weight"][order]
        else:
            self.data = data

    def __repr__(self):
        return f'{self.data["date"].size} data points from {self.data["date"][0]} to {self.data["date"][-1]}'

    def __len__(self):
        return len(self.data["date"])

    def __getitem__(self, index):
        if isinstance(index, slice):
            return WeightData({key: self.data[key][index] for key in self.data})
        elif isinstance(index, str):
            return self.data[index]

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
                    print(
                        f"Skipping duplicate date {date} at line {idx - 1} and {idx} and weights"
                    )
                    continue

            dates.append(date)
            weights.append(weight)

        return {
            "date": np.array(dates, dtype="datetime64[s]"),
            "weight": np.array(weights, dtype="float64"),
        }

    def filter(self, start, end=None):
        start = np.datetime64(start)
        if end is None:
            end = np.datetime64("today")

        in_range = (self.data["date"] >= start) & (self.data["date"] <= end)
        return WeightData({key: self.data[key][in_range] for key in self.data})

    def days(self, days):
        return self.filter(self.data["date"][-1] - np.timedelta64(days, "D"))

    def last(self, n):
        return WeightData({key: self.data[key][-n:] for key in self.data})
    
    def spline_fit(self, points_out=None, smooth=0.5):
        x = self.data["date"]
        y = self.data["weight"]

        if points_out is None:
            points_out = len(x)

        x_dtype = x.dtype
        x = x.astype("int64")

        xs = np.linspace(x[0], x[-1], points_out)
        ys = csaps(x, y, xs, smooth=smooth, normalizedsmooth=True)
        return xs.astype(x_dtype), ys

    def plot(self, **kwargs):
        return plot.with_pyplot(self, **kwargs)


