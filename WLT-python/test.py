# tests and benchmarks
import os
import sys
from contextlib import contextmanager
from timeit import timeit

from data import WeightData


@contextmanager
def suppress_stdout():
    with open(os.devnull, "w") as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout


def test_utils(force_download):
    from utils import url_open

    url = "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv"
    with suppress_stdout():
        url_open(url, force_download=force_download)


def test_data():
    from data import WeightData

    with suppress_stdout():
        data = WeightData()
        data.filter("2020-01-01")
        data["date"]
        data.last(30)
        data.days(30)


def test_pyplot(data, days):
    from plot import plt, with_pyplot

    with suppress_stdout():
        with_pyplot(data, days)
        plt.close("all")


print(f"utils force download: {timeit(lambda: test_utils(True), number=3)}")
print(f"utils no download: {timeit(lambda: test_utils(False), number=3)}")
print(f"data: {timeit(lambda: test_data(), number=10)}")

day_count = 90
with suppress_stdout():
    data = WeightData()
print(f"pyplot: {timeit(lambda: test_pyplot(data, day_count),number=10)}")
