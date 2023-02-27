# tests and benchmarks
import os
import sys
from contextlib import contextmanager
from timeit import timeit


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


def test_plot(data, days):
    from plot import plt, with_plotnine, with_pyplot

    with suppress_stdout():
        with_pyplot(data, days)
        with_plotnine(data, days)
        plt.close("all")


def test_pyplot(data, days):
    from plot import plt, with_pyplot

    with suppress_stdout():
        with_pyplot(data, days)
        plt.close("all")


def test_plotnine(data, days, fit=False):
    from plot import plt, with_plotnine

    with suppress_stdout():
        with_plotnine(data, days, fit=fit)
        plt.close("all")


print(f"utils force download: {timeit(lambda: test_utils(True),number=3)}")
print(f"utils no download: {timeit(lambda: test_utils(False),number=10)}")
print(f"data: {timeit(lambda: test_data(),number=100)}")

from data import WeightData

with suppress_stdout():
    data = WeightData()
print(f"plot: {timeit(lambda: test_plot(data, 365),number=100)}")
print(f"pyplot: {timeit(lambda: test_pyplot(data, 365),number=100)}")
print(f"plotnine: {timeit(lambda: test_plotnine(data, 365, fit=False),number=100)}")
print(
    f"plotnine (with smoothing line): {timeit(lambda: test_plotnine(data, 365, fit=True),number=100)}"
)
