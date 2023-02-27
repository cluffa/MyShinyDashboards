import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from plotnine import (
    aes,
    geom_point,
    geom_smooth,
    ggplot,
    labs,
    scale_x_date,
    theme_linedraw,
)


def with_pyplot(weight_data, day_count=90, fit=True):
    data = weight_data.days(day_count)
    fig, ax = plt.subplots()

    formats = {
        7: ("%b %d", mdates.DayLocator(interval=1)),
        30: ("%b %d", mdates.DayLocator(interval=7)),
        90: ("%b %d", mdates.DayLocator(bymonthday=(1, 15))),
        180: ("%b", mdates.MonthLocator(interval=1)),
        365: ("%b %Y", mdates.MonthLocator(interval=2)),
    }

    for max_days in formats:
        if day_count <= max_days:
            fmt, locator = formats[max_days]
            ax.axes.xaxis.set_major_locator(locator)
            ax.axes.xaxis.set_major_formatter(mdates.DateFormatter(fmt))
            break

    ax.set_title(f"Weight over the last {day_count} days (using pyplot/matplotlib))")
    ax.set_xlabel("Date")
    ax.set_ylabel("Weight (lbs)")
    ax.grid(True, zorder=0)
    ax.scatter(
        data["date"],
        data["weight"],
        zorder=10,
        s=10,
    )

    fig.set_dpi(300)

    return fig


def with_plotnine(weight_data, day_count=90, fit=True):
    data = weight_data.days(day_count)
    plot = (
        ggplot(mapping=aes(x=data["date"], y=data["weight"]))
        + labs(
            title=f"Weight over the last {day_count} days (using plotnine)",
            x="Date",
            y="Weight (lbs)",
        )
        + geom_point()
        + scale_x_date(
            date_breaks="1 month",
        )
        + theme_linedraw()
    )

    if fit:
        plot += geom_smooth(
            span=0.1,
            se=False,
            color="red",
            linetype="dashed",
        )

    return plot.draw()
