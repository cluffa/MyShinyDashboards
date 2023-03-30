import matplotlib.dates as mdates
import matplotlib.pyplot as plt


def with_pyplot(weight_data, days=90, fit=True, smooth=0.5):
    data = weight_data.days(days)
    fig, ax = plt.subplots()

    formats = {
        7: ("%b %d", mdates.DayLocator(interval=1)),
        30: ("%b %d", mdates.DayLocator(interval=7)),
        90: ("%b %d", mdates.DayLocator(bymonthday=(1, 15))),
        180: ("%b", mdates.MonthLocator(interval=1)),
        365: ("%b %Y", mdates.MonthLocator(interval=2)),
    }

    for max_days in formats:
        if days <= max_days:
            fmt, locator = formats[max_days]
            ax.axes.xaxis.set_major_locator(locator)
            ax.axes.xaxis.set_major_formatter(mdates.DateFormatter(fmt))
            break

    ax.set_title(f"Weight over the last {days} days")
    ax.set_xlabel("Date")
    ax.set_ylabel("Weight (lbs)")
    ax.grid(True, zorder=0)
    ax.scatter(
        data["date"],
        data["weight"],
        zorder=10,
        s=10,
    )

    if fit:
        xs, ys = data.spline_fit(points_out=days * 2, smooth=smooth)
        ax.plot(xs, ys, color="red", linestyle="--", zorder=20, linewidth=2)

    fig.set_dpi(300)

    return fig
    return fig
