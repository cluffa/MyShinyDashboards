from data import WeightData
from shiny import App, reactive, render, ui

app_ui = ui.page_fluid(
    ui.h1("Weight Loss Tracker"),
    ui.input_radio_buttons(
        "n",
        "Number of days",
        ["7", "30", "90", "180", "365"],
        selected="90",
        inline=True,
    ),
    ui.output_text_verbatim("data"),
    ui.input_slider("smooth", "Smooth", 0, 1, 0.3, step=0.01),
    ui.output_plot("plot1", width="90%", height="600px"),
)


def server(input, output, session):
    weight_data = WeightData()

    @reactive.Calc
    def day_count() -> int:
        return int(input.n())

    @output
    @render.plot
    def plot1():
        weight_data.plot(days=day_count(), smooth=input.smooth())

    @output
    @render.text()
    def data():
        data = weight_data.days(day_count())
        return str(data)


app = App(app_ui, server)
