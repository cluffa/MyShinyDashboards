import os
import sys


# return a list of lines from a url
# if running in pyodide, use pyodide.open_url
def url_open(url, force_download=False):
    # Read in the weights from Google Sheets
    if "pyodide" in sys.modules:
        # If running in pyodide, use pyodide.open_url
        print("Running in pyodide, using pyodide.open_url")
        import pyodide  # type: ignore

        lines = pyodide.http.open_url(url).readlines()
    else:
        # Otherwise, use urllib.request.urlopen
        from urllib.request import urlopen

        if os.path.exists("weight.csv") and not force_download:
            # If weight.csv exists, use that
            print("Using local copy of weight.csv")
            file = open("weight.csv", "r")
            lines = file.readlines()
        else:
            # Otherwise, download from Google Sheets
            print("Downloading weight.csv from Google Sheets")
            lines = [line.decode("utf-8").strip() + "\n" for line in urlopen(url)]
            file = open("weight.csv", "w")
            for line in lines:
                file.write(line)
            file.close()

    print(f"Read {len(lines)} lines")
    return lines
