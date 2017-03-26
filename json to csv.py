import unicodecsv as csv
import json


def json_to_csv(filename):
    with open(filename) as file:
        data = json.load(file)
    csvname = filename.split('.')[0] + ".csv"
    with open(csvname, "w") as file:
        csv_file = csv.writer(file)
        csv_file.writerow(data[0].keys())
        for item in data:
            csv_file.writerow(item.values())

json_to_csv("")
