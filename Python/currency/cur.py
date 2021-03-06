#!/usr/bin/env python3
"""
Support script for currency conversion app,
builds master reference table for offline testing
"""
import requests
from datetime import datetime
from csv import DictWriter
from dateutil.parser import parse
from pathlib import Path


def writenow():
    """Return utcnow() as ISO foramat string"""
    return datetime.isoformat(datetime.utcnow())


def check(file="master.tsv", delay=1):
    """Check master file expiry"""
    if not Path(file).exists():
        return True
    with open(file, "r") as f:
        line = next(f)
        _, time = line.split('\t')
        dt = parse(time)
        return datetime.utcnow().hour > dt.hour + delay


def marshal(data, file="master.tsv"):
    """Write time/base headed table of currency conversions"""
    with open(file, "w+") as f:
        f.write(f"{data['base']}\t{writenow()}\n")
        rates = data["rates"]
        writer = DictWriter(f, ["country", "rate"], delimiter='\t')
        for k, v in rates.items():
            writer.writerow({"country": k, "rate": v})


if __name__ == "__main__":
    if check():  # throttle requests
        stuff = requests.get("https://api.exchangeratesapi.io/latest")
        data = stuff.json()
        marshal(data)
        print(data)
