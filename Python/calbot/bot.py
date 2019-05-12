#!./bin/python3
import datetime
import pickle
from pathlib import Path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request


import sys
from parser import parse
import logging
from functools import wraps

SCOPES = ["https://www.googleapis.com/auth/calendar.readonly",
          "https://www.googleapis.com/auth/calendar.events"]
logging.basicConfig(filename="main.log", filemode="w+")


def logged(func):
    """Pipes exceptions through root logger"""
    @wraps(func)
    def deco(*args, **kwargs):
        try:
            result = func(*args, **kwargs)
        except Exception as e:
            logging.exception(f"{func.__name__}:\n{e}")
            sys.exit(1)
        return result
    return deco


@logged
def connect():
    """Authenticate with Google/user, connect to API"""
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    tokpath = Path(__file__).resolve().parent / 'token.pickle'
    credpath = Path(__file__).resolve().parent / 'credentials.json'
    if tokpath.exists():
        with open(tokpath, 'rb') as token:
            creds = pickle.load(token)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                credpath, scopes=SCOPES)
            creds = flow.run_local_server()
        # Save the credentials for the next run
        with open(tokpath, 'wb') as token:
            pickle.dump(creds, token)
    return build('calendar', 'v3', credentials=creds, cache_discovery=False)


@logged
def getCal(service, name):
    """Get calendar ID by name."""
    cals = service.calendarList().list(
        showHidden=True, minAccessRole="writer").execute()
    for entry in cals['items']:
        if entry['summary'] == name:
            return entry['id']
    print("Calendar not found")
    sys.exit(1)


@logged
def printCals(service):
    """Enumerate available calendars."""
    print("Calendars:")
    cals = service.calendarList().list(
        showHidden=True, minAccessRole="writer").execute()
    for entry in cals['items']:
        print(f"\t{entry['summary']}")


@logged
def expand(service, cal, event, holiday):
    """Expand recurring events to instances in data.
    """
    events = set()
    start, end = holiday
    response = service.events().instances(
        timeMin=start, timeMax=end,
        calendarId=cal, eventId=event).execute()
    for r in response['items']:
        events.add(r['id'])
    return events


@logged
def getEvents(service, cal, data):
    """Get recurring instances."""
    events = set()
    for start, end in data:
        response = service.events().list(
            calendarId=cal,
            timeMin=start, timeMax=end).execute()
        for event in response['items']:
            if 'recurrence' in event:
                events |= expand(service, cal, event['id'], (start, end))
    return events


@logged
def delEvents(service, cal, events):
    """Delete events and return number of deletions
    """
    for event in events:
        service.events().delete(calendarId=cal, eventId=event).execute()
    return len(events)


if __name__ == "__main__":
    service = connect()

    import argparse
    parser = argparse.ArgumentParser(description="Delete reccuring events")
    parser.add_argument("descriptor", argparse.FileType(mode='r'), metavar="Descriptor file",
                        help="TSV of event ranges to be cleared")
    parser.add_argument("target", metavar="Target calendar", nargs='?',
                        help="Calendar to be cleared", default=None)
    args = parser.parse_args()

    if args.target == None:
        print("Final arg should be calendar.")
        printCals(service)
        sys.exit(1)

    events_file = args.descriptor
    target_calendar = args.target
    data = parse(events_file)
    calID = getCal(service, target_calendar)
    expanded = getEvents(service, calID, data)
    delEvents(service, calID, expanded)
    print(f"{len(expanded)} Events deleted from '{target_calendar}'")
