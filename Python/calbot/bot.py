#!./bin/python3
import datetime
import pickle
import os.path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request


import sys
from parser import parse
import logging
from functools import wraps

logging.basicConfig(filename="main.log", filemode="w+")

SCOPES = ["https://www.googleapis.com/auth/calendar.readonly",
          "https://www.googleapis.com/auth/calendar.events"]


def exemptHandler(func):
    """Pipes exceptions through main logger"""
    name = func.__name__

    @wraps(func)
    def deco(*args):
        try:
            result = func(*args)
        except Exception as e:
            logging.exception("%s:\t%s" % (name, e))
            sys.exit(1)
        return result
    return deco


@exemptHandler
def connect():
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json', SCOPES)
            creds = flow.run_local_server()
        # Save the credentials for the next run
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)
    return build('calendar', 'v3', credentials=creds)


"""
@exemptHandler
def connect():
    store = file.Storage('token.json')
    creds = store.get()
    if not creds or creds.invalid:
        flow = client.flow_from_clientsecrets('credentials.json', SCOPES)
        creds = tools.run_flow(flow, store)  # fails
    logging.debug("Connection success")
    return build('calendar', 'v3', http=creds.authorize(Http()))
"""


@exemptHandler
def getCal(service, name):
    """Get calendar ID by name."""
    cals = service.calendarList().list(showHidden=True).execute()
    for entry in cals['items']:
        if entry['summary'] == name:
            return entry['id']
    print("Calendar not found")
    sys.exit(1)


@exemptHandler
def printCals(service):
    """Enumerate available calendars."""
    print("Calendars:")
    cals = service.calendarList().list(showHidden=True).execute()
    for entry in cals['items']:
        print("\t%s" % entry['summary'])


@exemptHandler
def is_recurring(event):
    """True if recurring event, else False or error.
    """
    try:
        event['recurrence']
        return True
    except KeyError:
        return False


@exemptHandler
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


@exemptHandler
def getEvents(service, cal, data):
    """Get recurring instances."""
    events = set()
    for start, end in data:
        response = service.events().list(
            calendarId=cal,
            timeMin=start, timeMax=end).execute()
        for event in response['items']:
            if is_recurring(event):
                events |= expand(service, cal, event['id'], (start, end))
    return events


@exemptHandler
def delEvents(service, cal, events):
    """Delete events and return number of deletions
    """
    for event in events:
        service.events().delete(calendarId=cal, eventId=event).execute()
    return len(events)


if __name__ == "__main__":
    service = connect()
    if len(sys.argv) < 3:
        if len(sys.argv) == 1:
            print("Descriptor file needed.")
            sys.exit(1)
        print("Final arg should be calendar.")
        printCals(service)
    else:  # Run deletions
        data = parse(sys.argv[1])
        calID = getCal(service, sys.argv[2])
        expanded = getEvents(service, calID, data)
        delEvents(service, calID, expanded)
        print("%d Events deleted from '%s'" % (len(expanded), sys.argv[2]))
